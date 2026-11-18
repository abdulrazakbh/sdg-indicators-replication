# 02_extract_covariates.R — predictors and income classification
#
# Inputs you download by hand (place in data/raw/):
#   OGHIST.xlsx  World Bank historical income classifications
#                https://datahelpdesk.worldbank.org  -> "World Bank country and lending groups"
#                -> "Historical classification by income" (file OGHIST.xlsx)
#   HDR*_Composite_indices_complete_time_series.csv
#                UNDP Human Development Report data centre, composite time series
#                (supplies female mean years of schooling, columns mys_f_YYYY)
# Downloaded automatically: WDI series in CFG$wdi
#
# Output: data/processed/covariates.csv, data/processed/income_class.csv
source("config/config.R"); source("R/utils.R")
suppressPackageStartupMessages({ library(WDI); library(readxl) })
dir_create(DATA_DIR, "processed")

# --- 1. WDI covariates --------------------------------------------------------
msg("Downloading WDI series: %s", paste(CFG$wdi, collapse = ", "))
wdi <- WDI(country = "all", indicator = CFG$wdi, start = min(CFG$years) - 5,
           end = max(CFG$years), extra = TRUE)
wdi <- wdi %>%
  filter(region != "Aggregates", !is.na(iso3c)) %>%
  transmute(iso3 = iso3c, year, !!!rlang::syms(names(CFG$wdi)), wdi_income_now = income)
for (v in CFG$log_vars) wdi[[v]] <- ifelse(wdi[[v]] > 0, log(wdi[[v]]), NA_real_)

# --- 2. Female mean years of schooling (UNDP HDR) ----------------------------
hdr_file <- list.files(file.path(DATA_DIR, "raw"), pattern = "^HDR.*time_series.*\\.csv$",
                       full.names = TRUE)[1]
if (!is.na(hdr_file)) {
  hdr <- read_csv(hdr_file, show_col_types = FALSE)
  mys <- hdr %>%
    select(iso3, matches("^mys_f_\\d{4}$")) %>%
    pivot_longer(-iso3, names_to = "year", values_to = "mys_f",
                 names_transform = list(year = ~ as.integer(sub("mys_f_", "", .x))))
  wdi <- left_join(wdi, mys, by = c("iso3", "year"))
  msg("Female schooling merged from %s", basename(hdr_file))
} else {
  wdi$mys_f <- NA_real_
  warning("No HDR composite time-series CSV in data/raw/. Female schooling will be ",
          "unavailable and is dropped from the sanitation and water predictor sets.")
}
write_csv(wdi %>% select(-wdi_income_now), file.path(DATA_DIR, "processed", "covariates.csv"))

# --- 3. Historical income classification -------------------------------------
read_oghist <- function(path) {
  sheet <- grep("Country Analytical History", excel_sheets(path), value = TRUE)[1]
  raw <- read_excel(path, sheet = sheet, col_names = FALSE, .name_repair = "minimal")
  raw <- as.data.frame(lapply(raw, as.character), stringsAsFactors = FALSE)
  # header row = the row with the most four-digit years
  yr_hits <- apply(raw, 1, function(r) sum(grepl("^(19|20)\\d{2}(\\.0)?$", r)))
  hdr_row <- which.max(yr_hits)
  years   <- suppressWarnings(as.integer(sub("\\.0$", "", raw[hdr_row, ])))
  code_col <- which.max(colSums(apply(raw, 2, function(c) grepl("^[A-Z]{3}$", c))))
  body <- raw[(hdr_row + 1):nrow(raw), , drop = FALSE]
  body <- body[grepl("^[A-Z]{3}$", body[[code_col]]), , drop = FALSE]
  yr_cols <- which(!is.na(years))
  out <- data.frame(iso3 = rep(body[[code_col]], times = length(yr_cols)),
                    year = rep(years[yr_cols], each = nrow(body)),
                    income = unlist(body[yr_cols], use.names = FALSE))
  out$income <- toupper(gsub("[^A-Za-z]", "", out$income))   # "LM*" -> "LM", ".." -> ""
  out$income[!out$income %in% c("L", "LM", "UM", "H")] <- NA
  out[!is.na(out$income), ]
}

og_path <- file.path(DATA_DIR, "raw", "OGHIST.xlsx")
if (file.exists(og_path)) {
  inc <- read_oghist(og_path)
  msg("Income history: %d countries, %d-%d", n_distinct(inc$iso3), min(inc$year), max(inc$year))
  inc$source <- "OGHIST"
} else {
  warning("OGHIST.xlsx not found. Falling back to TODAY's WDI income group for every year. ",
          "This misclassifies countries that changed group; do not use for final estimates.")
  map_now <- c("Low income" = "L", "Lower middle income" = "LM",
               "Upper middle income" = "UM", "High income" = "H")
  inc <- wdi %>% distinct(iso3, wdi_income_now) %>%
    mutate(income = unname(map_now[wdi_income_now])) %>%
    filter(!is.na(income)) %>%
    tidyr::crossing(year = CFG$years) %>%
    transmute(iso3, year, income, source = "WDI_current_FALLBACK")
}
write_csv(inc, file.path(DATA_DIR, "processed", "income_class.csv"))
msg("Done.")
