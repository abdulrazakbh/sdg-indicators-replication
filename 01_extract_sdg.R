# 01_extract_sdg.R — download outcome series from the UN Global SDG Database
# Output: data/raw/sdg_<SERIES>.csv, data/processed/sdg_long.csv,
#         data/raw/extraction_log.csv
source("config/config.R"); source("R/utils.R")
suppressPackageStartupMessages({ library(httr); library(jsonlite); library(countrycode) })

API <- "https://unstats.un.org/SDGAPI/v1/sdg/Series/Data"
PAGE_SIZE <- 5000
`%||%` <- function(a, b) if (is.null(a)) b else a

fetch_series <- function(code) {
  page <- 1; recs <- list(); total_pages <- 1
  repeat {
    resp <- RETRY("GET", API, query = list(seriesCode = code, page = page, pageSize = PAGE_SIZE),
                  times = 4, pause_base = 2, timeout(120))
    stop_for_status(resp)
    js <- fromJSON(content(resp, "text", encoding = "UTF-8"), simplifyVector = FALSE)
    if (!is.null(js$pageNumber) && js$pageNumber != page)
      stop("API returned page ", js$pageNumber, " for request ", page, "; check paging parameters")
    recs <- c(recs, js$data)
    total_pages <- js$totalPages %||% 1
    msg("%s: page %d/%d (%d records so far)", code, page, total_pages, length(recs))
    if (page >= total_pages) break
    page <- page + 1
  }
  if (!is.null(js$totalElements) && length(recs) != js$totalElements)
    warning(code, ": fetched ", length(recs), " records but API reports ", js$totalElements)

  map_dfr(recs, function(r) {
    dims <- r$dimensions; attrs <- r$attributes
    tibble(series      = r$series,
           m49         = as.integer(r$geoAreaCode),
           area_name   = r$geoAreaName,
           year        = as.integer(r$timePeriodStart %||% NA),
           value       = suppressWarnings(as.numeric(r$value %||% NA)),
           location    = dims$Location %||% NA_character_,
           reporting   = dims$`Reporting Type` %||% NA_character_,
           nature      = attrs$Nature %||% NA_character_,
           units       = attrs$Units %||% NA_character_,
           source      = r$source %||% NA_character_)
  })
}


dir_create(DATA_DIR, "raw"); dir_create(DATA_DIR, "processed")
log_rows <- list(); all_series <- list()

for (code in CFG$series) {
  raw <- fetch_series(code)
  write_csv(raw, file.path(DATA_DIR, "raw", paste0("sdg_", code, ".csv")))

  clean <- raw %>%
    filter(is.na(reporting) | reporting == "G") %>%               # global (harmonised) figures
    mutate(iso3 = suppressWarnings(countrycode(m49, "un", "iso3c"))) %>%
    filter(!is.na(iso3), !is.na(value))                           # drops regional aggregates

  dups <- clean %>% count(iso3, year, location) %>% filter(n > 1)
  if (nrow(dups)) {
    warning(code, ": ", nrow(dups), " duplicated country-year-location cells; keeping the first")
    clean <- clean %>% distinct(iso3, year, location, .keep_all = TRUE)
  }
  all_series[[code]] <- clean %>% select(series, iso3, area_name, year, location, value, nature)

  log_rows[[code]] <- tibble(series = code, extracted_utc = format(Sys.time(), tz = "UTC"),
                             n_raw = nrow(raw), n_country_obs = nrow(clean),
                             n_countries = n_distinct(clean$iso3),
                             year_min = min(clean$year), year_max = max(clean$year),
                             source_release = paste(unique(raw$source), collapse = " | "))
}

write_csv(bind_rows(all_series), file.path(DATA_DIR, "processed", "sdg_long.csv"))
write_csv(bind_rows(log_rows), file.path(DATA_DIR, "raw", "extraction_log.csv"))
print(bind_rows(log_rows) %>% select(series, n_countries, year_min, year_max))
msg("Done. Record the source_release column in the paper's data statement.")
