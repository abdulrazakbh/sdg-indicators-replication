# 03_build_donor_pool.R — apply the five donor-pool rules (Section 3.3)
# Usage:  Rscript 03_build_donor_pool.R            # all events
#         Rscript 03_build_donor_pool.R sbm jjm    # selected events
# Output per event: data/processed/panel_<event>.csv
#                   data/processed/donor_log_<event>.csv  (every removal, with rule)
source("config/config.R"); source("R/utils.R")

sdg  <- read_csv(file.path(DATA_DIR, "processed", "sdg_long.csv"), show_col_types = FALSE)
cov  <- read_csv(file.path(DATA_DIR, "processed", "covariates.csv"), show_col_types = FALSE)
inc  <- read_csv(file.path(DATA_DIR, "processed", "income_class.csv"), show_col_types = FALSE)
excl <- read_csv("config/exclusions.csv", show_col_types = FALSE)
if (any(inc$source != "OGHIST")) warning("Income classes use the WDI fallback; see 02_extract_covariates.R")

predictor_vars <- function(ev) {
  v <- names(CFG$wdi)
  if (ev$sector %in% CFG$female_schooling_sectors) v <- c(v, "mys_f")
  v
}

build_event <- function(id) {
  ev <- get_event(id)
  msg("== %s: %s", id, ev$label)
  log <- list()
  drop <- function(iso, rule, detail) {
    if (length(iso)) log[[length(log) + 1]] <<- tibble(iso3 = iso, rule = rule, detail = detail)
  }

  y <- sdg %>% filter(series == ev$outcome, location == ev$location, year %in% CFG$years) %>%
    select(iso3, year, value) %>%
    complete(iso3, year = CFG$years) %>% arrange(iso3, year)
  if (!ev$treated %in% y$iso3) stop(ev$treated, " has no data for ", ev$outcome, "/", ev$location)

  # Analysis window ends at the treated unit's last observed year
  end_year <- max(y$year[y$iso3 == ev$treated & !is.na(y$value)])
  win  <- CFG$years[CFG$years <= end_year]
  pre  <- ev$pre_years
  post <- win[win >= ev$first_treated]
  if (length(post) < 2) stop(id, ": fewer than two post-treatment years of data")
  y <- y %>% filter(year %in% win)
  cand <- setdiff(unique(y$iso3), ev$treated)
  drop(setdiff(unique(sdg$iso3), c(cand, ev$treated)), "0_no_series", "No values for this series/location")

  # Rule 1: income group in T0-1 (nearest earlier classified year if T0-1 missing)
  ref_year <- ev$first_treated - 1
  inc_ref <- inc %>% filter(year <= ref_year) %>% group_by(iso3) %>%
    slice_max(year, n = 1) %>% ungroup()
  ok_inc <- inc_ref$iso3[inc_ref$income %in% CFG$rules$income_groups]
  bad <- setdiff(cand, ok_inc)
  drop(bad, "1_income", paste0("Not in {", paste(CFG$rules$income_groups, collapse = ","),
                               "} in ", ref_year, " (or unclassified)"))
  cand <- setdiff(cand, bad)

  # Rule 2: coverage and interpolation
  y <- y %>% group_by(iso3) %>%
    mutate(y_interp = interp_interior(value, year, CFG$rules$max_gap_interp)) %>% ungroup()
  cov_tab <- y %>% filter(iso3 %in% cand) %>% group_by(iso3) %>%
    summarise(pre_share   = mean(!is.na(value[year %in% pre])),
              pre_complete  = all(!is.na(y_interp[year %in% pre])),
              post_complete = all(!is.na(y_interp[year %in% post])), .groups = "drop")
  bad <- cov_tab$iso3[cov_tab$pre_share < CFG$rules$min_coverage]
  drop(bad, "2_coverage", sprintf("Observed in < %.0f%% of pre-treatment years", 100 * CFG$rules$min_coverage))
  cand <- setdiff(cand, bad)
  bad <- intersect(cand, cov_tab$iso3[!cov_tab$pre_complete | !cov_tab$post_complete])
  drop(bad, "2_gaps", sprintf("Gap longer than %d years or missing at window ends", CFG$rules$max_gap_interp))
  cand <- setdiff(cand, bad)

  tr <- y %>% filter(iso3 == ev$treated)
  if (any(is.na(tr$y_interp[tr$year %in% c(pre, post)])))
    stop("Treated unit ", ev$treated, " has unfillable gaps in ", ev$outcome)

  # Predictors: need at least one pre-treatment value for each variable
  pv <- predictor_vars(ev)
  if ("mys_f" %in% pv && all(is.na(cov$mys_f[cov$iso3 == ev$treated]))) {
    warning(id, ": no female schooling data for treated unit; dropping mys_f")
    pv <- setdiff(pv, "mys_f")
  }
  cov_pre <- cov %>% filter(year %in% pre) %>% group_by(iso3) %>%
    summarise(across(all_of(pv), ~ any(!is.na(.x))), .groups = "drop")
  ok_cov <- cov_pre$iso3[apply(cov_pre[pv], 1, all)]
  if (!ev$treated %in% ok_cov) stop("Treated unit lacks pre-treatment covariates: ", paste(pv, collapse = ", "))
  bad <- setdiff(cand, ok_cov)
  drop(bad, "2_covariates", paste("Missing all pre-treatment values of a predictor:", paste(pv, collapse = ", ")))
  cand <- setdiff(cand, bad)

  # Rules 3 and 4: curated exclusions
  ex <- excl %>% rowwise() %>%
    filter(applies_to == "all" || id %in% strsplit(applies_to, ";")[[1]]) %>% ungroup()
  for (rl in c("concurrent_programme", "shock")) {
    e <- ex %>% filter(rule == rl, iso3 %in% cand)
    num <- if (rl == "concurrent_programme") "3_" else "4_"
    if (nrow(e)) for (k in seq_len(nrow(e)))
      drop(e$iso3[k], paste0(num, rl), paste0(e$reason[k], if (e$status[k] == "verify") " [VERIFY]" else ""))
    cand <- setdiff(cand, e$iso3)
  }

  # Rule 5: ceiling / floor
  last3 <- tail(pre, 3)
  lv <- y %>% filter(iso3 %in% cand, year %in% last3) %>% group_by(iso3) %>%
    summarise(m = mean(y_interp), .groups = "drop")
  mg <- CFG$rules$ceiling_margin
  bad <- lv$iso3[lv$m <= mg | lv$m >= 100 - mg]
  drop(bad, "5_ceiling", sprintf("Mean of %d-%d within %g points of 0 or 100", min(last3), max(last3), mg))
  cand <- setdiff(cand, bad)

  if (length(cand) < 10) warning(id, ": only ", length(cand), " donors remain; inference will be weak")

  panel <- y %>% filter(iso3 %in% c(ev$treated, cand)) %>%
    left_join(cov %>% select(iso3, year, all_of(pv)), by = c("iso3", "year")) %>%
    mutate(treated_unit = iso3 == ev$treated,
           treat = as.integer(treated_unit & year >= ev$first_treated)) %>%
    rename(y_raw = value, y = y_interp)

  dlog <- bind_rows(log)
  write_csv(panel, file.path(DATA_DIR, "processed", paste0("panel_", id, ".csv")))
  write_csv(dlog,  file.path(DATA_DIR, "processed", paste0("donor_log_", id, ".csv")))
  msg("%s: window %d-%d, %d pre / %d post years, %d donors retained",
      id, min(win), end_year, length(pre), length(post), length(cand))
  print(dlog %>% filter(rule != "0_no_series") %>% count(rule))
  invisible(list(panel = panel, log = dlog))
}

args <- commandArgs(trailingOnly = TRUE)
ids  <- if (length(args)) args else names(CFG$events)
for (id in ids) build_event(id)
