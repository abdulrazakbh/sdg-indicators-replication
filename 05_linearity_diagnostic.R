# 05_linearity_diagnostic.R — how are SDG access series built? (Sections 3.2 and 4.1)
# Input:  data/processed/sdg_long.csv  (from 01_extract_sdg.R)
# Output: results/linearity/series_class.csv   one row per series x location x country
#         results/linearity/table3.csv         counts by class (Table 3)
#         results/linearity/bend_years.csv     year of every bend (Section 4.1)
#         results/linearity/nature_codes.csv   database "Nature" flags by series (Section 6.1)
source("config/config.R"); source("R/utils.R")

BEND_TOL   <- 0.05   # a bend = |second difference| above this (percentage points)
MIN_YEARS  <- 10     # consecutive annual values required
MIN_CHANGE <- 1      # series must move more than this over the period

out <- dir_create(RESULTS_DIR, "linearity")
d <- read_csv(file.path(DATA_DIR, "processed", "sdg_long.csv"), show_col_types = FALSE) %>%
  filter(year >= 2000, year <= max(CFG$years))

classify <- function(v) {
  d2 <- diff(diff(v)); k <- sum(abs(d2) > BEND_TOL)
  if (k == 0) "straight line" else if (k <= 3) "segmented" else if (max(abs(d2)) < 1) "smooth curve" else "annual variation"
}

s <- d %>% group_by(series, location, iso3) %>% arrange(year, .by_group = TRUE) %>%
  filter(n() >= MIN_YEARS, all(diff(year) == 1), diff(range(value)) > MIN_CHANGE) %>%
  summarise(n_years = n(), first = min(year), last = max(year),
            bends = sum(abs(diff(diff(value))) > BEND_TOL),
            max_abs_d2 = max(abs(diff(diff(value)))),
            r2_linear = summary(lm(value ~ year))$r.squared,
            class = classify(value), .groups = "drop")
write_csv(s, file.path(out, "series_class.csv"))

lv <- c("straight line", "segmented", "smooth curve", "annual variation")
t3 <- s %>% mutate(class = factor(class, levels = lv)) %>%
  count(series, location, class, .drop = FALSE) %>%
  pivot_wider(names_from = class, values_from = n, values_fill = 0) %>%
  mutate(countries = `straight line` + segmented + `smooth curve` + `annual variation`,
         fitted_pct = round(100 * (`straight line` + segmented) / countries),
         annual_pct = round(100 * `annual variation` / countries)) %>%
  filter(countries > 0) %>% arrange(series, location)
write_csv(t3, file.path(out, "table3.csv"))

bends <- d %>% semi_join(s, by = c("series", "location", "iso3")) %>%
  group_by(series, location, iso3) %>% arrange(year, .by_group = TRUE) %>%
  reframe(year = year[-c(1, n())], d2 = diff(diff(value))) %>%
  filter(abs(d2) > BEND_TOL)
write_csv(bends, file.path(out, "bend_years.csv"))

nat <- d %>% count(series, nature) %>% group_by(series) %>% mutate(pct = round(100 * n / sum(n), 1))
write_csv(nat, file.path(out, "nature_codes.csv"))

print(as.data.frame(t3))
cat("\nIndia:\n"); print(as.data.frame(filter(s, iso3 == "IND")))
cat("\nBends in rural open defecation series by year:\n")
print(table(filter(bends, series == "SH_SAN_DEFECT", location == "RURAL")$year))
cat("\nNature codes (share of values):\n"); print(as.data.frame(nat))
msg("Results written to %s", out)
