# tests/make_fake_data.R — SIMULATED panel for testing the pipeline offline.
# Never use these numbers in the paper. India gets a built-in effect so you can
# check the pipeline recovers it: open defecation -12 pp by 2019 after 2015.
set.seed(42)
suppressPackageStartupMessages({ library(dplyr); library(tidyr); library(readr); library(countrycode) })
out <- "tests/fake_data/processed"; dir.create(out, recursive = TRUE, showWarnings = FALSE)
years <- 2000:2024
pool <- c("IND","NPL","BGD","PAK","KEN","RWA","ETH","UGA","TZA","GHA","NGA","SEN","MLI","BFA","NER",
          "TCD","MOZ","MWI","ZMB","ZWE","MDG","HTI","KHM","LAO","MMR","AFG","YEM","SDN","SSD","CMR",
          "CIV","BEN","TGO","GIN","SLE","LBR","MRT","AGO","COD","COG","CAF","BDI","SOM","ERI","LSO",
          "BOL","NIC","HND","PNG","SLB","TLS","VUT","KGZ","TJK","UZB","PHL","IDN","VNM","EGY","MAR")
u <- tibble(iso3 = pool, a = runif(length(pool), 55, 98), r = runif(length(pool), 0.02, 0.09))
u$a[u$iso3 == "IND"] <- 92; u$r[u$iso3 == "IND"] <- 0.035
grid <- tidyr::crossing(u, year = years) %>%
  mutate(t = year - 2000,
         od = a * exp(-r * t) + rnorm(n(), 0, 0.6),
         od = ifelse(iso3 == "IND" & year >= 2015, od - pmin(year - 2014, 4) * 3, od),
         elec = 100 - (100 - (100 - a / 1.3)) * exp(-r * 1.2 * t) + rnorm(n(), 0, 0.8),
         elec = ifelse(iso3 == "IND" & year >= 2018, elec + pmin(year - 2017, 3) * 2.5, elec),
         od = pmin(pmax(od, 0), 100), elec = pmin(pmax(elec, 0), 100))
grid$od[sample(nrow(grid), 40)] <- NA   # scattered gaps to exercise interpolation
sdg <- bind_rows(
  grid %>% transmute(series = "SH_SAN_DEFECT", iso3, area_name = iso3, year, location = "RURAL", value = od, nature = "E"),
  grid %>% transmute(series = "SH_SAN_SAFE", iso3, area_name = iso3, year, location = "RURAL", value = 100 - od * 0.9, nature = "E"),
  grid %>% transmute(series = "EG_ACS_ELEC", iso3, area_name = iso3, year, location = "ALLAREA", value = elec, nature = "E"),
  grid %>% transmute(series = "SH_H2O_SAFE", iso3, area_name = iso3, year, location = "RURAL", value = pmin(100, 100 - od * 0.7), nature = "E"))
sdg <- sdg %>% filter(!(series == "SH_SAN_DEFECT" & iso3 %in% c("IND") & is.na(value)))
write_csv(sdg %>% filter(!is.na(value)), file.path(out, "sdg_long.csv"))
cov <- tidyr::crossing(u, year = 1995:2024) %>%
  transmute(iso3, year, gdp_pc_ppp = log(800 + (100 - a) * 60 + (year - 1995) * 40 + rnorm(n(), 0, 50)),
            urban_pct = 20 + (100 - a) / 3 + (year - 1995) * 0.3, pop_dens = log(runif(n(), 20, 400)),
            oda_pc = runif(n(), 5, 80), mys_f = 1 + (100 - a) / 15 + (year - 1995) * 0.05)
write_csv(cov, file.path(out, "covariates.csv"))
inc <- tidyr::crossing(iso3 = pool, year = years) %>%
  mutate(income = ifelse(iso3 %in% c("EGY","IDN","PHL","VNM","MAR","UZB","BOL"), "LM", "L"),
         income = ifelse(iso3 == "IND", "LM", income), source = "OGHIST")
inc$income[inc$iso3 == "MAR"] <- "UM"      # should be dropped by rule 1
write_csv(inc, file.path(out, "income_class.csv"))
cat("Fake data written to", out, "\n")
