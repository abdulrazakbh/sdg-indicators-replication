# 00_install.R — run once on a new machine
# augsynth and synthdid are not on CRAN; they install from GitHub.

cran <- c("Synth", "dplyr", "tidyr", "readr", "purrr", "jsonlite", "httr",
          "ggplot2", "countrycode", "WDI", "readxl", "remotes", "scales")
missing <- setdiff(cran, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")

if (!requireNamespace("augsynth", quietly = TRUE))
  remotes::install_github("ebenmichael/augsynth", upgrade = "never")
if (!requireNamespace("synthdid", quietly = TRUE))
  remotes::install_github("synth-inference/synthdid", upgrade = "never")

for (p in c(cran, "augsynth", "synthdid"))
  cat(sprintf("%-12s %s\n", p, if (requireNamespace(p, quietly = TRUE)) "ok" else "MISSING"))

# Record versions for the replication package
writeLines(capture.output(sessionInfo()), "session_info.txt")
