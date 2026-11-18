# R/utils.R — shared helpers
suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(readr); library(purrr)
})

dir_create <- function(...) { p <- file.path(...); dir.create(p, recursive = TRUE, showWarnings = FALSE); p }

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), sprintf(...), "\n", sep = "")

# Longest run of consecutive NAs in a vector
max_na_run <- function(x) {
  r <- rle(is.na(x))
  if (!any(r$values)) 0L else max(r$lengths[r$values])
}

# Linear interpolation of interior gaps up to `maxgap` years; ends stay NA
interp_interior <- function(y, x, maxgap) {
  if (sum(!is.na(y)) < 2) return(y)
  out <- y
  obs <- which(!is.na(y))
  for (k in seq_len(length(obs) - 1)) {
    a <- obs[k]; b <- obs[k + 1]
    gap <- b - a - 1
    if (gap > 0 && gap <= maxgap) {
      idx <- (a + 1):(b - 1)
      out[idx] <- y[a] + (y[b] - y[a]) * (x[idx] - x[a]) / (x[b] - x[a])
    }
  }
  out
}

rmspe <- function(g) sqrt(mean(g^2, na.rm = TRUE))

# Read the event definition and attach derived years
get_event <- function(id) {
  ev <- CFG$events[[id]]
  if (is.null(ev)) stop("Unknown event: ", id)
  ev$id        <- id
  ev$pre_years <- CFG$years[CFG$years <  ev$first_treated]
  ev$post_years <- CFG$years[CFG$years >= ev$first_treated]
  ev
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
