# R/fit.R — estimation and inference (Sections 3.5-3.6)
suppressPackageStartupMessages({ library(Synth) })

# ---- Standard synthetic control (Abadie et al., 2010) ------------------------
# panel: columns iso3, year, y, predictors; one row per unit-year
synth_fit <- function(panel, treated, donors, pre, win, pv, quiet = TRUE) {
  # Drop donors with no value at all for some predictor inside THIS pre-window.
  # The donor-pool rules check the full pre-period; in-time placebos and
  # backdating use shorter windows, where a donor with patchy early data can
  # have nothing left for a predictor.
  avail <- panel %>% filter(iso3 %in% c(treated, donors), year %in% pre) %>%
    group_by(iso3) %>% summarise(across(all_of(pv), ~ any(!is.na(.x))), .groups = "drop")
  ok <- avail$iso3[apply(as.matrix(avail[pv]), 1, all)]
  if (!treated %in% ok) return(NULL)
  dropped <- setdiff(donors, ok)
  donors <- intersect(donors, ok)
  if (length(donors) < 2) return(NULL)

  d <- as.data.frame(panel %>% filter(iso3 %in% c(treated, donors), year %in% win))
  d$unit_id <- as.integer(factor(d$iso3))
  d$iso3 <- as.character(d$iso3)
  ids <- distinct(d, iso3, unit_id)
  lags <- unique(c(min(pre), pre[ceiling(length(pre) / 2)], max(pre)))
  dp <- NULL
  # dataprep prints a "Missing data" note for every gap it averages over; silence it
  invisible(capture.output(dp <- tryCatch(dataprep(
    foo = d, predictors = pv, predictors.op = "mean", time.predictors.prior = pre,
    special.predictors = lapply(lags, function(t) list("y", t, "mean")),
    dependent = "y", unit.variable = "unit_id", unit.names.variable = "iso3",
    time.variable = "year",
    treatment.identifier = ids$unit_id[ids$iso3 == treated],
    controls.identifier  = ids$unit_id[ids$iso3 %in% donors],
    time.optimize.ssr = pre, time.plot = win),
    error = function(e) { warning("dataprep failed: ", conditionMessage(e), call. = FALSE); NULL })))
  if (is.null(dp)) return(NULL)
  run <- function(...) {
    if (quiet) { sink(tempfile()); on.exit(sink()) }
    synth(data.prep.obj = dp, ...)
  }
  # Synth's ipop solver occasionally fails with "infinite or missing values in 'x'"
  # for particular donor sets. Try progressively different settings and record
  # which one worked, so the paper can report it.
  attempts <- list(default = list(), margin_1e3 = list(Margin.ipop = 1e-3),
                   sigf_4 = list(Sigf.ipop = 4), nlminb = list(optimxmethod = "nlminb"))
  so <- NULL; solver <- NA_character_
  for (nm in names(attempts)) {
    so <- tryCatch(do.call(run, attempts[[nm]]), error = function(e) NULL)
    if (!is.null(so)) { solver <- nm; break }
  }
  if (is.null(so)) {
    warning(sprintf("Synth failed for treated=%s with %d donors under all solver settings",
                    treated, length(donors)), call. = FALSE)
    return(NULL)
  }

  w <- as.numeric(so$solution.w)
  donor_iso <- ids$iso3[match(as.integer(rownames(so$solution.w)), ids$unit_id)]
  synth_y <- as.numeric(dp$Y0plot %*% w)
  obs_y   <- as.numeric(dp$Y1plot)
  gap <- tibble(year = win, observed = obs_y, synthetic = synth_y, gap = obs_y - synth_y)
  tab <- tryCatch({
    if (quiet) { sink(tempfile()); on.exit(sink(), add = TRUE) }
    synth.tab(dataprep.res = dp, synth.res = so)$tab.pred
  }, error = function(e) NULL)
  list(weights = tibble(iso3 = donor_iso, weight = w) %>% arrange(desc(weight)),
       gap = gap, balance = tab, solver = solver, loss_v = as.numeric(so$loss.v),
       dropped_donors = dropped,
       pre_rmspe  = rmspe(gap$gap[gap$year %in% pre]),
       post_rmspe = rmspe(gap$gap[!gap$year %in% pre]))
}

# ---- Convex hull diagnostic (Section 3.3) ------------------------------------
hull_check <- function(panel, treated, donors, pre) {
  panel %>% filter(year %in% pre) %>% group_by(year) %>%
    summarise(treated = y[iso3 == treated],
              donor_min = min(y[iso3 %in% donors]), donor_max = max(y[iso3 %in% donors]),
              .groups = "drop") %>%
    mutate(outside = treated < donor_min | treated > donor_max)
}

# ---- In-space placebos (Section 3.6) -----------------------------------------
placebo_space <- function(panel, treated, donors, pre, win, pv, main) {
  res <- map_dfr(donors, function(u) {
    f <- synth_fit(panel, u, setdiff(donors, u), pre, win, pv)
    if (is.null(f)) return(NULL)
    f$gap %>% mutate(unit = u, pre_rmspe = f$pre_rmspe, post_rmspe = f$post_rmspe, solver = f$solver)
  })
  failed <- setdiff(donors, unique(res$unit))
  if (length(failed)) warning("Placebo fits failed for: ", paste(failed, collapse = ", "), call. = FALSE)
  out <- bind_rows(main$gap %>% mutate(unit = treated, pre_rmspe = main$pre_rmspe,
                                       post_rmspe = main$post_rmspe, solver = main$solver), res)
  attr(out, "failed") <- failed
  out
}

placebo_pvalues <- function(pl, treated, cut) {
  r <- pl %>% distinct(unit, pre_rmspe, post_rmspe) %>% mutate(ratio = post_rmspe / pre_rmspe)
  tr <- r %>% filter(unit == treated)
  full <- mean(r$ratio >= tr$ratio)
  rc <- r %>% filter(pre_rmspe <= cut * tr$pre_rmspe)
  tibble(set = c("all placebos", sprintf("pre-RMSPE <= %gx treated", cut)),
         n_units = c(nrow(r), nrow(rc)),
         treated_rank = c(sum(r$ratio >= tr$ratio), sum(rc$ratio >= tr$ratio)),
         p_value = c(full, mean(rc$ratio >= tr$ratio)))
}

# ---- Augmented SCM with conformal intervals (Ben-Michael et al., 2021) -------
ascm_fit <- function(panel, treated, donors, pre, win, pv) {
  if (!requireNamespace("augsynth", quietly = TRUE)) return(NULL)
  d <- panel %>% filter(iso3 %in% c(treated, donors), year %in% win) %>% as.data.frame()
  # time-invariant predictors = pre-treatment means (same as the Synth predictors)
  means <- d %>% filter(year %in% pre) %>% group_by(iso3) %>%
    summarise(across(all_of(pv), ~ mean(.x, na.rm = TRUE), .names = "{.col}_m"), .groups = "drop")
  d <- left_join(d, means, by = "iso3")
  form <- as.formula(paste("y ~ treat |", paste0(pv, "_m", collapse = " + ")))
  fit <- tryCatch(augsynth::augsynth(form, iso3, year, d, progfunc = "Ridge", scm = TRUE),
                  error = function(e) { warning("augsynth with covariates failed: ", conditionMessage(e),
                                                "; refitting on outcomes only"); NULL })
  if (is.null(fit))
    fit <- tryCatch(augsynth::augsynth(y ~ treat, iso3, year, d, progfunc = "Ridge", scm = TRUE),
                    error = function(e) { warning("augsynth failed: ", conditionMessage(e)); NULL })
  if (is.null(fit)) return(NULL)
  s <- summary(fit, inf_type = "conformal")
  att <- as_tibble(s$att) %>% rename(year = Time)
  list(fit = fit, att = att, avg_att = s$average_att, l2_imbalance = s$l2_imbalance,
       scaled_l2 = s$scaled_l2_imbalance)
}

# ---- Synthetic difference-in-differences (Arkhangelsky et al., 2021) ---------
sdid_fit <- function(panel, treated, donors, win) {
  if (!requireNamespace("synthdid", quietly = TRUE)) return(NULL)
  d <- panel %>% filter(iso3 %in% c(treated, donors), year %in% win) %>%
    select(iso3, year, y, treat) %>% as.data.frame()
  pm <- synthdid::panel.matrices(d, unit = "iso3", time = "year", outcome = "y", treatment = "treat")
  est <- list(SDID = synthdid::synthdid_estimate(pm$Y, pm$N0, pm$T0),
              SC   = synthdid::sc_estimate(pm$Y, pm$N0, pm$T0),
              DID  = synthdid::did_estimate(pm$Y, pm$N0, pm$T0))
  map_dfr(names(est), function(k) {
    se <- tryCatch(sqrt(vcov(est[[k]], method = "placebo")), error = function(e) NA_real_)
    tibble(estimator = paste0("synthdid::", k), estimate = as.numeric(est[[k]]), se = as.numeric(se))
  })
}
