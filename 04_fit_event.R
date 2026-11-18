# 04_fit_event.R — estimate one treatment event with full inference
# Usage:  Rscript 04_fit_event.R            # defaults to sbm (Swachh Bharat, open defecation)
#         Rscript 04_fit_event.R saubhagya
#         SKIP_PLACEBOS=1 Rscript 04_fit_event.R sbm   # quick look, no in-space placebos
#
# Output: results/<event>/
#   summary.txt               headline numbers, hull diagnosis, p-values
#   weights.csv balance.csv   donor weights and predictor balance (Synth)
#   gaps.csv                  observed, synthetic, gap by year + ASCM ATT with conformal bounds
#   hull_check.csv            treated vs donor range, pre-treatment
#   placebo_space.csv placebo_pvalues.csv placebo_time.csv backdate.csv loo.csv
#   estimators.csv            SCM vs ASCM vs SDID / SC / DID comparison
#   fig_paths.png fig_gap_placebos.png fig_ratio.png fig_loo.png fig_ascm.png
source("config/config.R"); source("R/utils.R"); source("R/fit.R")
suppressPackageStartupMessages(library(ggplot2))

args <- commandArgs(trailingOnly = TRUE)
id   <- if (length(args)) args[1] else "sbm"
ev   <- get_event(id)
out  <- dir_create(RESULTS_DIR, id)
invisible(file.copy("config/config.R", file.path(out, "config_used.R"), overwrite = TRUE))

panel <- read_csv(file.path(DATA_DIR, "processed", paste0("panel_", id, ".csv")), show_col_types = FALSE)
pv     <- intersect(c(names(CFG$wdi), "mys_f"), names(panel))
win    <- sort(unique(panel$year))
pre    <- win[win <  ev$first_treated]
post   <- win[win >= ev$first_treated]
tr     <- ev$treated
donors <- setdiff(unique(panel$iso3), tr)
msg("%s | treated %s | %d donors | pre %d-%d | post %d-%d | predictors: %s",
    id, tr, length(donors), min(pre), max(pre), min(post), max(post), paste(pv, collapse = ", "))

# 1. Convex hull ---------------------------------------------------------------
hull <- hull_check(panel, tr, donors, pre)
write_csv(hull, file.path(out, "hull_check.csv"))
hull_share <- mean(hull$outside)
hull_flag  <- hull_share > CFG$hull_flag_share
msg("Treated outside donor range in %.0f%% of pre-years%s", 100 * hull_share,
    if (hull_flag) "  -> ASCM should be the primary estimator" else "")

# 2. Main SCM -----------------------------------------------------------------------
main <- synth_fit(panel, tr, donors, pre, win, pv)
if (is.null(main)) stop("Synth optimisation failed for the treated unit")
write_csv(main$weights, file.path(out, "weights.csv"))
if (!is.null(main$balance))
  write_csv(as_tibble(main$balance, rownames = "predictor"), file.path(out, "balance.csv"))
avg_scm <- mean(main$gap$gap[main$gap$year %in% post])
msg("SCM: pre-RMSPE %.3f | mean post gap %.2f pp", main$pre_rmspe, avg_scm)

# 3. In-space placebos ----------------------------------------------------------------
pvals <- NULL; pl <- NULL
if (Sys.getenv("SKIP_PLACEBOS") != "1") {
  msg("In-space placebos: %d refits", length(donors))
  pl <- placebo_space(panel, tr, donors, pre, win, pv, main)
  pvals <- placebo_pvalues(pl, tr, CFG$placebo_rmspe_cut)
  write_csv(pl, file.path(out, "placebo_space.csv"))
  write_csv(pvals, file.path(out, "placebo_pvalues.csv"))
  print(pvals)
}

# 4. In-time placebos (false start dates, data before the real start only) -----------
pt <- map_dfr(CFG$in_time_offsets, function(k) {
  fake <- ev$first_treated - k
  pre_f <- pre[pre < fake]
  if (length(pre_f) < 5) { msg("In-time T0-%d skipped: only %d pre years", k, length(pre_f)); return(NULL) }
  f <- synth_fit(panel, tr, donors, pre_f, pre, pv)
  if (is.null(f)) return(NULL)
  f$gap %>% mutate(offset = k, fake_start = fake, fake_post = year >= fake)
})
if (nrow(pt)) write_csv(pt, file.path(out, "placebo_time.csv"))

# 5. Backdating (anticipation) ---------------------------------------------------------
bd <- map_dfr(CFG$backdate_offsets, function(k) {
  start <- ev$first_treated - k
  f <- synth_fit(panel, tr, donors, pre[pre < start], win, pv)
  if (is.null(f)) return(NULL)
  f$gap %>% mutate(offset = k, backdated_start = start)
})
if (nrow(bd)) write_csv(bd, file.path(out, "backdate.csv"))

# 6. Leave-one-out -----------------------------------------------------------------------
key <- main$weights$iso3[main$weights$weight > CFG$loo_min_weight]
loo <- map_dfr(key, function(u) {
  f <- synth_fit(panel, tr, setdiff(donors, u), pre, win, pv)
  if (is.null(f)) return(NULL)
  f$gap %>% mutate(dropped = u, solver = f$solver)
})
loo_failed <- setdiff(key, unique(loo$dropped))
if (nrow(loo)) write_csv(loo, file.path(out, "loo.csv"))

# 7. Augmented SCM + conformal intervals --------------------------------------------------
asc <- ascm_fit(panel, tr, donors, pre, win, pv)
gaps <- main$gap
if (!is.null(asc)) {
  gaps <- gaps %>% left_join(asc$att %>% select(year, ascm_att = Estimate,
                                                ascm_lower = lower_bound, ascm_upper = upper_bound),
                             by = "year")
}
write_csv(gaps, file.path(out, "gaps.csv"))

# 8. SDID and comparators -------------------------------------------------------------
sd <- tryCatch(sdid_fit(panel, tr, donors, win), error = function(e) { warning(conditionMessage(e)); NULL })
est <- bind_rows(
  tibble(estimator = "Synth (standard SCM)", estimate = avg_scm, se = NA_real_,
         p_value = if (!is.null(pvals)) pvals$p_value[1] else NA_real_,
         note = "p from in-space placebo RMSPE ratio"),
  if (!is.null(asc)) tibble(estimator = "augsynth (ridge ASCM)",
                            estimate = asc$avg_att$Estimate[1], se = NA_real_,
                            p_value = asc$avg_att$p_val[1] %||% NA_real_,
                            note = "p from conformal test of the average effect"),
  if (!is.null(sd)) sd %>% mutate(p_value = NA_real_, note = "se from placebo variance"))
write_csv(est, file.path(out, "estimators.csv"))
print(est)

# 9. Figures --------------------------------------------------------------------------
theme_set(theme_minimal(base_size = 11) + theme(panel.grid.minor = element_blank()))
vline <- geom_vline(xintercept = ev$first_treated - 0.5, linetype = "dashed", colour = "grey40")
ylab  <- paste0(ev$outcome, " (", tolower(ev$location), ", %)")

p1 <- gaps %>% select(year, observed, synthetic) %>%
  pivot_longer(-year, names_to = "series") %>%
  mutate(series = recode(series, observed = tr, synthetic = paste("Synthetic", tr))) %>%
  ggplot(aes(year, value, linetype = series)) + geom_line(linewidth = 0.8) + vline +
  labs(x = NULL, y = ylab, linetype = NULL, title = ev$label) + theme(legend.position = "bottom")
ggsave(file.path(out, "fig_paths.png"), p1, width = 7, height = 4.5, dpi = 300, bg = "white")

if (!is.null(pl)) {
  cut_units <- pl %>% distinct(unit, pre_rmspe) %>%
    filter(pre_rmspe <= CFG$placebo_rmspe_cut * main$pre_rmspe) %>% pull(unit)
  p2 <- ggplot() +
    geom_line(data = filter(pl, unit != tr, unit %in% cut_units), aes(year, gap, group = unit),
              colour = "grey75", linewidth = 0.3) +
    geom_line(data = filter(pl, unit == tr), aes(year, gap), linewidth = 1) +
    geom_hline(yintercept = 0) + vline +
    labs(x = NULL, y = "Gap: observed minus synthetic (pp)",
         title = sprintf("%s vs in-space placebos (pre-RMSPE <= %gx)", tr, CFG$placebo_rmspe_cut))
  ggsave(file.path(out, "fig_gap_placebos.png"), p2, width = 7, height = 4.5, dpi = 300, bg = "white")

  rr <- pl %>% distinct(unit, pre_rmspe, post_rmspe) %>% mutate(ratio = post_rmspe / pre_rmspe) %>%
    arrange(ratio) %>% mutate(unit = factor(unit, levels = unit), is_tr = unit == tr)
  p3 <- ggplot(rr, aes(ratio, unit, fill = is_tr)) + geom_col(width = 0.7) +
    scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = "black"), guide = "none") +
    labs(x = "Post/pre RMSPE ratio", y = NULL) + theme(axis.text.y = element_text(size = 6))
  ggsave(file.path(out, "fig_ratio.png"), p3, width = 5, height = max(4, 0.12 * nrow(rr)), dpi = 300, bg = "white")
}

if (nrow(loo)) {
  p4 <- ggplot() + geom_line(data = loo, aes(year, gap, group = dropped), colour = "grey65", linewidth = 0.4) +
    geom_line(data = main$gap, aes(year, gap), linewidth = 1) + geom_hline(yintercept = 0) + vline +
    labs(x = NULL, y = "Gap (pp)", title = "Leave-one-out: each line drops one weighted donor")
  ggsave(file.path(out, "fig_loo.png"), p4, width = 7, height = 4.5, dpi = 300, bg = "white")
}

if (!is.null(asc)) {
  p5 <- gaps %>% filter(!is.na(ascm_att)) %>%
    ggplot(aes(year, ascm_att)) +
    geom_ribbon(aes(ymin = ascm_lower, ymax = ascm_upper), fill = "grey85") +
    geom_line(linewidth = 0.9) + geom_hline(yintercept = 0) + vline +
    labs(x = NULL, y = "ASCM effect (pp), 95% conformal interval", title = "Augmented SCM")
  ggsave(file.path(out, "fig_ascm.png"), p5, width = 7, height = 4.5, dpi = 300, bg = "white")
}

# 10. Summary ---------------------------------------------------------------------
sink(file.path(out, "summary.txt"))
cat(ev$label, "\n", strrep("=", nchar(ev$label)), "\n\n", sep = "")
cat(sprintf("Treated: %s | outcome %s (%s) | first treated year %d\n", tr, ev$outcome, ev$location, ev$first_treated))
cat(sprintf("Window %d-%d | %d pre-treatment years | %d post-treatment years | %d donors\n",
            min(win), max(win), length(pre), length(post), length(donors)))
cat(sprintf("Predictors: %s + outcome at %s\n\n", paste(pv, collapse = ", "),
            paste(unique(c(min(pre), pre[ceiling(length(pre) / 2)], max(pre))), collapse = ", ")))
cat(sprintf("Convex hull: treated outside donor range in %d of %d pre-years (%.0f%%)%s\n\n",
            sum(hull$outside), nrow(hull), 100 * hull_share,
            if (hull_flag) " -> report ASCM as primary" else ""))
cat("Donor weights > 0.01:\n"); print(as.data.frame(filter(main$weights, weight > 0.01)), row.names = FALSE)
cat(sprintf("\nSolver setting used for main fit: %s\n", main$solver))
if (!is.null(pl)) {
  cat(sprintf("Placebo fits: %d of %d succeeded", n_distinct(pl$unit) - 1, length(donors)))
  if (length(attr(pl, "failed"))) cat(" | failed:", paste(attr(pl, "failed"), collapse = ", "))
  cat("\nPlacebo solver settings used:\n"); print(table(distinct(pl, unit, solver)$solver))
}
if (length(loo_failed)) cat("Leave-one-out fits failed when dropping:", paste(loo_failed, collapse = ", "), "\n")
cat(sprintf("\nSCM pre-RMSPE: %.3f pp | post-RMSPE: %.3f pp | mean post gap: %+.2f pp\n",
            main$pre_rmspe, main$post_rmspe, avg_scm))
if (ev$target == 0) cat("(Outcome targets 0: a negative gap means the programme reduced it.)\n")
if (!is.null(pvals)) { cat("\nIn-space placebo p-values (post/pre RMSPE ratio):\n"); print(as.data.frame(pvals), row.names = FALSE) }
if (nrow(pt)) {
  cat("\nIn-time placebos (mean gap over false post-period; should be near 0):\n")
  print(as.data.frame(pt %>% filter(fake_post) %>% group_by(fake_start) %>% summarise(mean_gap = mean(gap))), row.names = FALSE)
}
if (nrow(bd)) {
  cat("\nBackdated starts (mean gap in the years before the real start; should be near 0):\n")
  print(as.data.frame(bd %>% filter(year >= backdated_start, year < ev$first_treated) %>%
                        group_by(backdated_start) %>% summarise(mean_gap = mean(gap))), row.names = FALSE)
}
if (nrow(loo)) {
  cat("\nLeave-one-out mean post gap by dropped donor:\n")
  print(as.data.frame(loo %>% filter(year %in% post) %>% group_by(dropped) %>% summarise(mean_gap = mean(gap))), row.names = FALSE)
}
cat("\nEstimator comparison (mean post-treatment effect, pp):\n"); print(as.data.frame(est), row.names = FALSE)
if (!is.null(asc)) cat(sprintf("\nASCM scaled L2 imbalance: %.3f (0 = perfect pre-fit, 1 = no better than uniform weights)\n", asc$scaled_l2))
sink()
msg("Results written to %s", out)
