# 06_fig1_nfhs.R — Fig. 1: India's official open-defecation series, synthetic India
# with and without Cambodia, and the NFHS survey estimates (Section 4.2)
# Needs: results/sbm_incl_khm/weights.csv  (Rscript 04_fit_event.R sbm_incl_khm)
#        results/sbm/weights.csv           (Rscript 04_fit_event.R sbm, Cambodia excluded)
source("config/config.R"); source("R/utils.R")
suppressPackageStartupMessages(library(ggplot2))

# NFHS rounds 3-5 as coded in the JMP 2025 India country file (rural open defecation)
NFHS <- tibble(year = c(2005.5, 2015.5, 2020.0), value = c(73.9, 54.3, 25.5),
               label = c("NFHS-3, 2005-06: 74%", "NFHS-4, 2015-16: 54%", "NFHS-5, 2019-21: 26%"))
# The other 22 surveys and censuses JMP fits its line through (same file, 'Data Summary')
JMP <- tibble(year  = c(2000, 2001, 2002, 2003, 2005, 2008, 2008, 2009, 2011, 2012, 2012,
                        2015, 2017, 2017, 2018, 2018, 2018, 2019, 2020, 2021, 2023, 2023),
              value = c(81.1, 78.1, 76.3, 80.8, 85.8, 65.8, 70.7, 65.2, 67.3, 42.2, 60.1,
                        52.1, 39.9, 33.0, 23.0, 29.9, 38.2, 6.8, 0.8, 20.7, 18.0, 11.4))

d <- read_csv(file.path(DATA_DIR, "processed", "sdg_long.csv"), show_col_types = FALSE) %>%
  filter(series == "SH_SAN_DEFECT", location == "RURAL", year %in% CFG$years)

synth_path <- function(wfile) {
  w <- read_csv(wfile, show_col_types = FALSE) %>% filter(weight > 0.001)
  d %>% inner_join(w, by = "iso3") %>% group_by(year) %>%
    summarise(value = sum(value * weight) / sum(weight), .groups = "drop")
}
lines <- bind_rows(
  d %>% filter(iso3 == "IND") %>% transmute(year, value, line = "India, official UN series"),
  synth_path(file.path(RESULTS_DIR, "sbm_incl_khm", "weights.csv")) %>% mutate(line = "Synthetic India, Cambodia in pool"),
  synth_path(file.path(RESULTS_DIR, "sbm", "weights.csv")) %>% mutate(line = "Synthetic India, Cambodia excluded"))
lines$line <- factor(lines$line, levels = c("India, official UN series",
                                            "Synthetic India, Cambodia in pool",
                                            "Synthetic India, Cambodia excluded"))

p <- ggplot(lines, aes(year, value, colour = line, linetype = line)) +
  geom_vline(xintercept = 2014.5, colour = "#8a8984", linetype = "dashed", linewidth = 0.4) +
  annotate("text", x = 2014.7, y = 97, label = "Swachh Bharat\nlaunched (Oct 2014)",
           hjust = 0, vjust = 1, size = 3, colour = "#52514e") +
  geom_point(data = JMP, aes(year, value), inherit.aes = FALSE, size = 1.3, colour = "#8a8984", alpha = 0.7) +
  geom_line(linewidth = 0.9) +
  geom_line(data = NFHS, aes(year, value), inherit.aes = FALSE, linetype = "dotted", colour = "#0b0b0b") +
  geom_point(data = NFHS, aes(year, value), inherit.aes = FALSE, size = 2.6, colour = "#0b0b0b") +
  geom_text(data = NFHS, aes(year + 0.4, value + 3, label = label), inherit.aes = FALSE,
            hjust = 0, size = 3, colour = "#0b0b0b") +
  scale_colour_manual(values = c("#2a78d6", "#eb6834", "#1baf7a"), name = NULL) +
  scale_linetype_manual(values = c("solid", "longdash", "dotted"), name = NULL) +
  scale_x_continuous(breaks = seq(2000, 2020, 5), limits = c(2000, 2025)) +
  scale_y_continuous(limits = c(0, 100), expand = c(0, 0)) +
  labs(x = NULL, y = "Rural population practising open defecation (%)") +
  theme_minimal(base_size = 10) +
  theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        legend.position = c(0.02, 0.02), legend.justification = c(0, 0),
        legend.key.width = unit(1.6, "lines"))

out <- dir_create(RESULTS_DIR, "figures")
ggsave(file.path(out, "fig1_sbm_nfhs.png"), p, width = 7.2, height = 4.6, dpi = 300, bg = "white")
ggsave(file.path(out, "fig1_sbm_nfhs.pdf"), p, width = 7.2, height = 4.6, bg = "white")
msg("Fig. 1 written to %s", out)
