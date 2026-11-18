# Can SDG indicators see national programmes? Replication code

R code that reproduces every table and figure in:

> Akki, B., Razak, B. H. A., Nagaraj, V. K., Rashmi, S. M., & Sanjay Shekar, N. C. (2026). *Can SDG indicators see national programmes? Synthetic control evidence from India's sanitation and electrification missions.* Manuscript under review.

The pipeline (i) classifies the shape of every UN SDG access series (open defecation, safely managed sanitation, safely managed drinking water, electricity access) across countries, and (ii) estimates synthetic control, augmented synthetic control and synthetic difference-in-differences models for India's Swachh Bharat Mission (Grameen) and Saubhagya, with placebo, backdating, leave-one-out and conformal inference.

## Data

No data files are redistributed here. Every input is public:

| Input | Source | How it is obtained |
|---|---|---|
| SDG series SH_SAN_DEFECT, SH_SAN_SAFE, SH_H2O_SAFE, EG_ACS_ELEC | UN Global SDG Indicators Database (release 2026_Q3.1, 30 Sep 2026; accessed 1 Nov 2026) | downloaded automatically by `01_extract_sdg.R` |
| GDP per capita, urban share, population density, ODA per capita | World Bank World Development Indicators | downloaded automatically by `02_extract_covariates.R` |
| Historical income classifications (`OGHIST.xlsx`) | World Bank | download manually into `data/raw/` |
| Female mean years of schooling (`HDR25_Composite_indices_complete_time_series.csv`) | UNDP Human Development Report 2025 | download manually into `data/raw/` |
| India survey points for Fig. 1 (NFHS-3/4/5 and other JMP inputs) | WHO/UNICEF JMP country file for India, 2025 update (washdata.org) | values entered in `06_fig1_nfhs.R` |

Because the UN database is revised with each release, a later download can differ slightly from the release used in the paper.

## Requirements

R 4.3 or later. `00_install.R` installs the packages (tidyverse, WDI, readxl, Synth, augsynth, synthdid and others). Open `sdg-causal.Rproj` in RStudio, or run every script from this folder.

## Run order

```r
Rscript 00_install.R                 # once per machine
Rscript 01_extract_sdg.R             # UN SDG API -> data/processed/sdg_long.csv
# download OGHIST.xlsx and the UNDP HDR composite time-series CSV into data/raw/ first
Rscript 02_extract_covariates.R      # WDI + income history + female schooling
Rscript 03_build_donor_pool.R        # five donor rules, all events
Rscript 04_fit_event.R sbm           # one event at a time (see list below)
Rscript 05_linearity_diagnostic.R    # Table 3: shape of every SDG access series
Rscript 06_fig1_nfhs.R               # Fig. 1 (needs sbm and sbm_incl_khm results)
```

Events used in the paper: `sbm` (Swachh Bharat, Cambodia excluded), `sbm_incl_khm`
(Cambodia in the pool), `saubhagya`, `saubhagya_rural`, `elec_2015`. Inside RStudio, run
an event other than `sbm` with:

```r
commandArgs <- function(...) "saubhagya"; source("04_fit_event.R"); rm(commandArgs)
```

Run every script from the project root. A full fit with placebos takes roughly 5–10 minutes per event. `SKIP_PLACEBOS=1 Rscript 04_fit_event.R sbm` gives a quick first look in under a minute.

## What each step decides

| Step | Paper section | Where the choices live |
|---|---|---|
| Outcome series, Location dimension, treatment years | 3.1–3.2 | `config/config.R` → `events` |
| Income, coverage, interpolation, ceiling rules | 3.3 | `config/config.R` → `rules` |
| Concurrent programmes and shocks (Rules 3–4) | 3.3 | `config/exclusions.csv` (hand-curated, see `config/README_exclusions.md`) |
| Predictors and outcome lags | 3.4 | `config/config.R` → `wdi`, `R/fit.R::synth_fit` |
| SCM, ASCM, SDID | 3.5 | `R/fit.R` |
| Placebos, backdating, leave-one-out, conformal intervals | 3.6 | `R/fit.R`, `04_fit_event.R` |

Every removal from a donor pool is written to `data/processed/donor_log_<event>.csv` with its rule number. That file becomes the supplementary donor-exclusion table.

## Reading `results/<event>/summary.txt`

1. **Convex hull line first.** If India sits outside the donor range in more than 20% of pre-treatment years, report augmented SCM as the primary estimate and the standard SCM alongside it.
2. **Pre-RMSPE.** This should be small relative to the effect. A pre-RMSPE of 2 pp next to a 3 pp effect is not credible.
3. **Placebo p-value.** With about 50 donors the smallest possible value is about 0.02. Report both the full and the RMSPE-filtered sets.
4. **In-time and backdating rows.** These should sit near zero. A large gap at a false date means the model is picking up a trend, not the programme.
5. **Estimator table.** Agreement in sign and magnitude across SCM, ASCM and SDID is the headline robustness result.

The per-year ASCM conformal bands are wide by construction when there is a single treated unit. Use the conformal p-value for the average effect as the main ASCM inference, and present the bands in the persistence figure.

## Testing without the real data

```bash
Rscript tests/make_fake_data.R
SDG_DATA_DIR=tests/fake_data Rscript 03_build_donor_pool.R sbm
SDG_DATA_DIR=tests/fake_data SDG_RESULTS_DIR=tests/fake_results Rscript 04_fit_event.R sbm
```

The simulated panel builds in a known India effect of −3, −6, −9 and then −12 pp on open defecation from 2015, a post-period mean of −10.2 pp. On that data the pipeline recovers −10.4 (SCM), −11.6 (ASCM) and −11.1 (SDID), with placebo p = 0.018 and in-time placebo gaps below 0.6 pp. **Never use the fake data or its results in the paper.**

## Known behaviour

- Synth's `ipop` solver fails for occasional donor sets. `synth_fit()` retries with three alternative settings and records the one used, and `summary.txt` reports the solver counts for the placebo fits. Report this in the Supplementary Material.
- Synth prints a harmless "coercing argument of type 'double' to logical" warning from its internals.
- If `OGHIST.xlsx` is missing, script 02 falls back to *current* income groups and warns. Do not publish estimates from that fallback.
