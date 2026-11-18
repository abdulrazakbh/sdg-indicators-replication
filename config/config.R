# =============================================================================
# config.R — every analysis choice in one place
# Third SDG paper: synthetic control evaluation of mission-mode programmes
# =============================================================================
# Change values here, never inside the numbered scripts. Each run copies this
# file into results/<event>/config_used.R so estimates stay traceable.

DATA_DIR    <- Sys.getenv("SDG_DATA_DIR", "data")      # tests point this elsewhere
RESULTS_DIR <- Sys.getenv("SDG_RESULTS_DIR", "results")

CFG <- list(

  years = 2000:2024,   # JMP 2025 release covers 2000-2024; update with each release

  # --- SDG outcome series (UN Global SDG Indicators Database) ----------------
  series = c("SH_SAN_DEFECT", "SH_SAN_SAFE", "EG_ACS_ELEC", "SH_H2O_SAFE"),

  # --- Treatment events --------------------------------------------------------
  # first_treated: first calendar year counted as treated
  # location:      "RURAL" or "ALLAREA" (Location dimension of the SDG series)
  # target:        2030 target value used for the ceiling rule and gap closure
  events = list(
    sbm        = list(label = "Swachh Bharat Mission (Grameen) - open defecation",
                      treated = "IND", outcome = "SH_SAN_DEFECT", location = "RURAL",
                      first_treated = 2015, sector = "sanitation", target = 0),
    # Same event with Cambodia left in the donor pool (Cambodia is excluded only
    # from "sbm" in exclusions.csv); reproduces the first estimate in Section 4.2
    sbm_incl_khm = list(label = "Swachh Bharat Mission (Grameen) - open defecation, Cambodia in pool",
                      treated = "IND", outcome = "SH_SAN_DEFECT", location = "RURAL",
                      first_treated = 2015, sector = "sanitation", target = 0),
    sbm_safe   = list(label = "Swachh Bharat Mission (Grameen) - safely managed sanitation",
                      treated = "IND", outcome = "SH_SAN_SAFE", location = "RURAL",
                      first_treated = 2015, sector = "sanitation", target = 100),
    saubhagya  = list(label = "Saubhagya - access to electricity",
                      treated = "IND", outcome = "EG_ACS_ELEC", location = "ALLAREA",
                      first_treated = 2018, sector = "electricity", target = 100),
    # Electrification as one push: DDUGJY village electrification (Dec 2014)
    # followed by Saubhagya household connections (Oct 2017)
    elec_2015  = list(label = "India electrification push (DDUGJY + Saubhagya) - access to electricity",
                      treated = "IND", outcome = "EG_ACS_ELEC", location = "ALLAREA",
                      first_treated = 2015, sector = "electricity", target = 100),
    saubhagya_rural = list(label = "Saubhagya - access to electricity, rural",
                      treated = "IND", outcome = "EG_ACS_ELEC", location = "RURAL",
                      first_treated = 2018, sector = "electricity", target = 100),
    jjm        = list(label = "Jal Jeevan Mission - safely managed drinking water",
                      treated = "IND", outcome = "SH_H2O_SAFE", location = "RURAL",
                      first_treated = 2020, sector = "water", target = 100),
    nepal_shmp = list(label = "Nepal Sanitation and Hygiene Master Plan - open defecation",
                      treated = "NPL", outcome = "SH_SAN_DEFECT", location = "RURAL",
                      first_treated = 2012, sector = "sanitation", target = 0)
  ),

  # --- Donor-pool rules (Section 3.3) -----------------------------------------
  rules = list(
    income_groups  = c("L", "LM"),  # World Bank analytical classes allowed in year T0-1
    min_coverage   = 0.80,          # share of pre-treatment years with an observed value
    max_gap_interp = 2,             # longest interior gap filled by linear interpolation
    ceiling_margin = 5              # exclude donors whose last-3-pre-year mean is within
                                    # this many points of 0 or 100
  ),

  # --- Predictors (Section 3.4) -----------------------------------------------
  wdi = c(gdp_pc_ppp = "NY.GDP.PCAP.PP.KD",   # logged in the panel build
          urban_pct  = "SP.URB.TOTL.IN.ZS",
          pop_dens   = "EN.POP.DNST",          # logged in the panel build
          oda_pc     = "DT.ODA.ODAT.PC.ZS"),
  log_vars = c("gdp_pc_ppp", "pop_dens"),
  # Female mean years of schooling comes from the UNDP HDR composite time series
  # (columns mys_f_YYYY). Used only for sanitation and water events.
  female_schooling_sectors = c("sanitation", "water"),

  # --- Inference (Section 3.6) ------------------------------------------------
  placebo_rmspe_cut = 5,        # drop placebos with pre-RMSPE > 5x treated (sensitivity)
  in_time_offsets   = c(5, 8),  # false start dates T0-5 and T0-8
  backdate_offsets  = c(1, 2),  # anticipation checks
  loo_min_weight    = 0.01,     # donors above this weight are dropped one at a time
  hull_flag_share   = 0.20      # flag convex-hull problem if treated lies outside the
                                # donor range in more than 20% of pre-treatment years
)
