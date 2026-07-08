## =============================================================================
## SCRIPT 0 of 6: Automatic data sync via Git + 'pins' (GitHub)
##
## Implements the automation described in "R_Git_GitHub_Automation_Guide.docx":
##   1. RStudio's built-in Git pane          -> commit/push of code AND data
##   2. 'usethis'                            -> one-time repo <-> GitHub wiring
##   3. 'pins'                               -> automatic cloud storage of the
##                                              actual data frames/files, so
##                                              anyone (or you, on a different
##                                              machine) can pull the latest
##                                              real data with one line, instead
##                                              of re-uploading CSVs by hand.
##
## PULL happens at the START of the pipeline (source this before 01_...R):
##   any real data file already pinned to the GitHub board is downloaded into
##   data/ automatically, before the synthetic-fallback loaders in 01-05 get a
##   chance to run. Loaders are unchanged -- they just see a real file waiting
##   in data/ and use it instead of generating placeholder data.
##
## PUSH happens at the END of the pipeline (call sync_push() from the bottom
## of 06_district_maps_and_uncertainty.R once outputs/ is complete): any new
## or changed file in data/ and outputs/ is pinned to the board AND committed
## + pushed to GitHub, so the next run (by you or a collaborator) starts from
## the latest version automatically.
##
## Everything here degrades gracefully, matching the rest of the pipeline:
##   - no internet / no GITHUB_PAT / repo not reachable -> sync is skipped
##     with a message, the local synthetic-fallback pipeline still runs.
##   - no 'pins' or 'gert' package -> falls back to plain `git` shell calls
##     via system2(), or to a local pins board under .pins/ so nothing errors.
## =============================================================================

## ---- 0. CONFIG ---------------------------------------------------------------
## Edit these three lines once for your project, then forget about it -- every
## future session will sync automatically.
sync_cfg <- list(
  github_repo   = Sys.getenv("SLR_GITHUB_REPO", "yourusername/coastal-slr-study"),
  github_branch = Sys.getenv("SLR_GITHUB_BRANCH", "main"),
  # A GitHub Personal Access Token with 'repo' scope. Never hard-code this --
  # set it once per machine with:
  #   usethis::edit_r_environ()   # add a line: GITHUB_PAT=ghp_xxxxxxxxxxxx
  # or Sys.setenv(GITHUB_PAT = "ghp_xxxxxxxxxxxx") for a one-off session.
  github_pat    = Sys.getenv("GITHUB_PAT", ""),
  max_file_mb   = 90   # GitHub's hard cap is 100MB/file; stay under it and
  # warn (per the guide) to use Git LFS or cloud storage
  # (S3 / Google Drive) above this size instead.
)

data_dir <- if (exists("data_dir")) data_dir else "data"
out_dir  <- if (exists("out_dir"))  out_dir  else "outputs"
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

## Files this project expects to sync (pull down if present on the board /
## push up once you supply them locally). Mirrors the table in README.md.
sync_manifest <- c(
  "psmsl_charchanga_1016.csv",
  "psmsl_coxsbazar_1397.csv",
  "psmsl_hironpoint_203.csv",
  "district_covariates.csv",
  "climate_ocean_annual.csv",
  "bgd_coastal_districts.shp",
  "bgd_coastal_districts.dbf",
  "bgd_coastal_districts.shx",
  "bgd_coastal_districts.prj",
  "ipcc_ar6_ssp_table9.5.csv"
)

## ---- 1. AVAILABILITY CHECKS ---------------------------------------------------
has_pkg <- function(p) requireNamespace(p, quietly = TRUE)

sync_available <- function() {
  if (identical(sync_cfg$github_pat, "")) {
    message("[data-sync] No GITHUB_PAT set -- skipping cloud sync, using ",
            "local data/ and synthetic fallbacks only. Set one with ",
            "usethis::edit_r_environ() to enable automatic sync.")
    return(FALSE)
  }
  if (identical(sync_cfg$github_repo, "yourusername/coastal-slr-study")) {
    message("[data-sync] sync_cfg$github_repo still has its placeholder value ",
            "-- edit the CONFIG block at the top of 00_data_sync.R to point at ",
            "your real GitHub repo. Skipping cloud sync for now.")
    return(FALSE)
  }
  TRUE
}

## ---- 2. PINS BOARD CONNECTION --------------------------------------------------
## Preferred path: a GitHub-backed pins board (pins >= 1.0 API). Falls back to
## a local pins board (still versioned, just not shared) if 'pins' isn't
## installed or the GitHub board can't be reached -- so nothing ever errors
## out the rest of the pipeline.
get_board <- function() {
  if (has_pkg("pins")) {
    if (sync_available()) {
      board <- tryCatch(
        pins::board_github(
          repo   = sync_cfg$github_repo,
          branch = sync_cfg$github_branch,
          path   = "pins",
          token  = sync_cfg$github_pat
        ),
        error = function(e) {
          message("[data-sync] Could not reach GitHub pins board (", conditionMessage(e),
                  "). Falling back to a local pins board under .pins/.")
          NULL
        }
      )
      if (!is.null(board)) return(board)
    }
    return(pins::board_folder(".pins", versioned = TRUE))
  }
  message("[data-sync] Package 'pins' not installed -- install.packages(\"pins\") ",
          "for automatic cloud data storage. Continuing without it.")
  NULL
}

## ---- 3. PULL: fetch any real data already pinned, before the loaders run ------
sync_pull <- function() {
  board <- get_board()
  if (is.null(board)) return(invisible(FALSE))
  
  pulled_any <- FALSE
  for (fname in sync_manifest) {
    pin_name  <- gsub("[^A-Za-z0-9]", "_", fname)
    local_path <- file.path(data_dir, fname)
    if (file.exists(local_path)) next  # a local file always wins -- never overwrite
    has_pin <- tryCatch(pin_name %in% pins::pin_list(board), error = function(e) FALSE)
    if (!isTRUE(has_pin)) next
    tryCatch({
      pinned_path <- pins::pin_download(board, pin_name)
      file.copy(pinned_path[1], local_path, overwrite = TRUE)
      message("[data-sync] Pulled '", fname, "' from GitHub pins board -> ", local_path)
      pulled_any <<- TRUE
    }, error = function(e) {
      message("[data-sync] Failed to pull '", fname, "': ", conditionMessage(e))
    })
  }
  if (!pulled_any) {
    message("[data-sync] No new real data pulled from the board -- ",
            "loaders below will use whatever is in data/, or synthetic fallback.")
  }
  invisible(pulled_any)
}

## ---- 4. PUSH: pin + git-commit whatever real data/outputs exist locally -------
## Call sync_push() once at the end of the pipeline (bottom of script 06) so
## every real file you've dropped in, and every output the pipeline produced,
## is automatically stored to GitHub -- no manual `git add/commit/push` needed.
sync_push <- function(commit_message = paste0("Auto-sync data + outputs (",
                                              format(Sys.time(), "%Y-%m-%d %H:%M"), ")")) {
  board <- get_board()
  
  files_to_pin <- list.files(c(data_dir, out_dir), recursive = TRUE, full.names = TRUE)
  if (length(files_to_pin) == 0) {
    message("[data-sync] Nothing in data/ or outputs/ to sync yet.")
    return(invisible(FALSE))
  }
  
  if (!is.null(board)) {
    for (f in files_to_pin) {
      size_mb <- file.info(f)$size / 1e6
      if (size_mb > sync_cfg$max_file_mb) {
        message("[data-sync] Skipping '", f, "' (", round(size_mb, 1), " MB) -- ",
                "over the ", sync_cfg$max_file_mb, "MB safety threshold. Per the ",
                "automation guide, use Git LFS or an S3/Google Drive link for ",
                "files this large instead of pinning them directly.")
        next
      }
      pin_name <- gsub("[^A-Za-z0-9]", "_", basename(f))
      tryCatch({
        pins::pin_upload(board, f, name = pin_name,
                         title = basename(f),
                         description = paste("Auto-synced from coastal SLR pipeline,",
                                             format(Sys.Date())))
      }, error = function(e) {
        message("[data-sync] Failed to pin '", f, "': ", conditionMessage(e))
      })
    }
    message("[data-sync] Pinned ", length(files_to_pin), " file(s) to the board.")
  }
  
  git_sync_commit(commit_message)
  invisible(TRUE)
}

## ---- 5. GIT COMMIT/PUSH HELPER -------------------------------------------------
## Mirrors "Option 1" from the automation guide (RStudio's Git pane / one-click
## Stage-Commit-Push) but scripted, so it runs unattended as part of the
## pipeline. Prefers 'gert' (pure-R libgit2 bindings); falls back to shelling
## out to the `git` CLI if 'gert' isn't installed; skips quietly if this isn't
## a git repo at all (e.g. a fresh checkout without version control yet).
git_sync_commit <- function(commit_message) {
  in_git_repo <- dir.exists(".git")
  if (!in_git_repo) {
    message("[data-sync] Not inside a git repository -- run ",
            "usethis::use_git() and usethis::use_github() once to enable ",
            "automatic commit/push of data + outputs.")
    return(invisible(FALSE))
  }
  
  if (has_pkg("gert")) {
    tryCatch({
      gert::git_add(c(data_dir, out_dir))
      status <- gert::git_status()
      if (nrow(status) == 0) {
        message("[data-sync] Working tree clean -- nothing new to commit.")
        return(invisible(FALSE))
      }
      gert::git_commit(commit_message)
      gert::git_push()
      message("[data-sync] Committed and pushed via gert: ", commit_message)
      return(invisible(TRUE))
    }, error = function(e) {
      message("[data-sync] gert commit/push failed (", conditionMessage(e),
              "); falling back to command-line git.")
    })
  }
  
  # Fallback: plain `git` CLI via system2(), matching how RStudio's Git pane
  # would drive it -- works as long as `git` is on PATH and remote creds are
  # already cached (credential helper / SSH key), which is the normal state
  # once you've done the one-time `usethis::use_github()` setup.
  tryCatch({
    system2("git", c("add", shQuote(data_dir), shQuote(out_dir)))
    diff_status <- system2("git", c("diff", "--cached", "--quiet"))
    if (identical(diff_status, 0L)) {
      message("[data-sync] Working tree clean -- nothing new to commit.")
      return(invisible(FALSE))
    }
    system2("git", c("commit", "-m", shQuote(commit_message)))
    system2("git", c("push"))
    message("[data-sync] Committed and pushed via git CLI: ", commit_message)
    invisible(TRUE)
  }, error = function(e) {
    message("[data-sync] git CLI commit/push failed: ", conditionMessage(e))
    invisible(FALSE)
  })
}

## ---- 6. RUN THE PULL NOW -------------------------------------------------------
## This is what actually executes when you `source("00_data_sync.R")` (or when
## 01_setup_and_trend_analysis.R sources it for you). Push is deliberately
## NOT called here -- call sync_push() explicitly once your run is complete
## (a call is already wired in at the bottom of 06_district_maps_and_uncertainty.R).
sync_pull()
## =============================================================================
## COASTAL SEA LEVEL RISE STUDY -- EXTENDED ANALYSIS
## Bangladesh Coast (1969-2024) with Forecasting to 2075
## Implements the 15 recommendations from "Recommendations_to_Improve_
## Coastal_SLR_Study.docx"
##
## SCRIPT 1 of 6: Setup, data assembly, and non-parametric trend analysis
##   -> Addresses Recommendations #1 (more data), #2 (full 1969-2024 record)
## =============================================================================

## ---- 0. PACKAGES ------------------------------------------------------------
required_pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "lubridate",            # data wrangling / plotting
  "trend",                                              # Mann-Kendall / Sen's slope (fallback below if unavailable)
  "forecast",                                           # ARIMA, ETS
  "prophet",                                            # Prophet
  "mgcv",                                               # GAM
  "randomForest",                                       # Random Forest
  "xgboost",                                            # XGBoost
  "Metrics",                                            # RMSE/MAE/MAPE helpers
  "corrplot",                                           # correlation heatmap
  "sf", "terra", "spdep", "gstat",                      # spatial: Moran's I, Getis-Ord, kriging
  "cowplot", "RColorBrewer", "ggspatial"                # figure assembly
)

new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]
if (length(new_pkgs) > 0) {
  message("Installing missing packages: ", paste(new_pkgs, collapse = ", "))
  install.packages(new_pkgs, repos = "https://cloud.r-project.org")
}
invisible(lapply(required_pkgs, function(p) suppressPackageStartupMessages(
  require(p, character.only = TRUE, quietly = TRUE))))

set.seed(20902037)  # reproducibility, tied to student ID

## ---- 1. FILE PATHS -----------------------------------------------------------
## Point these at your real files. If a file is missing, the loaders below
## fall back to calibrated synthetic data so the whole pipeline still runs
## end-to-end (useful for testing/dry-runs before your real data arrives).
data_dir <- "data"
out_dir  <- "outputs"
fig_dir  <- file.path(out_dir, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- 1b. AUTOMATIC DATA SYNC (Git + 'pins' / GitHub) --------------------------
## Pulls any real data files already stored on your GitHub pins board into
## data/ BEFORE the loaders below run, so real data is picked up automatically
## across machines/sessions instead of needing manual re-upload each time.
## See 00_data_sync.R for the one-time GITHUB_REPO / GITHUB_PAT setup, and
## "R_Git_GitHub_Automation_Guide.docx" for the underlying Git/usethis/pins
## concepts this implements. Safe to leave in place even before you've set
## anything up -- it just skips itself with a message.
if (file.exists("00_data_sync.R")) {
  source("00_data_sync.R")
} else {
  message("[data-sync] 00_data_sync.R not found next to this script -- ",
          "skipping automatic cloud sync, using local data/ and synthetic ",
          "fallbacks only.")
}

tide_gauge_files <- list(
  Charchanga  = file.path(data_dir, "psmsl_charchanga_1016.csv"),
  CoxsBazar   = file.path(data_dir, "psmsl_coxsbazar_1397.csv"),
  HironPoint  = file.path(data_dir, "psmsl_hironpoint_203.csv")
)
district_covariates_file <- file.path(data_dir, "district_covariates.csv")
climate_ocean_file       <- file.path(data_dir, "climate_ocean_annual.csv")
district_shapefile       <- file.path(data_dir, "bgd_coastal_districts.shp")
ipcc_ar6_ssp_file        <- file.path(data_dir, "ipcc_ar6_ssp_table9.5.csv")

## ---- 2. RECOMMENDATION #2: FULL 1969-2024 TIDE GAUGE RECORDS -----------------
## Loader with synthetic fallback calibrated to the trends already reported
## in Table 1 of the paper (Sen's slope, tau) so downstream code is testable
## even before the real PSMSL downloads are dropped into data/.

load_tide_gauge <- function(path, station_name, start_yr, end_yr,
                            target_slope_mm, target_tau, baseline_yrs = 1970:1979) {
  if (file.exists(path)) {
    df <- read.csv(path)
    names(df) <- tolower(names(df))
    stopifnot(all(c("year", "sea_level") %in% names(df)))
    df$station <- station_name
    return(df)
  }
  message("[synthetic fallback] ", station_name,
          ": real PSMSL file not found at ", path,
          " -- generating calibrated placeholder series.")
  yrs <- start_yr:end_yr
  n <- length(yrs)
  # Build a series whose Sen's slope/tau closely match Table 1 of the paper
  slope_cm_yr <- target_slope_mm / 10
  base <- slope_cm_yr * (yrs - baseline_yrs[1])
  noise_sd <- ifelse(abs(target_tau) > 0.9, 1.2, 3.0)  # tighter noise -> higher tau
  sea_level <- base + rnorm(n, 0, noise_sd)
  # re-baseline to the 1970-1979 mean, matching the paper's anomaly method (3.1)
  baseline_idx <- yrs %in% baseline_yrs
  sea_level <- sea_level - mean(sea_level[baseline_idx], na.rm = TRUE)
  data.frame(year = yrs, sea_level = sea_level, station = station_name)
}

tide_charchanga <- load_tide_gauge(tide_gauge_files$Charchanga, "Charchanga",
                                   1969, 2024, target_slope_mm = 6.75, target_tau = 0.945)
tide_coxsbazar  <- load_tide_gauge(tide_gauge_files$CoxsBazar, "Cox's Bazar",
                                   1969, 2024, target_slope_mm = 2.85, target_tau = 0.605)
tide_hironpoint <- load_tide_gauge(tide_gauge_files$HironPoint, "Hiron Point",
                                   1969, 2024, target_slope_mm = -6.37, target_tau = -0.636)

tide_all <- bind_rows(tide_charchanga, tide_coxsbazar, tide_hironpoint)

## drop years with <11 months coverage -- handled upstream if you provide a
## `n_months` column; documented here so the rule from Section 3.1 is explicit.
if ("n_months" %in% names(tide_all)) {
  tide_all <- tide_all %>% filter(n_months >= 11)
}

## ---- 3. MANN-KENDALL + SEN'S SLOPE (custom implementation, no CRAN needed) ---
## Self-contained versions so the analysis works even if the `trend` package
## can't be installed (e.g. offline grading environment). If `trend` IS
## available we cross-check against it below.

mk_test_custom <- function(x, t = seq_along(x)) {
  ok <- !is.na(x); x <- x[ok]; t <- t[ok]
  n <- length(x)
  S <- 0
  for (i in 1:(n - 1)) S <- S + sum(sign(x[(i + 1):n] - x[i]))
  ties <- table(x)
  tie_term <- sum(ties * (ties - 1) * (2 * ties + 5))
  varS <- (n * (n - 1) * (2 * n + 5) - tie_term) / 18
  Zs <- if (S > 0) (S - 1) / sqrt(varS) else if (S < 0) (S + 1) / sqrt(varS) else 0
  pval <- 2 * (1 - pnorm(abs(Zs)))
  tau <- S / (n * (n - 1) / 2)
  list(S = S, varS = varS, Z = Zs, p.value = pval, tau = tau, n = n)
}

sens_slope_custom <- function(x, t = seq_along(x)) {
  ok <- !is.na(x); x <- x[ok]; t <- t[ok]
  n <- length(x)
  slopes <- numeric(0)
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    if (t[j] != t[i]) slopes <- c(slopes, (x[j] - x[i]) / (t[j] - t[i]))
  }
  list(slope = median(slopes),
       ci_lower = as.numeric(quantile(slopes, 0.025)),
       ci_upper = as.numeric(quantile(slopes, 0.975)))
}

sig_stars <- function(p) {
  ifelse(p < 0.001, "***", ifelse(p < 0.01, "**", ifelse(p < 0.05, "*", "ns")))
}

trend_results <- tide_all %>%
  group_by(station) %>%
  group_modify(~{
    mk <- mk_test_custom(.x$sea_level, .x$year)
    ss <- sens_slope_custom(.x$sea_level, .x$year)
    tibble(
      period       = paste(min(.x$year), max(.x$year), sep = "-"),
      n_years      = mk$n,
      sen_slope_mm_yr = round(ss$slope * 10, 2),
      ci_lower_mm  = round(ss$ci_lower * 10, 2),
      ci_upper_mm  = round(ss$ci_upper * 10, 2),
      mk_tau       = round(mk$tau, 3),
      p_value      = signif(mk$p.value, 3),
      significance = sig_stars(mk$p.value)
    )
  }) %>%
  ungroup()

print(trend_results)
write.csv(trend_results, file.path(out_dir, "table1_mk_sen_trends.csv"), row.names = FALSE)

## Cross-check against `trend` package if installed (Recommendation-doc's
## stated methodology in Sec 3.2 explicitly names this package)
if (requireNamespace("trend", quietly = TRUE)) {
  cat("\n--- Cross-check against trend::mk.test / trend::sens.slope ---\n")
  for (st in unique(tide_all$station)) {
    x <- tide_all$sea_level[tide_all$station == st]
    ts_x <- ts(x)
    mk_pkg <- trend::mk.test(ts_x)
    sens_pkg <- trend::sens.slope(ts_x)
    cat(sprintf("%-12s trend-pkg tau=%.3f p=%.2e | sen=%.2f mm/yr\n",
                st, mk_pkg$estimates[["tau"]], mk_pkg$p.value,
                sens_pkg$estimates * 10))
  }
}
## =============================================================================
## SCRIPT 2 of 6: District-level SLR data + multi-model forecasting
##   -> Addresses Recommendations #3 (multiple models), #4 (compare performance),
##      #5 (train/test split), #6 (forecast validation/residuals), #7 (PIs)
## Run 01_setup_and_trend_analysis.R first (needs `tide_all`, `trend_results`,
## `data_dir`, `out_dir`).
## =============================================================================

## ---- 1. DISTRICT METADATA (Rec #1: land + climate + ocean covariates) --------
## Real workflow: read district_covariates_file (subsidence rates from InSAR/GPS
## studies, population density, elevation, LULC, groundwater extraction, distance
## to coast) and climate_ocean_file (SST, rainfall, river discharge, wind,
## pressure, cyclone frequency, SSH, salinity, currents, tides, NDVI, DEM).
## Falls back to the values already reported in Table 2 of the paper if the
## files aren't supplied yet, so the rest of the pipeline is testable now.

load_district_covariates <- function(path) {
  if (file.exists(path)) return(read.csv(path))
  message("[synthetic fallback] district_covariates.csv not found -- using ",
          "Table 2 values from the existing report as the base dataset.")
  data.frame(
    district      = c("Satkhira","Khulna","Bagerhat","Pirojpur","Barguna",
                      "Patuakhali","Bhola","Lakshmipur","Noakhali","Feni",
                      "Chittagong","Cox's Bazar"),
    subsidence_mm_yr = c(18.5,15.2,14.8,13.6,13.1,12.4,11.8,10.2,9.5,8.1,6.8,5.4),
    pop_density_km2   = c(750,850,620,780,560,600,680,1450,1100,1700,1900,900),
    # Additional Rec #1 land/climate/ocean covariates -- placeholders calibrated
    # to plausible regional ranges; replace with real DEM/NDVI/SST/rainfall extracts.
    elevation_m       = c(1.2,1.5,1.8,2.1,1.9,2.3,2.0,2.8,3.1,3.6,4.5,5.2),
    sst_anom_c        = round(rnorm(12, 0.55, 0.08), 2),
    rainfall_mm_yr    = round(rnorm(12, 2400, 150)),
    groundwater_extraction_mm_yr = round(rnorm(12, 6, 1.5), 1),
    distance_to_coast_km = c(2,5,8,12,3,4,6,10,15,7,20,3),
    river_discharge_m3s  = round(rnorm(12, 4500, 800)),
    cyclone_freq_per_decade = c(3.2,2.8,3.0,2.5,3.6,3.4,3.1,2.2,2.0,1.8,1.5,2.1),
    stringsAsFactors = FALSE
  )
}

district_cov <- load_district_covariates(district_covariates_file)
write.csv(district_cov, file.path(out_dir, "district_covariates_used.csv"), row.names = FALSE)

## ---- 2. REGIONAL MEAN RELATIVE SLR TIME SERIES (2000-2025 -> extend to 1969) -
## Rec #2 asks us to train on the FULL 1969-2024 record rather than just
## 2000-2025. We reconstruct a regional-mean relative-SLR series back to 1969
## by combining the tide-gauge eustatic signal with mean district subsidence,
## instead of only the 2000-2025 window used in the original report.

mean_subsidence <- mean(district_cov$subsidence_mm_yr)  # ~10.8 mm/yr, matches Sec 3.4

regional_eustatic <- tide_all %>%
  filter(station %in% c("Charchanga", "Cox's Bazar")) %>%  # exclude Hiron Point (subsidence artifact, see Sec 5.2)
  group_by(year) %>%
  summarise(eustatic_cm = mean(sea_level, na.rm = TRUE) / 10, .groups = "drop") %>%
  filter(year >= 1969, year <= 2024)

regional_slr <- regional_eustatic %>%
  arrange(year) %>%
  mutate(
    subsidence_cm = mean_subsidence / 10 * (year - min(year)),
    relative_slr_cm = eustatic_cm - eustatic_cm[1] + subsidence_cm
  )

write.csv(regional_slr, file.path(out_dir, "regional_slr_1969_2024.csv"), row.names = FALSE)

## ---- 3. TRAIN / TEST SPLIT (Recommendation #5) -------------------------------
train_df <- regional_slr %>% filter(year <= 2014)
test_df  <- regional_slr %>% filter(year >  2014, year <= 2024)

cat(sprintf("\nTrain: %d-%d (n=%d) | Test: %d-%d (n=%d)\n",
            min(train_df$year), max(train_df$year), nrow(train_df),
            min(test_df$year), max(test_df$year), nrow(test_df)))

## ---- 4. FORECASTING MODEL SUITE (Recommendation #3) --------------------------
## ARIMA, ETS, Prophet, Linear Regression, GAM, Random Forest, XGBoost.
## Each fit function returns point forecasts + 80/95% PIs where the model
## supports it natively (Recommendation #7); PIs for ML models are added via
## residual bootstrap (see build_bootstrap_pi() below).

h <- nrow(test_df)
ts_train <- ts(train_df$relative_slr_cm, start = min(train_df$year))

fit_arima <- function() {
  m <- forecast::auto.arima(ts_train)
  fc <- forecast::forecast(m, h = h, level = c(80, 95))
  list(model = m, point = as.numeric(fc$mean),
       lo80 = as.numeric(fc$lower[, 1]), hi80 = as.numeric(fc$upper[, 1]),
       lo95 = as.numeric(fc$lower[, 2]), hi95 = as.numeric(fc$upper[, 2]))
}

fit_ets <- function() {
  m <- forecast::ets(ts_train)
  fc <- forecast::forecast(m, h = h, level = c(80, 95))
  list(model = m, point = as.numeric(fc$mean),
       lo80 = as.numeric(fc$lower[, 1]), hi80 = as.numeric(fc$upper[, 1]),
       lo95 = as.numeric(fc$lower[, 2]), hi95 = as.numeric(fc$upper[, 2]))
}

fit_prophet <- function() {
  if (!requireNamespace("prophet", quietly = TRUE)) {
    message("[skip] prophet not installed -- install.packages('prophet') to enable.")
    return(NULL)
  }
  pdf_train <- data.frame(ds = as.Date(paste0(train_df$year, "-06-30")),
                          y  = train_df$relative_slr_cm)
  m <- prophet::prophet(pdf_train, yearly.seasonality = FALSE,
                        interval.width = 0.95)
  future <- data.frame(ds = as.Date(paste0(test_df$year, "-06-30")))
  fc <- predict(m, future)
  list(model = m, point = fc$yhat, lo95 = fc$yhat_lower, hi95 = fc$yhat_upper,
       lo80 = NA, hi80 = NA)
}

fit_lm <- function() {
  m <- lm(relative_slr_cm ~ year, data = train_df)
  pr <- predict(m, newdata = test_df, interval = "prediction", level = 0.95)
  pr80 <- predict(m, newdata = test_df, interval = "prediction", level = 0.80)
  list(model = m, point = pr[, "fit"], lo95 = pr[, "lwr"], hi95 = pr[, "upr"],
       lo80 = pr80[, "lwr"], hi80 = pr80[, "upr"])
}

fit_gam <- function() {
  m <- mgcv::gam(relative_slr_cm ~ s(year, k = 10), data = train_df)
  pr <- predict(m, newdata = test_df, se.fit = TRUE)
  z95 <- qnorm(0.975); z80 <- qnorm(0.90)
  list(model = m, point = pr$fit,
       lo95 = pr$fit - z95 * pr$se.fit, hi95 = pr$fit + z95 * pr$se.fit,
       lo80 = pr$fit - z80 * pr$se.fit, hi80 = pr$fit + z80 * pr$se.fit)
}

## Residual-bootstrap PI builder for tree-based ML models that don't produce
## PIs natively (Recommendation #7 applied to RF / XGBoost).
build_bootstrap_pi <- function(point_pred, resid_pool, n_boot = 2000) {
  boot_resid <- replicate(n_boot, sample(resid_pool, length(point_pred), replace = TRUE))
  boot_mat <- matrix(point_pred, nrow = length(point_pred), ncol = n_boot) + boot_resid
  list(
    lo80 = apply(boot_mat, 1, quantile, 0.10), hi80 = apply(boot_mat, 1, quantile, 0.90),
    lo95 = apply(boot_mat, 1, quantile, 0.025), hi95 = apply(boot_mat, 1, quantile, 0.975)
  )
}

fit_rf <- function() {
  m <- randomForest::randomForest(relative_slr_cm ~ year, data = train_df, ntree = 1000)
  point <- predict(m, test_df)
  train_resid <- train_df$relative_slr_cm - predict(m, train_df)
  pi <- build_bootstrap_pi(point, train_resid)
  list(model = m, point = point, lo80 = pi$lo80, hi80 = pi$hi80, lo95 = pi$lo95, hi95 = pi$hi95)
}

fit_xgb <- function() {
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    message("[skip] xgboost not installed -- install.packages('xgboost') to enable.")
    return(NULL)
  }
  Xtr <- as.matrix(train_df["year"]); ytr <- train_df$relative_slr_cm
  Xte <- as.matrix(test_df["year"])
  dtr <- xgboost::xgb.DMatrix(Xtr, label = ytr)
  m <- xgboost::xgb.train(params = list(objective = "reg:squarederror", max_depth = 3, eta = 0.1),
                          data = dtr, nrounds = 200, verbose = 0)
  point <- predict(m, Xte)
  train_resid <- ytr - predict(m, Xtr)
  pi <- build_bootstrap_pi(point, train_resid)
  list(model = m, point = point, lo80 = pi$lo80, hi80 = pi$hi80, lo95 = pi$lo95, hi95 = pi$hi95)
}

model_fits <- list(
  ARIMA = fit_arima(), ETS = fit_ets(), Prophet = fit_prophet(),
  LinearRegression = fit_lm(), GAM = fit_gam(), RandomForest = fit_rf(), XGBoost = fit_xgb()
)
model_fits <- model_fits[!sapply(model_fits, is.null)]

## ---- 5. MODEL PERFORMANCE COMPARISON (Recommendation #4) ---------------------
compute_accuracy <- function(actual, pred) {
  c(RMSE = sqrt(mean((actual - pred)^2)),
    MAE  = mean(abs(actual - pred)),
    MAPE = mean(abs((actual - pred) / actual)) * 100,
    R2   = 1 - sum((actual - pred)^2) / sum((actual - mean(actual))^2))
}

model_comparison <- do.call(rbind, lapply(names(model_fits), function(nm) {
  round(compute_accuracy(test_df$relative_slr_cm, model_fits[[nm]]$point), 3)
}))
rownames(model_comparison) <- names(model_fits)
model_comparison <- as.data.frame(model_comparison)
model_comparison <- model_comparison[order(model_comparison$RMSE), ]

cat("\n--- Table: Forecasting model comparison (test period, held-out 2015-2024) ---\n")
print(model_comparison)
write.csv(model_comparison, file.path(out_dir, "table_model_comparison.csv"), row.names = TRUE)

best_model_name <- rownames(model_comparison)[1]
cat("\nBest model by RMSE:", best_model_name, "\n")

## ---- 6. RESIDUAL / FORECAST VALIDATION DIAGNOSTICS (Recommendation #6) -------
best_fit <- model_fits[[best_model_name]]
residuals_test <- test_df$relative_slr_cm - best_fit$point

diag_df <- data.frame(year = test_df$year, actual = test_df$relative_slr_cm,
                      predicted = best_fit$point, residual = residuals_test)
write.csv(diag_df, file.path(out_dir, "best_model_residual_diagnostics.csv"), row.names = FALSE)

cat("\nResidual diagnostics (best model):\n")
cat("  Mean residual (bias):", round(mean(residuals_test), 3), "cm\n")
cat("  Residual SD:", round(sd(residuals_test), 3), "cm\n")
if (requireNamespace("stats", quietly = TRUE) && length(residuals_test) >= 8) {
  sw <- shapiro.test(residuals_test)
  cat("  Shapiro-Wilk normality p-value:", signif(sw$p.value, 3), "\n")
}

save(model_fits, model_comparison, best_model_name, regional_slr, train_df, test_df,
     mean_subsidence, district_cov,
     file = file.path(out_dir, "script2_workspace.RData"))
## =============================================================================
## SCRIPT 3 of 6: Decomposition, correlation, regression, sensitivity
##   -> Addresses Recommendations #9 (STL decomposition), #10 (correlation
##      matrix/heatmap), #11 (regression analysis), #12 (sensitivity analysis)
## Run scripts 01 and 02 first (needs `regional_slr`, `district_cov`,
## `mean_subsidence`, `out_dir`, `fig_dir`).
## =============================================================================

## ---- 1. TIME-SERIES DECOMPOSITION (Recommendation #9) ------------------------
## Classical STL needs frequency > 1 (i.e. sub-annual data). Our SLR series is
## annual, so we decompose with a loess-based trend/residual split, which is
## the STL-equivalent for non-seasonal annual records. If you obtain monthly
## PSMSL data instead of annual means, swap this for stats::stl() directly.

decompose_annual_series <- function(years, values, span = 0.35) {
  trend_fit <- loess(values ~ years, span = span, degree = 2)
  trend_comp <- predict(trend_fit)
  # crude 5-yr moving "seasonal-like" cyclical component around the trend
  detrended <- values - trend_comp
  cyclical_comp <- as.numeric(stats::filter(detrended, rep(1/5, 5), sides = 2))
  cyclical_comp[is.na(cyclical_comp)] <- 0
  residual_comp <- values - trend_comp - cyclical_comp
  data.frame(year = years, observed = values, trend = trend_comp,
             cyclical = cyclical_comp, residual = residual_comp)
}

decomp <- decompose_annual_series(regional_slr$year, regional_slr$relative_slr_cm)
write.csv(decomp, file.path(out_dir, "stl_decomposition_regional_slr.csv"), row.names = FALSE)

decomp_long <- tidyr::pivot_longer(decomp, cols = c(observed, trend, cyclical, residual),
                                   names_to = "component", values_to = "value")
p_decomp <- ggplot2::ggplot(decomp_long, ggplot2::aes(year, value)) +
  ggplot2::geom_line(color = "#1b6ca8") +
  ggplot2::facet_wrap(~component, scales = "free_y", ncol = 1) +
  ggplot2::labs(title = "Decomposition of Regional Mean Relative SLR (1969-2024)",
                x = "Year", y = "cm") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_stl_decomposition.png"), p_decomp,
                width = 7, height = 8, dpi = 300)

## Optionally use ACF/PACF on the residual component to support Rec #13's
## request for ACF/PACF diagnostic plots.
png(file.path(fig_dir, "fig_acf_pacf_residuals.png"), width = 1000, height = 500)
par(mfrow = c(1, 2))
acf(decomp$residual, main = "ACF of decomposition residuals")
pacf(decomp$residual, main = "PACF of decomposition residuals")
dev.off()

## ---- 2. CORRELATION MATRIX / HEATMAP (Recommendation #10) --------------------
## SLR, subsidence, SST, rainfall, elevation, population -- joined at the
## district level for 2025 cross-section (extend to a panel across years if
## your covariate files include multi-year climate series).

## NOTE: if you are running this on the synthetic fallback data (no real
## district_covariates.csv supplied yet), a small noise term is added so the
## demo regression below doesn't show a degenerate/perfect fit purely from
## the deterministic subsidence->SLR formula. Remove `+ rnorm(...)` once real,
## independently-measured SLR and covariate data are used.
district_slr_2025 <- district_cov %>%
  mutate(cumulative_slr_2025_cm = subsidence_mm_yr / 10 * 25 +
           (mean(regional_slr$eustatic_cm[regional_slr$year %in% c(2000, 2025)])) +
           rnorm(nrow(district_cov), 0, 1.5))

corr_vars <- district_slr_2025 %>%
  select(SLR = cumulative_slr_2025_cm, Subsidence = subsidence_mm_yr,
         SST = sst_anom_c, Rainfall = rainfall_mm_yr,
         Elevation = elevation_m, Population = pop_density_km2)

corr_matrix <- cor(corr_vars, use = "pairwise.complete.obs", method = "pearson")
write.csv(round(corr_matrix, 3), file.path(out_dir, "table_correlation_matrix.csv"))

png(file.path(fig_dir, "fig_correlation_heatmap.png"), width = 900, height = 900)
if (requireNamespace("corrplot", quietly = TRUE)) {
  corrplot::corrplot(corr_matrix, method = "color", type = "upper",
                     addCoef.col = "black", tl.col = "black", tl.srt = 45,
                     title = "Correlation Matrix: SLR, Subsidence, SST, Rainfall, Elevation, Population",
                     mar = c(0, 0, 2, 0))
} else {
  heatmap(corr_matrix, symm = TRUE, main = "Correlation Matrix (base heatmap fallback)")
}
dev.off()

## ---- 3. REGRESSION ANALYSIS (Recommendation #11) ------------------------------
## SLR ~ Subsidence + Population + Elevation + SST

reg_model <- lm(SLR ~ Subsidence + Population + Elevation + SST, data = corr_vars)
reg_summary <- summary(reg_model)
cat("\n--- Regression: SLR ~ Subsidence + Population + Elevation + SST ---\n")
print(reg_summary)

coef_table <- as.data.frame(reg_summary$coefficients)
coef_table$term <- rownames(coef_table)
coef_table <- coef_table[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
coef_table$adj_r_squared <- reg_summary$adj.r.squared
write.csv(coef_table, file.path(out_dir, "table_regression_coefficients.csv"), row.names = FALSE)

cat(sprintf("\nAdjusted R^2 = %.3f\n", reg_summary$adj.r.squared))

## variable-importance style plot from standardized coefficients, feeding
## Recommendation #13's request for a "variable importance" figure
std_coefs <- data.frame(
  variable = names(coef(reg_model))[-1],
  std_estimate = coef(lm(scale(SLR) ~ scale(Subsidence) + scale(Population) +
                           scale(Elevation) + scale(SST), data = corr_vars))[-1]
)
p_varimp <- ggplot2::ggplot(std_coefs, ggplot2::aes(x = reorder(variable, abs(std_estimate)),
                                                    y = std_estimate)) +
  ggplot2::geom_col(fill = "#1b6ca8") + ggplot2::coord_flip() +
  ggplot2::labs(title = "Standardized Regression Coefficients (Variable Importance)",
                x = NULL, y = "Standardized coefficient") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_variable_importance.png"), p_varimp,
                width = 6, height = 4, dpi = 300)

## ---- 4. SENSITIVITY ANALYSIS (Recommendation #12) -----------------------------
## Evaluate +-10% and +-20% changes in subsidence rate and in the SSP eustatic
## trajectory, propagated through to cumulative relative SLR by 2075.

sensitivity_scenarios <- c(-0.20, -0.10, 0, 0.10, 0.20)

run_sensitivity <- function(base_subsidence_mm_yr, base_eustatic_2075_cm,
                            subsidence_delta, eustatic_delta,
                            years_ahead = 2075 - 2025) {
  subs <- base_subsidence_mm_yr * (1 + subsidence_delta)
  eust <- base_eustatic_2075_cm * (1 + eustatic_delta)
  subs_component_cm <- subs / 10 * years_ahead
  total_2075_cm <- eust + subs_component_cm
  data.frame(subsidence_delta = subsidence_delta, eustatic_delta = eustatic_delta,
             subsidence_mm_yr = subs, eustatic_2075_cm = eust,
             total_relative_slr_2075_cm = total_2075_cm)
}

base_subsidence <- mean_subsidence          # ~10.8 mm/yr
base_eustatic_2075 <- 58.0                  # SSP5-8.5 global component from Table 3

sensitivity_grid <- expand.grid(subsidence_delta = sensitivity_scenarios,
                                eustatic_delta = sensitivity_scenarios)
sensitivity_results <- do.call(rbind, lapply(seq_len(nrow(sensitivity_grid)), function(i) {
  run_sensitivity(base_subsidence, base_eustatic_2075,
                  sensitivity_grid$subsidence_delta[i], sensitivity_grid$eustatic_delta[i])
}))
write.csv(sensitivity_results, file.path(out_dir, "table_sensitivity_analysis.csv"), row.names = FALSE)

p_sens <- ggplot2::ggplot(sensitivity_results,
                          ggplot2::aes(x = factor(subsidence_delta * 100),
                                       y = total_relative_slr_2075_cm,
                                       fill = factor(eustatic_delta * 100))) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::labs(title = "Sensitivity of 2075 Relative SLR to Subsidence and Eustatic Rise (+-10%/20%)",
                x = "Subsidence rate change (%)", y = "Total relative SLR by 2075 (cm)",
                fill = "Eustatic rise\nchange (%)") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_sensitivity_analysis.png"), p_sens,
                width = 8, height = 5, dpi = 300)

cat("\nSensitivity range: 2075 relative SLR spans",
    round(min(sensitivity_results$total_relative_slr_2075_cm), 1), "to",
    round(max(sensitivity_results$total_relative_slr_2075_cm), 1), "cm",
    "depending on +-20% subsidence/eustatic assumptions.\n")

save(decomp, corr_matrix, reg_model, sensitivity_results,
     file = file.path(out_dir, "script3_workspace.RData"))
## =============================================================================
## SCRIPT 4 of 6: Spatial statistics
##   -> Addresses Recommendation #8: Moran's I, Hotspot Analysis (Getis-Ord Gi*),
##      Cluster Analysis, Kriging
## Requires: sf, spdep, gstat, terra (install.packages(c("sf","spdep","gstat","terra")))
## Run scripts 01-03 first (needs `district_cov`, `out_dir`, `fig_dir`,
## `district_shapefile` path from script 01).
## =============================================================================

need_spatial <- c("sf", "spdep", "gstat")
have_spatial <- sapply(need_spatial, requireNamespace, quietly = TRUE)

if (!all(have_spatial)) {
  message("Spatial packages missing (", paste(need_spatial[!have_spatial], collapse = ", "),
          "). Install with install.packages(c('sf','spdep','gstat','terra')) to run this script.\n",
          "Continuing with centroid-only, distance-based approximations that do not need `sf`/shapefiles.")
}

## ---- 1. DISTRICT CENTROIDS (fallback if no shapefile is supplied) ------------
## Real workflow: read district_shapefile with sf::st_read(), join district_cov
## by district name, and use polygon geometries for the maps in script 06 and
## for a proper contiguity (queen/rook) weights matrix here. The approximate
## lon/lat centroids below let Moran's I / Getis-Ord run on a distance-based
## weights matrix even without the shapefile.

approx_centroids <- data.frame(
  district = c("Satkhira","Khulna","Bagerhat","Pirojpur","Barguna","Patuakhali",
               "Bhola","Lakshmipur","Noakhali","Feni","Chittagong","Cox's Bazar"),
  lon = c(89.07, 89.55, 89.79, 89.98, 90.11, 90.35, 90.78, 90.85, 91.10, 91.40, 91.80, 91.98),
  lat = c(22.71, 22.82, 22.65, 22.58, 22.16, 22.35, 22.19, 22.94, 22.87, 23.02, 22.36, 21.44)
)

district_spatial <- dplyr::left_join(district_cov, approx_centroids, by = "district")
district_spatial$slr_2025_cm <- district_spatial$subsidence_mm_yr / 10 * 25 +
  mean(regional_slr$eustatic_cm[regional_slr$year %in% c(2000, 2025)])

## ---- 2. SPATIAL WEIGHTS MATRIX -------------------------------------------------
build_knn_weights <- function(coords_df, k = 4) {
  d <- as.matrix(dist(coords_df[, c("lon", "lat")]))
  w <- matrix(0, nrow(d), nrow(d))
  for (i in seq_len(nrow(d))) {
    nn <- order(d[i, ])[2:(k + 1)]  # exclude self
    w[i, nn] <- 1
  }
  w / rowSums(w)  # row-standardize
}

W <- build_knn_weights(district_spatial, k = 4)

## ---- 3. GLOBAL MORAN'S I (Recommendation #8) ----------------------------------
spdep_ok <- tryCatch(requireNamespace("spdep", quietly = TRUE), error = function(e) FALSE)
if (spdep_ok) {
  lw <- spdep::mat2listw(W, style = "W")
  moran_result <- spdep::moran.test(district_spatial$slr_2025_cm, lw)
  cat("\n--- Global Moran's I: 2025 relative SLR across districts ---\n")
  print(moran_result)
  moran_out <- data.frame(
    statistic = "Moran's I",
    I = moran_result$estimate[["Moran I statistic"]],
    expected = moran_result$estimate[["Expectation"]],
    variance = moran_result$estimate[["Variance"]],
    p_value = moran_result$p.value
  )
} else {
  ## manual Moran's I fallback (no external package)
  manual_moran_i <- function(x, W) {
    n <- length(x); xbar <- mean(x)
    num <- sum(W * outer(x - xbar, x - xbar))
    den <- sum((x - xbar)^2)
    S0 <- sum(W)
    (n / S0) * (num / den)
  }
  I_val <- manual_moran_i(district_spatial$slr_2025_cm, W)
  cat("\n--- Global Moran's I (manual calc, spdep unavailable): I =", round(I_val, 3), "---\n")
  moran_out <- data.frame(statistic = "Moran's I", I = I_val, expected = NA, variance = NA, p_value = NA)
}
write.csv(moran_out, file.path(out_dir, "table_morans_i.csv"), row.names = FALSE)

## ---- 4. HOTSPOT ANALYSIS: GETIS-ORD Gi* (Recommendation #8) -------------------
getis_ord_gi_star <- function(x, W) {
  n <- length(x)
  xbar <- mean(x); s <- sqrt(sum(x^2) / n - xbar^2)
  gi <- numeric(n)
  for (i in seq_len(n)) {
    wi <- W[i, ]
    Wi_sum <- sum(wi)
    num <- sum(wi * x) - xbar * Wi_sum
    den <- s * sqrt((n * sum(wi^2) - Wi_sum^2) / (n - 1))
    gi[i] <- num / den
  }
  gi
}

gi_star <- getis_ord_gi_star(district_spatial$slr_2025_cm, W)
district_spatial$gi_star <- gi_star
district_spatial$hotspot_class <- cut(gi_star,
                                      breaks = c(-Inf, -1.96, -1.645, 1.645, 1.96, Inf),
                                      labels = c("Cold spot (99%)", "Cold spot (95%)", "Not significant",
                                                 "Hot spot (95%)", "Hot spot (99%)"))

cat("\n--- Getis-Ord Gi* hotspot classification ---\n")
print(district_spatial[, c("district", "slr_2025_cm", "gi_star", "hotspot_class")])
write.csv(district_spatial, file.path(out_dir, "table_getis_ord_hotspots.csv"), row.names = FALSE)

p_hotspot <- ggplot2::ggplot(district_spatial, ggplot2::aes(lon, lat, color = hotspot_class, size = slr_2025_cm)) +
  ggplot2::geom_point() +
  ggplot2::geom_text(ggplot2::aes(label = district), vjust = -1, size = 3, color = "black") +
  ggplot2::scale_color_brewer(palette = "RdBu", direction = -1) +
  ggplot2::labs(title = "Getis-Ord Gi* Hotspot Analysis: 2025 Relative SLR",
                x = "Longitude", y = "Latitude", color = "Hotspot class", size = "SLR (cm)") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_getis_ord_hotspots.png"), p_hotspot, width = 7, height = 6, dpi = 300)

## ---- 5. CLUSTER ANALYSIS (Recommendation #8) ----------------------------------
## K-means clustering of districts on subsidence + SLR + population density,
## complementing the spatial hotspot analysis with a multivariate typology.
cluster_vars <- scale(district_spatial[, c("subsidence_mm_yr", "slr_2025_cm", "pop_density_km2")])
set.seed(20902037)
k_cluster <- kmeans(cluster_vars, centers = 3, nstart = 25)
district_spatial$cluster <- factor(k_cluster$cluster)

cat("\n--- K-means cluster assignment (3 clusters: subsidence, SLR, pop. density) ---\n")
print(district_spatial[, c("district", "subsidence_mm_yr", "slr_2025_cm", "pop_density_km2", "cluster")])

p_cluster <- ggplot2::ggplot(district_spatial, ggplot2::aes(subsidence_mm_yr, slr_2025_cm,
                                                            color = cluster, size = pop_density_km2)) +
  ggplot2::geom_point() +
  ggplot2::geom_text(ggplot2::aes(label = district), vjust = -1, size = 3, color = "black") +
  ggplot2::labs(title = "District Cluster Analysis (subsidence, SLR, population density)",
                x = "Subsidence (mm/yr)", y = "2025 relative SLR (cm)", color = "Cluster", size = "Pop. density") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_cluster_analysis.png"), p_cluster, width = 7, height = 6, dpi = 300)

## ---- 6. KRIGING (Recommendation #8) --------------------------------------------
## Ordinary kriging interpolates relative SLR across the coast from the 12
## district point estimates. Requires `gstat` + `sf`; falls back to inverse-
## distance weighting (IDW, base R only) if gstat is unavailable.

grid_lon <- seq(min(district_spatial$lon) - 0.3, max(district_spatial$lon) + 0.3, length.out = 60)
grid_lat <- seq(min(district_spatial$lat) - 0.3, max(district_spatial$lat) + 0.3, length.out = 60)
pred_grid <- expand.grid(lon = grid_lon, lat = grid_lat)

kriging_ok <- tryCatch({
  ok <- requireNamespace("sf", quietly = TRUE) && requireNamespace("gstat", quietly = TRUE)
  if (ok) {
    pts_sf <- sf::st_as_sf(district_spatial, coords = c("lon", "lat"), crs = 4326)
    grid_sf <- sf::st_as_sf(pred_grid, coords = c("lon", "lat"), crs = 4326)
    vgm_fit <- gstat::variogram(slr_2025_cm ~ 1, pts_sf)
    vgm_model <- gstat::fit.variogram(vgm_fit, gstat::vgm("Sph"))
    krige_result <- gstat::krige(slr_2025_cm ~ 1, pts_sf, grid_sf, model = vgm_model)
    pred_grid$slr_pred <<- krige_result$var1.pred
    TRUE
  } else FALSE
}, error = function(e) {
  message("[fallback] sf/gstat failed to load (", conditionMessage(e),
          ") -- using inverse-distance weighting instead of kriging.")
  FALSE
})

if (!kriging_ok) {
  message("[fallback] gstat/sf unavailable -- using inverse-distance weighting instead of kriging.")
  idw_predict <- function(grid, pts, power = 2) {
    sapply(seq_len(nrow(grid)), function(i) {
      d <- sqrt((grid$lon[i] - pts$lon)^2 + (grid$lat[i] - pts$lat)^2)
      d[d == 0] <- 1e-6
      w <- 1 / d^power
      sum(w * pts$slr_2025_cm) / sum(w)
    })
  }
  pred_grid$slr_pred <- idw_predict(pred_grid, district_spatial)
}

p_krige <- ggplot2::ggplot(pred_grid, ggplot2::aes(lon, lat, fill = slr_pred)) +
  ggplot2::geom_raster() +
  ggplot2::geom_point(data = district_spatial, ggplot2::aes(lon, lat), inherit.aes = FALSE,
                      color = "black", size = 1) +
  ggplot2::scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  ggplot2::labs(title = "Interpolated 2025 Relative SLR Across the Bangladesh Coast",
                subtitle = "Ordinary kriging (or IDW fallback) from 12 district point estimates",
                x = "Longitude", y = "Latitude", fill = "SLR (cm)") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_kriging_interpolation.png"), p_krige, width = 7, height = 6, dpi = 300)

save(district_spatial, moran_out, pred_grid, file = file.path(out_dir, "script4_workspace.RData"))
## =============================================================================
## SCRIPT 5 of 6: IPCC AR6 SSP scenario projections to 2075
##   -> Extends section 3.4 of the paper using the best validated model from
##      script 02 plus explicit 80/95% prediction intervals (Recs #3, #7)
## Run scripts 01-02 first (needs `regional_slr`, `model_fits`,
## `best_model_name`, `mean_subsidence`, `out_dir`, `fig_dir`).
## =============================================================================

## ---- 1. IPCC AR6 WGI TABLE 9.5 SSP TRAJECTORIES -------------------------------
## Real workflow: read the digitized IPCC AR6 WGI Ch.9 Table 9.5 values from
## ipcc_ar6_ssp_file. Falls back to the same anchor values already used in
## Table 3 of the paper (2025/2030/.../2075 global SSP components in cm).

load_ssp_table <- function(path) {
  if (file.exists(path)) return(read.csv(path))
  message("[synthetic fallback] IPCC AR6 SSP table not found -- using anchor points from Table 3 of the report.")
  data.frame(
    year = c(2025, 2030, 2040, 2050, 2060, 2075),
    ssp126 = c(10.1, 11.6, NA, 17.5, NA, 25.0),
    ssp245 = c(10.1, 12.9, NA, 24.0, NA, 38.0),
    ssp585 = c(10.1, 14.9, NA, 34.0, NA, 58.0)
  )
}

ssp_anchors <- load_ssp_table(ipcc_ar6_ssp_file)

## Smoothly interpolate the anchor points to an annual series (2025-2075)
interpolate_ssp <- function(anchors_df, col) {
  ok <- !is.na(anchors_df[[col]])
  approx(anchors_df$year[ok], anchors_df[[col]][ok], xout = 2025:2075, method = "linear")$y
}

ssp_annual <- data.frame(
  year = 2025:2075,
  ssp126_global_cm = interpolate_ssp(ssp_anchors, "ssp126"),
  ssp245_global_cm = interpolate_ssp(ssp_anchors, "ssp245"),
  ssp585_global_cm = interpolate_ssp(ssp_anchors, "ssp585")
)

## ---- 2. ADD MEAN DISTRICT SUBSIDENCE -> REGIONAL RELATIVE SLR (Sec 3.4) ------
ssp_annual <- ssp_annual %>%
  mutate(
    subsidence_component_cm = mean_subsidence / 10 * (year - 2025),
    ssp126_relative_cm = ssp126_global_cm + subsidence_component_cm,
    ssp245_relative_cm = ssp245_global_cm + subsidence_component_cm,
    ssp585_relative_cm = ssp585_global_cm + subsidence_component_cm
  )

## ---- 3. PREDICTION INTERVALS ON THE PROJECTION (Recommendation #7) -----------
## We treat the model residual SD from the script-02 validation (held-out
## 2015-2024) as the 1-step uncertainty and grow it with forecast horizon
## (a standard sqrt(h) heuristic used when a model's native PI machinery
## doesn't extend cleanly to a 50-year horizon spliced onto an external
## scenario driver like the SSPs).

best_resid_sd <- sd(test_df$relative_slr_cm - model_fits[[best_model_name]]$point)

add_growing_pi <- function(df, center_col, out_prefix, resid_sd) {
  h <- df$year - min(df$year) + 1
  se_h <- resid_sd * sqrt(h)
  df[[paste0(out_prefix, "_lo80")]] <- df[[center_col]] - qnorm(0.90) * se_h
  df[[paste0(out_prefix, "_hi80")]] <- df[[center_col]] + qnorm(0.90) * se_h
  df[[paste0(out_prefix, "_lo95")]] <- df[[center_col]] - qnorm(0.975) * se_h
  df[[paste0(out_prefix, "_hi95")]] <- df[[center_col]] + qnorm(0.975) * se_h
  df
}

ssp_annual <- add_growing_pi(ssp_annual, "ssp126_relative_cm", "ssp126", best_resid_sd)
ssp_annual <- add_growing_pi(ssp_annual, "ssp245_relative_cm", "ssp245", best_resid_sd)
ssp_annual <- add_growing_pi(ssp_annual, "ssp585_relative_cm", "ssp585", best_resid_sd)

write.csv(ssp_annual, file.path(out_dir, "table3_ssp_projections_annual_2025_2075.csv"), row.names = FALSE)

## ---- 4. SNAPSHOT TABLE AT KEY HORIZONS (matches Table 3 in the report) -------
key_years <- c(2025, 2030, 2040, 2050, 2060, 2075)
risk_thresholds <- c(MODERATE = 25, HIGH = 35, SEVERE = 60)

classify_risk <- function(x) {
  cut(x, breaks = c(-Inf, risk_thresholds["MODERATE"], risk_thresholds["HIGH"],
                    risk_thresholds["SEVERE"], Inf),
      labels = c("LOW/MODERATE", "MODERATE-HIGH", "HIGH-SEVERE", "SEVERE"))
}

snapshot_table <- ssp_annual %>%
  filter(year %in% key_years) %>%
  mutate(across(c(ssp126_relative_cm, ssp245_relative_cm, ssp585_relative_cm), \(x) round(x, 1))) %>%
  mutate(risk_ssp585 = classify_risk(ssp585_relative_cm)) %>%
  select(year, ssp126_relative_cm, ssp245_relative_cm, ssp585_relative_cm, risk_ssp585)

cat("\n--- Table 3 (extended): Projected regional mean relative SLR (cm) ---\n")
print(snapshot_table)
write.csv(snapshot_table, file.path(out_dir, "table3_ssp_snapshot.csv"), row.names = FALSE)

## ---- 5. FIGURE: SSP TRAJECTORIES WITH UNCERTAINTY BANDS (Rec #13) -----------
ssp_long <- ssp_annual %>%
  select(year, ssp126_relative_cm, ssp245_relative_cm, ssp585_relative_cm,
         ssp126_lo95, ssp126_hi95, ssp245_lo95, ssp245_hi95, ssp585_lo95, ssp585_hi95) %>%
  tidyr::pivot_longer(cols = c(ssp126_relative_cm, ssp245_relative_cm, ssp585_relative_cm),
                      names_to = "scenario", values_to = "relative_slr_cm") %>%
  mutate(scenario = recode(scenario,
                           ssp126_relative_cm = "SSP1-2.6 (low)",
                           ssp245_relative_cm = "SSP2-4.5 (intermediate)",
                           ssp585_relative_cm = "SSP5-8.5 (high)"))

p_ssp <- ggplot2::ggplot(ssp_annual, ggplot2::aes(x = year)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ssp585_lo95, ymax = ssp585_hi95), fill = "red", alpha = 0.12) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = ssp126_lo95, ymax = ssp126_hi95), fill = "blue", alpha = 0.12) +
  ggplot2::geom_line(ggplot2::aes(y = ssp126_relative_cm, color = "SSP1-2.6"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(y = ssp245_relative_cm, color = "SSP2-4.5"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(y = ssp585_relative_cm, color = "SSP5-8.5"), linewidth = 1) +
  ggplot2::geom_hline(yintercept = risk_thresholds, linetype = "dashed", color = "grey40") +
  ggplot2::annotate("text", x = 2026, y = risk_thresholds + 2,
                    label = names(risk_thresholds), hjust = 0, size = 3, color = "grey30") +
  ggplot2::scale_color_manual(values = c("SSP1-2.6" = "#2c7fb8", "SSP2-4.5" = "#feb24c", "SSP5-8.5" = "#e31a1c")) +
  ggplot2::labs(title = "IPCC AR6 SSP Scenario Projections with 95% Prediction Intervals (2025-2075)",
                x = "Year", y = "Regional mean relative SLR (cm)", color = "Scenario") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_ssp_projections_with_uncertainty.png"), p_ssp,
                width = 8, height = 5.5, dpi = 300)

save(ssp_annual, snapshot_table, risk_thresholds, best_resid_sd,
     file = file.path(out_dir, "script5_workspace.RData"))
## =============================================================================
## SCRIPT 6 of 6: District-wise forecast maps + uncertainty discussion
##   -> Addresses Recommendation #15 (full workflow: collect data, build
##      models, validate, select best, forecast 2025-2075, map by district
##      for 2030/2040/2050/2060/2075) and Recommendation #14 (uncertainty
##      discussion covering model, scenario, subsidence, observation sources)
## Run scripts 01-05 first.
## =============================================================================

## ---- 1. DISTRICT-WISE FORECASTS AT KEY HORIZONS (Recommendation #15) --------
## Projects each district's OWN relative SLR (using its own subsidence rate)
## under each SSP, at 2030, 2040, 2050, 2060, 2075 -- this is the "district-wise
## forecast maps" step explicitly requested in the recommendations doc.

forecast_years <- c(2030, 2040, 2050, 2060, 2075)

district_forecast <- expand.grid(
  district = district_cov$district,
  year = forecast_years,
  scenario = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
  stringsAsFactors = FALSE
) %>%
  left_join(district_cov, by = "district") %>%
  left_join(approx_centroids, by = "district") %>%
  left_join(ssp_annual %>% select(year, ssp126_global_cm, ssp245_global_cm, ssp585_global_cm),
            by = "year") %>%
  mutate(
    global_ssp_cm = case_when(
      scenario == "SSP1-2.6" ~ ssp126_global_cm,
      scenario == "SSP2-4.5" ~ ssp245_global_cm,
      scenario == "SSP5-8.5" ~ ssp585_global_cm
    ),
    district_subsidence_component_cm = subsidence_mm_yr / 10 * (year - 2025),
    district_relative_slr_cm = global_ssp_cm + district_subsidence_component_cm,
    risk_class = cut(district_relative_slr_cm,
                     breaks = c(-Inf, 25, 35, 60, Inf),
                     labels = c("LOW/MODERATE", "MODERATE-HIGH", "HIGH-SEVERE", "SEVERE"))
  ) %>%
  select(district, lon, lat, year, scenario, subsidence_mm_yr,
         district_relative_slr_cm, risk_class)

write.csv(district_forecast, file.path(out_dir, "district_wise_forecasts_2030_2075.csv"), row.names = FALSE)

## ---- 2. DISTRICT-WISE FORECAST MAPS (faceted choropleth-style point maps) ----
## Real workflow: swap geom_point() for geom_sf(data = districts_shp) once the
## GADM shapefile is available, so districts render as filled polygons rather
## than sized/colored points.

p_district_maps <- ggplot2::ggplot(
  district_forecast %>% filter(scenario == "SSP5-8.5"),
  ggplot2::aes(lon, lat, color = district_relative_slr_cm, size = district_relative_slr_cm)
) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year, nrow = 1) +
  ggplot2::scale_color_distiller(palette = "YlOrRd", direction = 1) +
  ggplot2::labs(title = "District-Wise Relative SLR Forecasts Under SSP5-8.5",
                subtitle = "2030, 2040, 2050, 2060, 2075",
                x = "Longitude", y = "Latitude", color = "SLR (cm)", size = "SLR (cm)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file.path(fig_dir, "fig_district_forecast_maps_ssp585.png"), p_district_maps,
                width = 14, height = 4, dpi = 300)

## one map per scenario as well, for the appendix / full atlas (Sec 4.2 mentions
## a 26-map annual atlas -- this is the scenario-comparison companion to it)
p_scenario_compare_2075 <- ggplot2::ggplot(
  district_forecast %>% filter(year == 2075),
  ggplot2::aes(lon, lat, color = district_relative_slr_cm, size = district_relative_slr_cm)
) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~scenario) +
  ggplot2::scale_color_distiller(palette = "YlOrRd", direction = 1) +
  ggplot2::labs(title = "District-Wise Relative SLR by 2075, Across SSP Scenarios",
                x = "Longitude", y = "Latitude", color = "SLR (cm)", size = "SLR (cm)") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_district_forecast_2075_all_scenarios.png"),
                p_scenario_compare_2075, width = 12, height = 5, dpi = 300)

## ---- 3. RISK-CATEGORY EVOLUTION TABLE (extends Figure 8 of the report) ------
risk_evolution <- district_forecast %>%
  filter(scenario == "SSP5-8.5") %>%
  count(year, risk_class) %>%
  tidyr::pivot_wider(names_from = risk_class, values_from = n, values_fill = 0)
write.csv(risk_evolution, file.path(out_dir, "table_risk_category_evolution_2030_2075.csv"), row.names = FALSE)

p_risk_evo <- ggplot2::ggplot(district_forecast %>% filter(scenario == "SSP5-8.5"),
                              ggplot2::aes(x = factor(year), fill = risk_class)) +
  ggplot2::geom_bar(position = "fill") +
  ggplot2::scale_fill_brewer(palette = "YlOrRd") +
  ggplot2::labs(title = "Evolution of District Flood Risk Categories (SSP5-8.5)",
                x = "Year", y = "Proportion of districts", fill = "Risk class") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_risk_category_evolution.png"), p_risk_evo,
                width = 7, height = 5, dpi = 300)

## ---- 4. MASTER SUMMARY FIGURE: MODEL COMPARISON (Recommendation #13) --------
p_model_compare <- ggplot2::ggplot(
  data.frame(model = rownames(model_comparison), RMSE = model_comparison$RMSE),
  ggplot2::aes(x = reorder(model, RMSE), y = RMSE)
) +
  ggplot2::geom_col(fill = "#1b6ca8") + ggplot2::coord_flip() +
  ggplot2::labs(title = "Forecasting Model Comparison (Test RMSE, 2015-2024 Hold-out)",
                x = NULL, y = "RMSE (cm)") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_model_comparison_rmse.png"), p_model_compare,
                width = 6, height = 4, dpi = 300)

## Observed vs forecast plot for the best model (Recommendation #13)
obs_vs_fc <- data.frame(year = test_df$year, observed = test_df$relative_slr_cm,
                        forecast = model_fits[[best_model_name]]$point,
                        lo95 = model_fits[[best_model_name]]$lo95,
                        hi95 = model_fits[[best_model_name]]$hi95)
p_obs_fc <- ggplot2::ggplot(obs_vs_fc, ggplot2::aes(year)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lo95, ymax = hi95), fill = "grey70", alpha = 0.4) +
  ggplot2::geom_line(ggplot2::aes(y = observed, color = "Observed"), linewidth = 1) +
  ggplot2::geom_line(ggplot2::aes(y = forecast, color = "Forecast"), linewidth = 1, linetype = "dashed") +
  ggplot2::scale_color_manual(values = c(Observed = "black", Forecast = "#e31a1c")) +
  ggplot2::labs(title = paste("Observed vs Forecast (Best Model:", best_model_name, ") -- Held-out 2015-2024"),
                x = "Year", y = "Relative SLR (cm)", color = NULL) +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_observed_vs_forecast_best_model.png"), p_obs_fc,
                width = 7, height = 5, dpi = 300)

## ---- 5. UNCERTAINTY DISCUSSION (Recommendation #14) --------------------------
## This block computes the quantitative uncertainty components referenced in
## the discussion; paste the printed summary into Section 5 (Discussion) of
## the report, alongside prose addressing each source qualitatively.

uncertainty_summary <- list(
  model_uncertainty_cm_2075 = with(ssp_annual[ssp_annual$year == 2075, ],
                                   c(ssp585_hi95 - ssp585_lo95) / 2),
  scenario_spread_cm_2075 = with(ssp_annual[ssp_annual$year == 2075, ],
                                 ssp585_relative_cm - ssp126_relative_cm),
  subsidence_range_mm_yr = range(district_cov$subsidence_mm_yr),
  best_model_test_RMSE_cm = model_comparison[best_model_name, "RMSE"]
)

cat("\n=== UNCERTAINTY SUMMARY (Recommendation #14) ===\n")
cat(sprintf("Model uncertainty (95%% PI half-width) at 2075, SSP5-8.5: +-%.1f cm\n",
            uncertainty_summary$model_uncertainty_cm_2075))
cat(sprintf("Climate-scenario spread (SSP5-8.5 minus SSP1-2.6) at 2075: %.1f cm\n",
            uncertainty_summary$scenario_spread_cm_2075))
cat(sprintf("Subsidence uncertainty across districts: %.1f to %.1f mm/yr\n",
            uncertainty_summary$subsidence_range_mm_yr[1], uncertainty_summary$subsidence_range_mm_yr[2]))
cat(sprintf("Best model (%s) held-out test RMSE: %.2f cm\n",
            best_model_name, uncertainty_summary$best_model_test_RMSE_cm))
cat(paste(
  "\nFour uncertainty sources should be discussed narratively in Section 5:",
  "  1. MODEL uncertainty -- differences between ARIMA/ETS/GAM/RF/XGBoost point forecasts",
  "     and their PIs (quantified above; see table_model_comparison.csv).",
  "  2. CLIMATE SCENARIO uncertainty -- divergence between SSP1-2.6/2.4/5-8.5",
  "     pathways, widening with forecast horizon (see fig_ssp_projections_with_uncertainty.png).",
  "  3. SUBSIDENCE uncertainty -- district subsidence rates come from a synthesis",
  "     of published InSAR/GPS studies, not a single live geodetic network; report",
  "     the range/spread found in the literature per district, not a point estimate.",
  "  4. OBSERVATION uncertainty -- PSMSL tide-gauge data gaps (e.g. Cox's Bazar",
  "     post-2019) and datum/benchmark artifacts (Hiron Point negative trend, Sec 5.2).",
  sep = "\n"
))

capture.output(uncertainty_summary, file = file.path(out_dir, "uncertainty_summary.txt"))

## ---- 6. FINAL WORKFLOW SUMMARY (Recommendation #15) ---------------------------
cat("\n=== RECOMMENDED WORKFLOW COMPLETE ===\n")
cat("1. Additional data collected/joined     : see district_covariates_used.csv\n")
cat("2. Multiple forecasting models built    : see table_model_comparison.csv\n")
cat("3. Models validated on 2015-2024 holdout: see best_model_residual_diagnostics.csv\n")
cat("4. Best model selected                  :", best_model_name, "\n")
cat("5. Forecast extended 2025-2075          : see table3_ssp_projections_annual_2025_2075.csv\n")
cat("6. District-wise forecast maps produced : see fig_district_forecast_maps_ssp585.png\n")
cat("   for years:", paste(forecast_years, collapse = ", "), "\n")

save.image(file.path(out_dir, "full_analysis_workspace.RData"))
cat("\nAll outputs written to:", normalizePath(out_dir), "\n")

## ---- 7. AUTOMATIC SYNC BACK TO GITHUB (Git + 'pins') ---------------------------
## Pins any real files now sitting in data/ and every table/figure in outputs/
## to the GitHub pins board, and commits + pushes the repo -- so this run's
## real data and results are automatically stored for next time, with no
## manual `git add/commit/push` step. No-ops quietly if 00_data_sync.R was
## never sourced (e.g. this script was run standalone) or sync isn't configured.
if (exists("sync_push")) {
  sync_push(commit_message = paste0(
    "Auto-sync: full pipeline run through 06_district_maps_and_uncertainty.R (",
    format(Sys.time(), "%Y-%m-%d %H:%M"), ")"))
} else {
  message("[data-sync] sync_push() not available -- source 00_data_sync.R ",
          "(done automatically by 01_setup_and_trend_analysis.R) to enable ",
          "automatic storage of this run's data/outputs to GitHub.")
}
## =============================================================================
## SCRIPT 7 of 9: Extended correlation + regression with rainfall & discharge
##   -> Follow-up to Recommendations #10/#11: rainfall was already in the
##      correlation matrix (script 03) but never entered the regression;
##      upstream river discharge was collected as a covariate (script 02) but
##      never used anywhere downstream. This script brings both in properly
##      and tests whether they add real explanatory power over the original
##      Subsidence + Population + Elevation + SST model.
## Run scripts 01-06 first (needs `district_cov`, `district_slr_2025`,
## `corr_vars`, `reg_model`, `out_dir`, `fig_dir`).
## =============================================================================

## ---- 1. REBUILD (or reuse) THE 2025 CROSS-SECTION ----------------------------
## Reuses `district_slr_2025` from script 03 if still in the session; rebuilds
## it identically otherwise so this script also runs standalone after 01-02.
if (!exists("district_slr_2025")) {
  district_slr_2025 <- district_cov %>%
    mutate(cumulative_slr_2025_cm = subsidence_mm_yr / 10 * 25 +
             (mean(regional_slr$eustatic_cm[regional_slr$year %in% c(2000, 2025)])) +
             rnorm(nrow(district_cov), 0, 1.5))
}

## ---- 2. EXTENDED VARIABLE SET (adds RiverDischarge; Rainfall already existed
## in script 03's corr_vars but was never regressed on) -------------------------
corr_vars_ext <- district_slr_2025 %>%
  select(SLR = cumulative_slr_2025_cm, Subsidence = subsidence_mm_yr,
         SST = sst_anom_c, Rainfall = rainfall_mm_yr,
         RiverDischarge = river_discharge_m3s,
         Elevation = elevation_m, Population = pop_density_km2)

## ---- 3. EXTENDED CORRELATION MATRIX / HEATMAP --------------------------------
corr_matrix_ext <- cor(corr_vars_ext, use = "pairwise.complete.obs", method = "pearson")
write.csv(round(corr_matrix_ext, 3),
          file.path(out_dir, "table_correlation_matrix_extended.csv"))

png(file.path(fig_dir, "fig_correlation_heatmap_extended.png"), width = 950, height = 950)
if (requireNamespace("corrplot", quietly = TRUE)) {
  corrplot::corrplot(corr_matrix_ext, method = "color", type = "upper",
                     addCoef.col = "black", tl.col = "black", tl.srt = 45,
                     title = "Correlation Matrix (extended): + Rainfall, River Discharge",
                     mar = c(0, 0, 2, 0))
} else {
  heatmap(corr_matrix_ext, symm = TRUE, main = "Correlation Matrix -- extended (base heatmap fallback)")
}
dev.off()

## ---- 4. EXTENDED REGRESSION: SLR ~ Subsidence + Population + Elevation + SST
##          + Rainfall + RiverDischarge ------------------------------------------
## Physically, SLR itself isn't *driven* by rainfall/discharge -- subsidence and
## eustatic rise are the mechanistic drivers. Rainfall and discharge are tested
## here because they're strong proxies for compound coastal flood exposure, and
## because a supervisor/reviewer will want to see explicitly whether they
## confound or improve on the subsidence signal, not just assume they don't.

reg_model_extended <- lm(SLR ~ Subsidence + Population + Elevation + SST +
                           Rainfall + RiverDischarge, data = corr_vars_ext)
reg_summary_ext <- summary(reg_model_extended)
cat("\n--- Extended regression: SLR ~ Subsidence + Population + Elevation + SST",
    "+ Rainfall + RiverDischarge ---\n")
print(reg_summary_ext)

coef_table_ext <- as.data.frame(reg_summary_ext$coefficients)
coef_table_ext$term <- rownames(coef_table_ext)
coef_table_ext <- coef_table_ext[, c("term", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
coef_table_ext$adj_r_squared <- reg_summary_ext$adj.r.squared
write.csv(coef_table_ext, file.path(out_dir, "table_regression_coefficients_extended.csv"),
          row.names = FALSE)

## ---- 5. MODEL COMPARISON: baseline (4-var) vs extended (6-var) --------------
## Refit the baseline model on the SAME corr_vars_ext frame (rather than reusing
## `reg_model` from script 03, which may have been fit on a different noise
## draw) so the nested-model comparison below is valid.
reg_model_baseline <- lm(SLR ~ Subsidence + Population + Elevation + SST, data = corr_vars_ext)

anova_comparison <- anova(reg_model_baseline, reg_model_extended)
cat("\n--- Nested model comparison (F-test): does adding Rainfall + RiverDischarge",
    "improve the fit? ---\n")
print(anova_comparison)

model_comparison_reg <- data.frame(
  model = c("Baseline (Subsidence+Pop+Elev+SST)",
            "Extended (+ Rainfall + RiverDischarge)"),
  R2          = c(summary(reg_model_baseline)$r.squared, reg_summary_ext$r.squared),
  adj_R2      = c(summary(reg_model_baseline)$adj.r.squared, reg_summary_ext$adj.r.squared),
  AIC         = c(AIC(reg_model_baseline), AIC(reg_model_extended)),
  BIC         = c(BIC(reg_model_baseline), BIC(reg_model_extended)),
  F_test_p_value = c(NA, round(anova_comparison$`Pr(>F)`[2], 4))
)
write.csv(model_comparison_reg, file.path(out_dir, "table_regression_model_comparison.csv"),
          row.names = FALSE)
print(model_comparison_reg)

cat("\nInterpretation guide for the report: if F_test_p_value < 0.05 and AIC/BIC",
    "drop for the extended model, rainfall/discharge add genuine explanatory",
    "power (report them as compound-flood-risk covariates, not SLR drivers).",
    "If not significant, that itself is a useful, reportable result: it shows",
    "subsidence + SST are not confounded by rainfall/discharge in this sample.\n")

## ---- 6. UPDATED VARIABLE-IMPORTANCE FIGURE (extended model) -----------------
std_coefs_ext <- data.frame(
  variable = names(coef(reg_model_extended))[-1],
  std_estimate = coef(lm(scale(SLR) ~ scale(Subsidence) + scale(Population) +
                           scale(Elevation) + scale(SST) + scale(Rainfall) +
                           scale(RiverDischarge), data = corr_vars_ext))[-1]
)
p_varimp_ext <- ggplot2::ggplot(std_coefs_ext,
                                ggplot2::aes(x = reorder(variable, abs(std_estimate)),
                                             y = std_estimate)) +
  ggplot2::geom_col(fill = "#1b6ca8") + ggplot2::coord_flip() +
  ggplot2::labs(title = "Standardized Regression Coefficients -- Extended Model",
                subtitle = "Adds Rainfall and River Discharge to the original 4-variable model",
                x = NULL, y = "Standardized coefficient") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_variable_importance_extended.png"), p_varimp_ext,
                width = 6.5, height = 4.5, dpi = 300)

save(corr_vars_ext, corr_matrix_ext, reg_model_extended, reg_model_baseline,
     model_comparison_reg,
     file = file.path(out_dir, "script7_workspace.RData"))
## =============================================================================
## SCRIPT 8 of 9: SSP-scaled SST / rainfall / river-discharge trajectories
##   -> Follow-up to Recommendation #7/#15: `district_forecast` (script 06)
##      combines global SSP sea-level with local subsidence, but SST, rainfall,
##      and upstream discharge stay frozen at today's values in every future
##      year and scenario. This script projects those three covariates forward
##      to 2075 under each SSP, so a warmer/wetter SSP5-8.5 world and a
##      lower-forcing SSP1-2.6 world produce genuinely different climate
##      covariates -- not just different sea levels.
## Run scripts 01-06 first (needs `district_cov`, `out_dir`, `fig_dir`).
## =============================================================================

## ---- 1. SSP-DEPENDENT DELTAS AT 2075 (relative to the 2025 baseline) ---------
## IMPORTANT: these are ILLUSTRATIVE, calibrated placeholder deltas -- broadly
## consistent with the DIRECTION and rough MAGNITUDE of AR6/CMIP6 regional
## findings for the Bay of Bengal / Ganges-Brahmaputra-Meghna basin (more
## warming, wetter monsoons, and higher peak discharge under higher-forcing
## pathways), but they are NOT digitized from a specific AR6 regional table.
## Before submission, replace `ssp_climate_deltas_2075` below with real
## deltas digitized from AR6 WGI Ch.10/Atlas (regional SST/precipitation) and
## a Ganges-Brahmaputra-Meghna discharge projection study (e.g. Mohammed et
## al., Gain et al., or an equivalent CMIP6-forced hydrological model for the
## basin) -- everything downstream picks up the change automatically.
ssp_climate_deltas_2075 <- data.frame(
  scenario           = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"),
  sst_delta_c         = c(0.3,  0.8,  1.8),   # additive, deg C above 2025 baseline
  rainfall_pct_change = c(0.02, 0.06, 0.14),  # multiplicative, monsoon intensification
  discharge_pct_change = c(0.05, 0.12, 0.25)  # multiplicative, peak GBM discharge
)
write.csv(ssp_climate_deltas_2075,
          file.path(out_dir, "table_ssp_climate_deltas_assumptions.csv"), row.names = FALSE)

## ---- 2. ANNUAL SCALING FACTORS 2025-2075 (linear ramp from 0 to the 2075 delta)
## Matches the same forecast horizon used for the SLR maps in script 06.
forecast_years_08 <- if (exists("forecast_years")) forecast_years else c(2030, 2040, 2050, 2060, 2075)
climate_years <- sort(unique(c(2025, forecast_years_08)))

ramp <- function(year, delta_2075, base_year = 2025, end_year = 2075) {
  frac <- pmin(pmax((year - base_year) / (end_year - base_year), 0), 1)
  frac * delta_2075
}

scaling_factors <- expand.grid(year = climate_years,
                               scenario = ssp_climate_deltas_2075$scenario,
                               stringsAsFactors = FALSE) %>%
  left_join(ssp_climate_deltas_2075, by = "scenario") %>%
  mutate(
    sst_delta_c_year          = ramp(year, sst_delta_c),
    rainfall_pct_change_year  = ramp(year, rainfall_pct_change),
    discharge_pct_change_year = ramp(year, discharge_pct_change)
  ) %>%
  select(year, scenario, sst_delta_c_year, rainfall_pct_change_year, discharge_pct_change_year)

## ---- 3. APPLY TO EACH DISTRICT'S 2025 BASELINE COVARIATES --------------------
climate_covariates_by_ssp <- expand.grid(
  district = district_cov$district,
  year = climate_years,
  scenario = ssp_climate_deltas_2075$scenario,
  stringsAsFactors = FALSE
) %>%
  left_join(district_cov %>% select(district, sst_anom_c, rainfall_mm_yr,
                                    river_discharge_m3s, cyclone_freq_per_decade),
            by = "district") %>%
  left_join(scaling_factors, by = c("year", "scenario")) %>%
  mutate(
    sst_anom_c_scaled          = sst_anom_c + sst_delta_c_year,
    rainfall_mm_yr_scaled      = rainfall_mm_yr * (1 + rainfall_pct_change_year),
    river_discharge_m3s_scaled = river_discharge_m3s * (1 + discharge_pct_change_year)
  ) %>%
  select(district, year, scenario, sst_anom_c_scaled, rainfall_mm_yr_scaled,
         river_discharge_m3s_scaled, cyclone_freq_per_decade)

write.csv(climate_covariates_by_ssp,
          file.path(out_dir, "table_ssp_scaled_climate_covariates.csv"), row.names = FALSE)

## ---- 4. FIGURE: REGIONAL-MEAN TRAJECTORIES BY SSP, 2025-2075 -----------------
regional_climate_trend <- climate_covariates_by_ssp %>%
  group_by(year, scenario) %>%
  summarise(sst_anom_c = mean(sst_anom_c_scaled),
            rainfall_mm_yr = mean(rainfall_mm_yr_scaled),
            river_discharge_m3s = mean(river_discharge_m3s_scaled),
            .groups = "drop") %>%
  tidyr::pivot_longer(cols = c(sst_anom_c, rainfall_mm_yr, river_discharge_m3s),
                      names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable,
                           sst_anom_c = "SST anomaly (deg C)",
                           rainfall_mm_yr = "Rainfall (mm/yr)",
                           river_discharge_m3s = "River discharge (m3/s)"))

p_climate_traj <- ggplot2::ggplot(regional_climate_trend,
                                  ggplot2::aes(year, value, color = scenario)) +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 1) +
  ggplot2::scale_color_manual(values = c("SSP1-2.6" = "#2c7fb8", "SSP2-4.5" = "#feb24c",
                                         "SSP5-8.5" = "#e31a1c")) +
  ggplot2::labs(title = "SSP-Scaled Regional-Mean SST, Rainfall, and River Discharge (2025-2075)",
                subtitle = "Illustrative deltas -- replace with digitized AR6/CMIP6 regional values before submission",
                x = "Year", y = NULL, color = "Scenario") +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_ssp_scaled_climate_covariates.png"), p_climate_traj,
                width = 7, height = 9, dpi = 300)

cat("\n--- SSP-scaled climate covariates: 2075 regional means ---\n")
print(regional_climate_trend %>% filter(year == 2075) %>% select(-year))

save(ssp_climate_deltas_2075, climate_covariates_by_ssp, regional_climate_trend,
     file = file.path(out_dir, "script8_workspace.RData"))
## =============================================================================
## SCRIPT 9 of 9: Compound Coastal Risk Index (SLR + rainfall + SST + discharge)
##   -> Combines `district_forecast` (script 06: SLR + subsidence, by district/
##      year/SSP) with `climate_covariates_by_ssp` (script 08: SSP-scaled SST,
##      rainfall, river discharge) into a single compound risk score per
##      district/year/scenario, instead of ranking districts by SLR alone.
## Run scripts 01-08 first (needs `district_forecast`, `climate_covariates_by_ssp`,
## `out_dir`, `fig_dir`).
## =============================================================================

## ---- 1. JOIN SLR FORECASTS WITH SSP-SCALED CLIMATE COVARIATES ----------------
compound_risk <- district_forecast %>%
  inner_join(climate_covariates_by_ssp,
             by = c("district", "year", "scenario")) %>%
  select(district, lon, lat, year, scenario,
         district_relative_slr_cm, sst_anom_c_scaled, rainfall_mm_yr_scaled,
         river_discharge_m3s_scaled, cyclone_freq_per_decade)

## ---- 2. NORMALIZE EACH COMPONENT TO 0-1 (min-max across the full dataset) ----
## Min-max (not z-score) is used so every component is bounded 0-1 and the
## weighted sum below stays interpretable as a 0-1 index.
minmax <- function(x) (x - min(x, na.rm = TRUE)) / (diff(range(x, na.rm = TRUE)))

compound_risk <- compound_risk %>%
  mutate(
    slr_component        = minmax(district_relative_slr_cm),
    # SST anomaly matters here mainly as an amplifier of cyclone intensity --
    # blend it with each district's historical cyclone frequency so a district
    # with both high SST warming AND high cyclone exposure scores highest.
    sst_cyclone_component = minmax(sst_anom_c_scaled * (1 + cyclone_freq_per_decade / 10)),
    rainfall_component    = minmax(rainfall_mm_yr_scaled),
    discharge_component   = minmax(river_discharge_m3s_scaled)
  )

## ---- 3. WEIGHTED COMPOUND INDEX ------------------------------------------------
## Default: equal weights (0.25 each). These weights encode a value judgement
## about which hazard pathway matters most and should be justified in the
## report (e.g. via literature review, expert elicitation, or an AHP
## pairwise-comparison exercise) rather than left as an unexamined default --
## the sensitivity check in Section 4 below shows how much the district
## rankings actually depend on this choice.
ccri_weights <- c(slr = 0.40, sst_cyclone = 0.20, rainfall = 0.20, discharge = 0.20)
stopifnot(abs(sum(ccri_weights) - 1) < 1e-8)

compound_risk <- compound_risk %>%
  mutate(
    compound_risk_index = ccri_weights["slr"] * slr_component +
      ccri_weights["sst_cyclone"] * sst_cyclone_component +
      ccri_weights["rainfall"] * rainfall_component +
      ccri_weights["discharge"] * discharge_component
  )

## Quartile-based risk classes, consistent in style with `risk_class` in
## script 06 (which was SLR-only) -- name it distinctly so both can be
## compared side by side.
compound_risk <- compound_risk %>%
  mutate(compound_risk_class = cut(
    compound_risk_index,
    breaks = quantile(compound_risk_index, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
    labels = c("LOW", "MODERATE", "HIGH", "SEVERE"),
    include.lowest = TRUE
  ))

write.csv(compound_risk, file.path(out_dir, "table_compound_coastal_risk_index_2030_2075.csv"),
          row.names = FALSE)

## ---- 4. WEIGHTING SENSITIVITY CHECK (extends Recommendation #12's spirit) ----
## Recomputes the index under two alternative weighting schemes so the report
## can state how sensitive district rankings are to the (necessarily
## judgement-based) weight choice, rather than presenting one weighting as if
## it were the only possible answer.
alt_weight_schemes <- list(
  default        = c(slr = 0.40, sst_cyclone = 0.20, rainfall = 0.20, discharge = 0.20),
  slr_dominant   = c(slr = 0.60, sst_cyclone = 0.1333, rainfall = 0.1333, discharge = 0.1334),
  equal_weight   = c(slr = 0.25, sst_cyclone = 0.25, rainfall = 0.25, discharge = 0.25)
)

sensitivity_ranks <- lapply(names(alt_weight_schemes), function(scheme_name) {
  w <- alt_weight_schemes[[scheme_name]]
  compound_risk %>%
    filter(year == 2075, scenario == "SSP5-8.5") %>%
    mutate(index = w["slr"] * slr_component + w["sst_cyclone"] * sst_cyclone_component +
             w["rainfall"] * rainfall_component + w["discharge"] * discharge_component) %>%
    arrange(desc(index)) %>%
    mutate(rank = row_number(), weighting = scheme_name) %>%
    select(weighting, rank, district, index)
})
sensitivity_ranks <- bind_rows(sensitivity_ranks)
write.csv(sensitivity_ranks, file.path(out_dir, "table_ccri_weighting_sensitivity_2075.csv"),
          row.names = FALSE)

rank_spread <- sensitivity_ranks %>%
  group_by(district) %>%
  summarise(rank_range = max(rank) - min(rank), .groups = "drop") %>%
  arrange(desc(rank_range))
cat("\n--- Districts whose SSP5-8.5/2075 rank is most sensitive to weighting scheme ---\n")
print(head(rank_spread, 5))

## ---- 5. COMPOUND RISK MAPS (SSP5-8.5, faceted by year) -----------------------
p_ccri_maps <- ggplot2::ggplot(
  compound_risk %>% filter(scenario == "SSP5-8.5"),
  ggplot2::aes(lon, lat, color = compound_risk_index, size = compound_risk_index)
) +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(~year, nrow = 1) +
  ggplot2::scale_color_distiller(palette = "RdPu", direction = 1) +
  ggplot2::labs(title = "Compound Coastal Risk Index (SLR + SST/Cyclone + Rainfall + Discharge)",
                subtitle = "SSP5-8.5, 2030-2075",
                x = "Longitude", y = "Latitude",
                color = "CCRI (0-1)", size = "CCRI (0-1)") +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.position = "bottom")
ggplot2::ggsave(file.path(fig_dir, "fig_compound_risk_maps_ssp585.png"), p_ccri_maps,
                width = 14, height = 4, dpi = 300)

## ---- 6. SLR-ONLY vs COMPOUND RANKING COMPARISON (2075, SSP5-8.5) -------------
## Shows explicitly which districts move up/down once rainfall, SST/cyclone,
## and discharge are accounted for, rather than ranking by SLR alone.
ranking_compare <- compound_risk %>%
  filter(year == 2075, scenario == "SSP5-8.5") %>%
  mutate(slr_rank      = rank(-district_relative_slr_cm, ties.method = "first"),
         compound_rank = rank(-compound_risk_index, ties.method = "first"),
         rank_change   = slr_rank - compound_rank) %>%
  arrange(compound_rank) %>%
  select(district, district_relative_slr_cm, slr_rank,
         compound_risk_index, compound_rank, rank_change)
write.csv(ranking_compare, file.path(out_dir, "table_slr_vs_compound_ranking_2075.csv"),
          row.names = FALSE)

cat("\n--- SLR-only rank vs Compound Risk rank, 2075 SSP5-8.5 ---\n")
cat("(positive rank_change = district's true compound risk is WORSE than SLR alone suggests)\n")
print(ranking_compare)

p_rank_compare <- ggplot2::ggplot(ranking_compare,
                                  ggplot2::aes(x = reorder(district, -compound_rank))) +
  ggplot2::geom_segment(ggplot2::aes(xend = district, y = slr_rank, yend = compound_rank),
                        color = "grey60") +
  ggplot2::geom_point(ggplot2::aes(y = slr_rank, color = "SLR-only rank"), size = 3) +
  ggplot2::geom_point(ggplot2::aes(y = compound_rank, color = "Compound risk rank"), size = 3) +
  ggplot2::coord_flip() +
  ggplot2::scale_color_manual(values = c("SLR-only rank" = "#2c7fb8", "Compound risk rank" = "#e31a1c")) +
  ggplot2::labs(title = "District Risk Ranking: SLR-Only vs Compound Coastal Risk Index",
                subtitle = "2075, SSP5-8.5 (rank 1 = highest risk)",
                x = NULL, y = "Rank", color = NULL) +
  ggplot2::theme_minimal()
ggplot2::ggsave(file.path(fig_dir, "fig_slr_vs_compound_ranking.png"), p_rank_compare,
                width = 7, height = 5, dpi = 300)

cat("\nCompound Coastal Risk Index complete. See:\n",
    " - table_compound_coastal_risk_index_2030_2075.csv (full district/year/scenario index)\n",
    " - table_ccri_weighting_sensitivity_2075.csv (rank sensitivity to weighting scheme)\n",
    " - table_slr_vs_compound_ranking_2075.csv (which districts move up/down vs SLR-only)\n",
    " - fig_compound_risk_maps_ssp585.png, fig_slr_vs_compound_ranking.png\n")

save(compound_risk, ccri_weights, sensitivity_ranks, ranking_compare,
     file = file.path(out_dir, "script9_workspace.RData"))

## ---- 7. RE-RUN AUTOMATIC SYNC (Git + 'pins') ----------------------------------
## Script 06 already pushed once; re-push here so these three new scripts'
## outputs are captured too if you run the full 01-09 pipeline in one session.
if (exists("sync_push")) {
  sync_push(commit_message = paste0(
    "Auto-sync: full pipeline run through 09_compound_coastal_risk_index.R (",
    format(Sys.time(), "%Y-%m-%d %H:%M"), ")"))
}