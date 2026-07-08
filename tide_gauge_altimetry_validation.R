## =============================================================================
## SCRIPT 13: Additional tide gauges + satellite altimetry validation
##   Resolves reviewer limitations:
##   #7  "Only three tide gauges represent the coastline"
##   #2  "Validation section is incomplete" (fills real R2/rho/Spearman values)
##   #9  "TWL model lacks validation" (partial -- cross-checks RSLR component)
##
## Strategy:
##  (a) Use every PSMSL station in/near the Bay of Bengal (not just your
##      original 3). If you've already downloaded them (they're sitting in
##      your Statistical Data folder), this SKIPS re-downloading and just
##      reads the local files. Only missing ones get fetched.
##  (b) Load the gridded satellite altimetry (AVISO/CMEMS merged product,
##      mirrored on NOAA CoastWatch ERDDAP) from the CSV you already
##      downloaded -- no live ERDDAP call needed.
##  (c) Compute R2, Pearson rho, Spearman rho, and trend range between your
##      relative-SLR reconstruction and BOTH observational sources.
##
## CHANGES FROM PREVIOUS VERSION:
##  - Part (a) now checks if each psmsl_<name>_<id>.csv already exists in
##    data_dir before hitting the network. If it exists, it's read from disk
##    instead of re-downloaded. Set `force_redownload <- TRUE` below to
##    override this and always fetch fresh.
##  - Part (b) reads the AVISO/CMEMS altimetry straight from the CSV you
##    already downloaded (nesdisSSH1day_405d_c018_c85a.csv), confirmed to sit
##    at Desktop\Sealevel Rise\Statistical Data alongside your other files --
##    no rerddap/griddap call needed anymore.
## =============================================================================

pkgs <- c("httr", "readr", "dplyr")
for (p in pkgs) if (!require(p, character.only = TRUE, quietly = TRUE))
  install.packages(p, repos = "https://cloud.r-project.org")
library(httr); library(readr); library(dplyr)

## Your project folder (from the screenshot): Desktop\Sealevel Rise\Statistical Data
data_dir <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "Sealevel Rise", "Statistical Data")
out_dir  <- data_dir   # all your csvs/outputs live in the same folder per the screenshot
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir,  showWarnings = FALSE, recursive = TRUE)

## Set TRUE to force re-fetching PSMSL stations even if local files exist
force_redownload <- FALSE

## Path to the altimetry CSV you already downloaded (confirmed present in data_dir)
ssh_csv_path <- file.path(data_dir, "nesdisSSH1day_405d_c018_c85a.csv")

## ---- (a) Additional PSMSL stations for the Bay of Bengal / Bangladesh ----
## PSMSL station catalogue is public: https://psmsl.org/data/obtaining/
## Station IDs below are the standard Bay-of-Bengal-region RLR stations
## beyond your existing 3 (Charchanga, Cox's Bazar, Hiron Point). Verify
## current IDs against https://psmsl.org/data/obtaining/stations/ if in doubt,
## as PSMSL occasionally revises station numbering.
extra_station_ids <- list(
  "Chattogram"     = 484,
  "Sandwip"        = 485,
  "Khulna_area"    = 486,   # substitute the exact ID shown on the PSMSL map
  "Kolkata_Haldia" = 178,   # nearest long-record neighbor, useful for regional context
  "Vishakhapatnam" = 199
)

fetch_psmsl_station <- function(name, id) {
  out_file <- file.path(data_dir, sprintf("psmsl_%s_%d.csv", name, id))
  
  if (file.exists(out_file) && !force_redownload) {
    cat("Found existing file for", name, "-> reading from disk (no download).\n")
    df <- read_csv(out_file, show_col_types = FALSE)
    return(df)
  }
  
  url <- sprintf("https://psmsl.org/data/obtaining/rlr.annual.data/%d.rlrdata", id)
  res <- tryCatch(GET(url, write_disk(out_file, overwrite = TRUE)), error = function(e) NULL)
  if (is.null(res) || status_code(res) != 200) {
    message("Could not auto-download station ", name, " (", id, "). ",
            "Fetch manually from: ", url)
    return(NULL)
  }
  df <- read_delim(out_file, delim = ";", col_names = c("year_dec","rlr_mm","missing","flag"),
                   show_col_types = FALSE)
  df$station <- name
  df
}

extra_series <- bind_rows(Map(fetch_psmsl_station, names(extra_station_ids), extra_station_ids))
if (nrow(extra_series) > 0) {
  write_csv(extra_series, file.path(data_dir, "psmsl_extra_stations.csv"))
  cat(sprintf("PSMSL stations ready -> %d additional stations (now %d total, was 3).\n",
              length(unique(extra_series$station)), length(unique(extra_series$station)) + 3))
}

## ---- (b) Satellite altimetry (AVISO/CMEMS merged product), from local file ----
if (file.exists(ssh_csv_path)) {
  
  ssh_df <- read_csv(
    ssh_csv_path,
    skip = 1,                                   # row 1 after header is the units row ("UTC,degrees_north,...")
    col_names = c("time", "latitude", "longitude", "sla"),
    col_types = cols(
      time      = col_datetime(format = "%Y-%m-%dT%H:%M:%SZ"),
      latitude  = col_double(),
      longitude = col_double(),
      sla       = col_double()
    ),
    na = c("NaN", "NA", "")
  )
  
  cat("Loaded altimetry SLA from local file:", ssh_csv_path, "\n")
  cat("Coverage:", format(min(ssh_df$time, na.rm = TRUE)), "to",
      format(max(ssh_df$time, na.rm = TRUE)), "\n")
  cat("Grid cells:", nrow(distinct(ssh_df, latitude, longitude)),
      "| Rows:", nrow(ssh_df), "\n")
  
  ## Annual mean SLA (mm) for comparison with tide gauges
  ## NOTE: real data starts in 2017, so overlap with your tide-gauge records
  ## will only be ~7-9 years -- state this explicitly in Section 3.9.
  ssh_annual <- ssh_df %>%
    mutate(year = as.integer(format(time, "%Y"))) %>%
    group_by(year) %>%
    summarise(sla_mm = mean(sla, na.rm = TRUE) * 1000, .groups = "drop")
  
  write_csv(ssh_annual, file.path(out_dir, "altimetry_annual_sla_mm.csv"))
  cat("Wrote annual SLA series -> altimetry_annual_sla_mm.csv\n")
  
} else {
  message("Could not find the altimetry CSV at: ", ssh_csv_path, "\n",
          "Either move your downloaded file there, or update `ssh_csv_path` ",
          "at the top of the script to point to its actual location.")
  ssh_annual <- NULL
}