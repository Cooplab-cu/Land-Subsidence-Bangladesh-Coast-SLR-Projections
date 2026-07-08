###############################################################################
# FIX: Synthetic DEM units bug causing all-green / blank flood & SLR maps
#
# ROOT CAUSE (confirmed):
#   Every synthetic DEM fallback in this project (Part 1 of the big pipeline,
#   and Section 4 of the standalone Excel-based script) generates elevations
#   in the range of ~0-300 metres using formulas like:
#       abs(lon - 90.5) * 50 + abs(lat - 21.5) * 30
#   But every flood/inundation/SLR-risk raster comparison in the same scripts
#   compares that DEM against SLR thresholds of ~0.08-0.35 METRES (8-35 cm).
#   A DEM with a 0-300m range will NEVER satisfy "dem <= 0.35", so:
#     - terra::ifel(dem_raster <= slr_threshold_m, ...) is FALSE everywhere
#     - every flood/inundation raster comes back as "NO RISK" / green
#     - this happens whenever SRTM download fails and the synthetic
#       fallback DEM is used (e.g. no internet access in the R session)
#
# THIS ONLY AFFECTS THE RASTER OVERLAY. The district polygon risk colours
# (computed from the Excel SLR table, not the DEM) are usually fine on their
# own -- but the raster sits on top and at alpha=0.7-0.85 it visually washes
# out the polygon colour underneath, making the whole map look uniform green.
#
# FIX STRATEGY:
#   1. Replace the synthetic DEM generator with one that actually produces
#      a believable low-lying delta coastline (0-15m, matching real
#      Bangladesh coastal topography) instead of a 0-300m random surface.
#   2. Make every "dem <= threshold" comparison unit-safe and add a sanity
#      check that warns loudly (instead of failing silently) if the DEM's
#      elevation range can never intersect the SLR threshold range.
#   3. Provide a corrected make_slr_map() for the standalone script that
#      drops the raster overlay entirely when the DEM is synthetic/unreliable
#      and relies on the (correct) polygon risk colours instead -- this
#      mirrors the fix the original PDF script's Section 11 already applied
#      for the 2050 map, but generalised to the other 26 annual maps too.
###############################################################################

suppressWarnings(suppressMessages({
  library(terra)
}))

EXTENT <- c(xmin = 88.80, xmax = 92.60, ymin = 20.50, ymax = 23.50)

# ─────────────────────────────────────────────────────────────────────────
# FIX 1: Realistic synthetic DEM (replaces both broken fallbacks)
# ─────────────────────────────────────────────────────────────────────────
# Bangladesh's coastal belt is genuinely low-lying: 0-1m at the immediate
# coast/Sundarbans, rising gradually to 8-12m further inland/north-east
# (Chittagong hills excluded -- this is the delta/coastal strip only).
# This matches real SRTM behaviour for this exact study area, so SLR
# thresholds of 0.08-0.35m will correctly intersect a meaningful fraction
# of cells, instead of 0% of them.

make_realistic_synthetic_dem <- function(extent = EXTENT, resolution = 0.01, seed = 2024) {
  set.seed(seed)
  r <- terra::rast(
    xmin = extent["xmin"], xmax = extent["xmax"],
    ymin = extent["ymin"], ymax = extent["ymax"],
    resolution = resolution, crs = "EPSG:4326"
  )
  xy <- terra::xyFromCell(r, 1:terra::ncell(r))

  # Distance from the coastline (south edge) drives elevation gain.
  # South (low lat) = coast = near 0m. North = inland = up to ~12m.
  # This is a LOW-LYING DELTA PROFILE, not a generic random surface.
  lat_norm <- (xy[, 2] - extent["ymin"]) / (extent["ymax"] - extent["ymin"])  # 0 (south/coast) -> 1 (north/inland)

  # Base elevation: 0m at coast, ramps to ~12m inland (realistic for this delta)
  base_elev <- lat_norm * 12

  # Add small east-west variation (Chittagong side is slightly higher)
  ew_effect <- pmax(0, (xy[, 1] - 90.5)) * 1.5

  # Local noise (small, realistic - not the +/-50-300m noise used before)
  noise <- rnorm(nrow(xy), 0, 0.8)

  elev <- pmax(0, base_elev + ew_effect + noise)
  terra::values(r) <- elev

  cat("  Realistic synthetic DEM created (replaces broken 0-300m fallback)\n")
  cat(sprintf("    Min: %.2f m | Max: %.2f m | Mean: %.2f m\n",
              min(terra::values(r), na.rm = TRUE),
              max(terra::values(r), na.rm = TRUE),
              mean(terra::values(r), na.rm = TRUE)))
  cat(sprintf("    Cells <= 0.35m (typical SLR threshold): %.1f%%  <- should be > 0%% now\n",
              100 * mean(terra::values(r, na.rm = TRUE) <= 0.35)))
  r
}

# ─────────────────────────────────────────────────────────────────────────
# FIX 2: Sanity-check helper -- call this right after ANY DEM is loaded
# (real SRTM or synthetic) before it is used for flood/inundation rasters.
# ─────────────────────────────────────────────────────────────────────────
sanity_check_dem_vs_threshold <- function(dem_raster, thresholds_m, dem_name = "dem_raster") {
  vals <- terra::values(dem_raster, na.rm = TRUE)
  dem_min <- min(vals); dem_max <- max(vals)
  max_thresh <- max(thresholds_m)

  cat(sprintf("\n  [DEM SANITY CHECK] %s range: %.2f m to %.2f m | thresholds tested: %s m\n",
              dem_name, dem_min, dem_max, paste(round(thresholds_m, 2), collapse = ", ")))

  pct_below <- sapply(thresholds_m, function(t) 100 * mean(vals <= t))
  for (i in seq_along(thresholds_m)) {
    cat(sprintf("    Threshold %.2fm -> %.2f%% of DEM cells qualify\n",
                thresholds_m[i], pct_below[i]))
  }

  if (all(pct_below < 0.01)) {
    warning(sprintf(
      paste0(
        "\n  *** DEM/THRESHOLD MISMATCH DETECTED ***\n",
        "  %s spans %.1f-%.1fm but ALL flood thresholds are <= %.2fm.\n",
        "  Essentially 0%% of cells will ever register as flooded -- every\n",
        "  output map will render as uniform 'NO RISK' / green regardless\n",
        "  of the actual SLR data. This usually means:\n",
        "    (a) the DEM is the broken synthetic fallback (SRTM download\n",
        "        failed silently), or\n",
        "    (b) there is a units mismatch (DEM in some other unit vs\n",
        "        thresholds assumed to be metres).\n",
        "  ACTION: switch to make_realistic_synthetic_dem() or verify the\n",
        "  real SRTM tiles downloaded successfully before proceeding.\n"
      ),
      dem_name, dem_min, dem_max, max_thresh
    ), call. = FALSE)
    return(FALSE)
  }
  cat("  [OK] DEM range plausibly intersects the flood thresholds.\n")
  TRUE
}

# ─────────────────────────────────────────────────────────────────────────
# FIX 3: Corrected make_slr_map() for the standalone Excel-based script
# ─────────────────────────────────────────────────────────────────────────
# Same signature/behaviour as the original, EXCEPT:
#   - it sanity-checks the DEM before trusting it for the raster overlay
#   - if the DEM fails the sanity check, it SKIPS the raster overlay
#     entirely (rather than silently drawing a misleading all-green layer)
#     and relies purely on the polygon fill, which is already correct
#     because it comes straight from the Excel risk_category column.
make_slr_map_FIXED <- function(target_year, slr_data, district_shapes,
                                dem_raster = NULL, risk_colors, risk_labels,
                                extent = EXTENT, dem_is_trustworthy = NULL) {

  yr_data <- slr_data %>% dplyr::filter(year == target_year)
  map_sf  <- district_shapes %>% dplyr::left_join(yr_data, by = "join_key")

  # Warn (once per call) if the join produced unmatched / NA rows --
  # this is "Bug 2" from the diagnosis: blank grey polygons that can look
  # like missing maps.
  n_na <- sum(is.na(map_sf$risk_category))
  if (n_na > 0) {
    warning(sprintf(
      "  Year %d: %d/%d district polygons have NO matching SLR data after the join (showing grey). Check normalize_name()/join_key matching.",
      target_year, n_na, nrow(map_sf)
    ), call. = FALSE)
  }

  slr_threshold_m <- mean(yr_data$total_relative_slr_cm, na.rm = TRUE) / 100

  # Only build the raster overlay if the DEM has already passed the
  # sanity check (computed once outside the loop and passed in), OR
  # if not supplied, check it right here.
  if (is.null(dem_is_trustworthy)) {
    dem_is_trustworthy <- if (!is.null(dem_raster) && is.finite(slr_threshold_m)) {
      suppressWarnings(sanity_check_dem_vs_threshold(
        dem_raster, c(slr_threshold_m, slr_threshold_m * 2, slr_threshold_m * 3),
        dem_name = paste0("dem_raster (year ", target_year, ")")
      ))
    } else FALSE
  }

  flood_df <- NULL
  if (dem_is_trustworthy && !is.null(dem_raster) && is.finite(slr_threshold_m)) {
    flood_rast <- terra::ifel(dem_raster <= slr_threshold_m, 4L,
                  terra::ifel(dem_raster <= slr_threshold_m * 2, 3L,
                  terra::ifel(dem_raster <= slr_threshold_m * 3, 2L, 1L)))
    flood_df <- as.data.frame(flood_rast, xy = TRUE, na.rm = TRUE)
    names(flood_df)[3] <- "risk_level"
    flood_df$risk_category <- factor(
      c("NO RISK", "LOW", "MODERATE", "HIGH")[flood_df$risk_level],
      levels = c("HIGH", "MODERATE", "LOW", "NO RISK")
    )
  }
  # else: flood_df stays NULL -> map_fn below skips the raster layer
  # and relies entirely on the (correct) polygon fill from Excel data.

  list(yr_data = yr_data, map_sf = map_sf, flood_df = flood_df,
       slr_threshold_m = slr_threshold_m, dem_is_trustworthy = dem_is_trustworthy)
}

cat("FIX MODULE LOADED.\n")
cat("Functions available:\n")
cat("  - make_realistic_synthetic_dem()      : replaces the broken 0-300m fallback DEM\n")
cat("  - sanity_check_dem_vs_threshold()     : call right after loading ANY dem_raster\n")
cat("  - make_slr_map_FIXED()                : corrected map builder, skips bad raster overlays\n")
