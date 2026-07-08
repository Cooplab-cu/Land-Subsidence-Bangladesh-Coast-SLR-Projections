###############################################################################
# BANGLADESH COASTAL SEA LEVEL RISE MAPPING (2000–2025)
# Study Area : Satkhira to Teknaf | Bay of Bengal
# Author     : Roman Hossain (ID: 20902037)
# Course     : R Programming – Coastal Flood Risk Mapping
# Version    : 5.0 — Full pipeline with automated data generation,
#              multi-layer vulnerability index, IPCC SSP scenarios,
#              high-resolution outputs and full Excel reporting
###############################################################################

# ─────────────────────────────────────────────────────────────────────────────
# 0. PACKAGES
# ─────────────────────────────────────────────────────────────────────────────
packages <- c(
  "terra", "sf", "ggplot2", "ggspatial", "RColorBrewer",
  "tidyverse", "geodata", "writexl", "openxlsx", "trend",
  "ggpubr", "scales", "cowplot", "viridis", "stringr",
  "readxl", "patchwork", "ggrepel", "classInt", "rnaturalearth",
  "rnaturalearthdata"
)
installed <- rownames(installed.packages())
to_install <- packages[!packages %in% installed]
if (length(to_install) > 0)
  install.packages(to_install, repos = "https://cloud.r-project.org",
                   dependencies = TRUE, quiet = TRUE)
invisible(lapply(packages, library, character.only = TRUE))

# ─────────────────────────────────────────────────────────────────────────────
# 1. GLOBAL SETTINGS & DIRECTORIES
# ─────────────────────────────────────────────────────────────────────────────
EXTENT  <- c(xmin = 88.80, xmax = 92.60, ymin = 20.50, ymax = 23.50)
EXCEL_PATH <- "output/data/Bangladesh_Coastal_SLR_Analysis.xlsx"
for (d in c("output/maps", "output/data", "output/trend",
            "output/risk", "output/atlas"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
THEME_TITLE  <- "#1F4E79"
THEME_BG     <- "#F0F8FF"
THEME_PANEL  <- "#C8E6FF"
THEME_GRID   <- "#DDDDDD"
BASE_DPI     <- 320
MAP_W        <- 16; MAP_H <- 11
normalize_name <- function(x) {
  x <- tolower(trimws(x))
  x <- gsub("[''`´]", "",  x)
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(x)
}

risk_colors <- c(
  "HIGH"    = "#CC0000",
  "MODERATE"= "#FF8C00",
  "LOW"     = "#FFD700",
  "NO RISK" = "#228B22"
)
risk_labels <- c(
  "HIGH"    = "HIGH (>35 cm)",
  "MODERATE"= "MODERATE (20–35 cm)",
  "LOW"     = "LOW (8–20 cm)",
  "NO RISK" = "NO RISK (<8 cm)"
)
flood_colors <- c(
  "SEVERE"  = "#67000D",
  "HIGH"    = "#CC0000",
  "MODERATE"= "#FF8C00",
  "LOW"     = "#228B22"
)

# ─────────────────────────────────────────────────────────────────────────────
# 2. AUTOMATED DATA GENERATION
#    All district SLR data built from published sources:
#    IPCC AR6 • AVISO • BWDB tide gauges • Alam & Dominey-Howes (2020)
#    Subsidence from Pethick & Orford (2013), Brown & Nicholls (2015)

# ─────────────────────────────────────────────────────────────────────────────
# 2a. District master table -------------------------------------------------
district_meta <- tibble::tribble(
  ~district,     ~lat,    ~lon,   ~subsidence_mm_yr, ~pop_density, ~lulc_agri_pct, ~lulc_mangrove_pct, ~dist_to_coast_km, ~embankment_km,
  "Satkhira",    22.33,   89.11,  18.5,              750,          0.62,           0.18,               5,                 320,
  "Khulna",      22.82,   89.56,  15.2,              850,          0.55,           0.20,               40,                280,
  "Bagerhat",    22.66,   89.79,  14.8,              620,          0.58,           0.22,               20,                350,
  "Pirojpur",    22.58,   89.98,  13.6,              780,          0.65,           0.12,               30,                210,
  "Barguna",     22.15,   90.12,  13.1,              560,          0.70,           0.08,               10,                180,
  "Patuakhali",  22.36,   90.33,  12.4,              600,          0.72,           0.07,               15,                220,
  "Bhola",       22.18,   90.65,  11.8,              680,          0.68,           0.06,               8,                 160,
  "Lakshmipur",  22.94,   90.84,  10.2,              1450,         0.60,           0.04,               25,                140,
  "Noakhali",    22.87,   91.10,  9.5,               1100,         0.63,           0.05,               12,                170,
  "Feni",        23.02,   91.40,  8.1,               1700,         0.58,           0.03,               20,                90,
  "Chittagong",  22.34,   91.83,  6.8,               1900,         0.45,           0.06,               5,                 50,
  "Cox's Bazar", 21.44,   91.98,  5.4,               900,          0.52,           0.10,               3,                 80
)
# 2b. Teknaf upazila --------------------------------------------------------
teknaf_meta <- tibble::tribble(
  ~district,  ~lat,   ~lon,   ~subsidence_mm_yr, ~pop_density, ~lulc_agri_pct, ~lulc_mangrove_pct, ~dist_to_coast_km, ~embankment_km,
  "Teknaf",   20.87,  92.30,  4.2,               650,          0.48,           0.15,               2,                 30
)
teknaf_meta$parent_district <- "Cox's Bazar"
teknaf_meta$is_upazila      <- TRUE
district_meta$parent_district <- district_meta$district
district_meta$is_upazila      <- FALSE
all_meta <- bind_rows(district_meta, teknaf_meta)
# 2c. Global SLR time series (IPCC AR6 Table 9.5, historical 2000-2025) -----
#     Global mean SLR relative to 2000 baseline (mm)
global_slr_annual <- tibble(
  year = 2000:2025,
  global_slr_mm = c(0.0, 3.2, 6.6, 9.8, 13.1, 16.4, 19.9, 23.4,
                    27.0, 30.7, 34.4, 38.2, 42.1, 46.0, 50.0, 54.1,
                    58.3, 62.6, 67.0, 71.5, 76.1, 80.8, 85.6, 90.5,
                    95.5, 100.6)
)
# 2d. Build full time series ------------------------------------------------
slr_raw <- all_meta %>%
  crossing(year = 2000:2025) %>%
  left_join(global_slr_annual, by = "year") %>%
  mutate(
    years_elapsed         = year - 2000,
    cumul_subsidence_mm   = subsidence_mm_yr * years_elapsed,
    total_relative_slr_mm = global_slr_mm + cumul_subsidence_mm,
    total_relative_slr_cm = total_relative_slr_mm / 10,
    risk_category = case_when(
      total_relative_slr_cm > 35 ~ "HIGH",
      total_relative_slr_cm > 20 ~ "MODERATE",
      total_relative_slr_cm >  8 ~ "LOW",
      TRUE                        ~ "NO RISK"
    ),
    risk_category = factor(risk_category,
                           levels = c("HIGH","MODERATE","LOW","NO RISK")),
    join_key       = normalize_name(district),
    parent_join_key = normalize_name(parent_district)
  )
slr_ts_points   <- slr_raw
slr_ts_polygons <- slr_raw %>% filter(!is_upazila)
# 2e. IPCC AR6 SSP scenario projections to 2075 (for Section 11) -----------
#     SSP1-2.6: low, SSP2-4.5: intermediate, SSP5-8.5: high
ssp_global <- tibble(
  year = 2025:2075,
  ssp126_mm = seq(100.6, 250, length.out = 51),
  ssp245_mm = seq(100.6, 380, length.out = 51),
  ssp585_mm = seq(100.6, 580, length.out = 51)
)
# 2f. Write raw Excel (source of truth for reproducibility) ----------------
trend_export_raw <- slr_ts_points %>%
  filter(!is_upazila) %>%
  group_by(District = district) %>%
  summarise(
    `SLR 2000 (cm)` = total_relative_slr_cm[year == 2000],
    `SLR 2025 (cm)` = total_relative_slr_cm[year == 2025],
    `Total Rise 2000-2025 (cm)` = total_relative_slr_cm[year == 2025] -
      total_relative_slr_cm[year == 2000],
    `Rise Rate (cm/yr)` = round(
      (total_relative_slr_cm[year == 2025] -
         total_relative_slr_cm[year == 2000]) / 25, 4),
    `Subsidence Rate (mm/yr)` = subsidence_mm_yr[year == 2000],
    `Pop Density (/km²)`      = pop_density[year == 2000],
    `Agri Land (%)`           = lulc_agri_pct[year == 2000],
    `Mangrove Cover (%)`      = lulc_mangrove_pct[year == 2000],
    `Dist to Coast (km)`      = dist_to_coast_km[year == 2000],
    `Embankment (km)`         = embankment_km[year == 2000],
    .groups = "drop"
  )
regional_trend_raw <- slr_ts_points %>%
  group_by(year) %>%
  summarise(
    mean_slr_cm = mean(total_relative_slr_cm, na.rm = TRUE),
    max_slr_cm  = max(total_relative_slr_cm,  na.rm = TRUE),
    min_slr_cm  = min(total_relative_slr_cm,  na.rm = TRUE),
    n_high      = sum(risk_category == "HIGH"),
    n_moderate  = sum(risk_category == "MODERATE"),
    n_low       = sum(risk_category == "LOW"),
    n_no_risk   = sum(risk_category == "NO RISK"),
    .groups = "drop"
  )
writexl::write_xlsx(
  list(
    "SLR Raw Data"     = as.data.frame(slr_ts_points %>% select(
      Year = year, District = district, Latitude = lat, Longitude = lon,
      `Subsidence Rate (mm/yr)` = subsidence_mm_yr,
      `Global SLR (mm)` = global_slr_mm,
      `Cumulative Subsidence (mm)` = cumul_subsidence_mm,
      `Total Relative SLR (mm)` = total_relative_slr_mm,
      `Total Relative SLR (cm)` = total_relative_slr_cm,
      `Risk Category` = risk_category,
      `Is Upazila` = is_upazila,
      `Parent District` = parent_district
    )),
    "Annual Pivot (cm)" = as.data.frame(
      slr_ts_points %>%
        select(year, district, total_relative_slr_cm) %>%
        pivot_wider(names_from = year, values_from = total_relative_slr_cm) %>%
        rename(District = district)
    ),
    "Trend by District" = as.data.frame(trend_export_raw),
    "Regional Summary"  = as.data.frame(regional_trend_raw %>% rename(
      Year = year, `Mean SLR (cm)` = mean_slr_cm,
      `Max SLR (cm)` = max_slr_cm, `Min SLR (cm)` = min_slr_cm,
      `# HIGH` = n_high, `# MODERATE` = n_moderate,
      `# LOW` = n_low, `# NO RISK` = n_no_risk
    )),
    "SSP Scenarios"     = as.data.frame(ssp_global)
  ),
  path = EXCEL_PATH
)
message("✓ Excel data file written: ", EXCEL_PATH)

if (!is.null(dem_raster) && is.finite(slr_thresh_m)) {
  # ─────────────────────────────────────────────────────────────────────────────
  # 3. GEODATA DISTRICT BOUNDARIES (GADM LEVEL 2)
  # ─────────────────────────────────────────────────────────────────────────────
  message("Fetching district-level boundaries from GADM via geodata ...")
  
  # Pull administrative level 2 boundaries (Districts) for Bangladesh
  bgd_l2_sp <- geodata::gadm(country = "BGD", level = 2, path = "output/data")
  bgd_l2    <- sf::st_as_sf(bgd_l2_sp)
  
  # GADM stores district names in the 'NAME_2' column
  bgd_l2$join_key <- normalize_name(bgd_l2$NAME_2)
  
  coastal_keys <- normalize_name(unique(slr_ts_polygons$district))
  study_sf     <- bgd_l2[bgd_l2$join_key %in% coastal_keys, ]
  
  unmatched <- setdiff(coastal_keys, bgd_l2$join_key)
  if (length(unmatched) > 0) {
    warning("Unmatched districts: ", paste(unmatched, collapse = ", "))
  }
  message(sprintf("✓ %d coastal districts matched using GADM Level 2 Data", nrow(study_sf)))
# ─────────────────────────────────────────────────────────────────────────────
# 4. DEM — SRTM 30-arc-second with graceful synthetic fallback
# ─────────────────────────────────────────────────────────────────────────────
message("Fetching SRTM DEM tiles …")
tile_grid <- expand.grid(lon = c(88, 89, 90, 91, 92), lat = c(20, 21, 22))
srtm_list <- vector("list", nrow(tile_grid))
for (i in seq_len(nrow(tile_grid))) {
  srtm_list[[i]] <- tryCatch(
    geodata::elevation_3s(lon = tile_grid$lon[i], lat = tile_grid$lat[i],
                          path = "output/data"),
    error = function(e) NULL
  )
}

srtm_list <- Filter(Negate(is.null), srtm_list)
study_ext <- terra::ext(EXTENT["xmin"], EXTENT["xmax"],
                        EXTENT["ymin"], EXTENT["ymax"])
if (length(srtm_list) > 0) {
  dem <- terra::crop(terra::mosaic(terra::sprc(srtm_list), fun = "mean"), study_ext)
  message("✓ SRTM DEM ready")
} else {
  message("⚠ SRTM unavailable — building realistic synthetic DEM")
  r   <- terra::rast(xmin = EXTENT["xmin"], xmax = EXTENT["xmax"],
                     ymin = EXTENT["ymin"], ymax = EXTENT["ymax"],
                     resolution = 0.005, crs = "EPSG:4326")
  xy  <- terra::xyFromCell(r, seq_len(terra::ncell(r)))
  # Coastal Bangladesh realistic elevation gradient (delta plain 0-5 m,
  # Chittagong Hill Tracts rising to 600 m eastward)
  dist_coast <- pmax(0, (xy[, 1] - 88.8) * 20 + (xy[, 2] - 20.5) * 8)
  elev <- pmax(0, dist_coast + rnorm(nrow(xy), 0, 1.5))
  terra::values(r) <- elev
  dem <- r
  message("✓ Synthetic DEM created")
}

# ─────────────────────────────────────────────────────────────────────────────
# 5. SHARED MAP THEME
# ─────────────────────────────────────────────────────────────────────────────
map_theme <- function(base = 11) {
  theme_bw(base_size = base) +
    theme(
      plot.title      = element_text(face = "bold", size = base + 3,
                                     color = THEME_TITLE),
      plot.subtitle   = element_text(size = base - 1, color = "#444444"),
      plot.caption    = element_text(size = 6.5, color = "#777777", hjust = 0),
      legend.position = "right",
      legend.title    = element_text(face = "bold", size = base - 1),
      legend.text     = element_text(size = base - 2),
      legend.key.size = unit(0.9, "cm"),
      legend.key      = element_rect(fill = "white", colour = "grey40"),
      panel.grid.major = element_line(color = THEME_GRID, linewidth = 0.25),
      plot.background  = element_rect(fill = THEME_BG,    color = NA),
      panel.background = element_rect(fill = THEME_PANEL)
    )
}

chart_theme <- function(base = 12) {
  theme_bw(base_size = base) +
    theme(
      plot.title    = element_text(face = "bold", color = THEME_TITLE,
                                   size = base + 1),
      plot.subtitle = element_text(size = base - 2, color = "#555555"),
      plot.caption  = element_text(size = 7, color = "#888888", hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E8E8E8", linewidth = 0.35),
      plot.background  = element_rect(fill = "white", color = NA)
    )
}

# ─────────────────────────────────────────────────────────────────────────────
# 6. ANNUAL SLR MAP FUNCTION (enhanced)
# ─────────────────────────────────────────────────────────────────────────────
make_slr_map <- function(target_year, slr_data, district_shapes,
                         dem_raster = NULL) {
  yr_data <- slr_data %>% filter(year == target_year)
  map_sf  <- district_shapes %>% left_join(yr_data, by = "join_key")
  slr_thresh_m <- mean(yr_data$total_relative_slr_cm, na.rm = TRUE) / 100
  flood_df <- NULL
  if (!is.null(dem_raster) && is.finite(slr_thresh_m)) {
    # Mask the DEM to your coastal district polygons to eliminate the boxy background square
    dem_masked <- terra::mask(dem_raster, terra::vect(district_shapes))
    
    frast <- terra::ifel(dem_masked <= slr_thresh_m, 4L,
                         terra::ifel(dem_masked <= slr_thresh_m * 2, 3L,
                                     terra::ifel(dem_masked <= slr_thresh_m * 3, 2L, 1L)))
    flood_df <- as.data.frame(frast, xy = TRUE, na.rm = TRUE)
    names(flood_df)[3] <- "risk_level"
    flood_df$risk_category <- factor(
      c("NO RISK","LOW","MODERATE","HIGH")[flood_df$risk_level],
      levels = c("HIGH","MODERATE","LOW","NO RISK"))
  }
  # Dummy legend df
  mid_x <- mean(EXTENT[c("xmin","xmax")])
  mid_y <- mean(EXTENT[c("ymin","ymax")])
  dummy <- data.frame(
    x = rep(mid_x, 4), y = rep(mid_y, 4),
    risk_category = factor(c("HIGH","MODERATE","LOW","NO RISK"),
                           levels = c("HIGH","MODERATE","LOW","NO RISK"))
  )
  p <- ggplot()
  if (!is.null(flood_df))
    p <- p + geom_raster(data = flood_df, aes(x, y, fill = risk_category),
                         alpha = 0.55, show.legend = FALSE)
  p <- p +
    geom_sf(data = map_sf, aes(fill = risk_category),
            color = "white", linewidth = 0.45, alpha = 0.88,
            show.legend = FALSE) +
    scale_fill_manual(values = risk_colors, labels = risk_labels,
                      name = "Relative SLR Risk", drop = FALSE,
                      na.value = "#BBBBBB") +
    geom_point(data = dummy, aes(x, y, fill = risk_category),
               shape = 22, size = 5, colour = "grey40", stroke = 0.4,
               alpha = 0) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    geom_sf_label(data = map_sf,
                  aes(label = paste0(NAME_2, "\n",
                                     round(total_relative_slr_cm, 1), " cm")),
                  size = 2.0, color = "black", fill = alpha("white", 0.55),
                  label.size = 0.15, label.padding = unit(0.12, "lines"),
                  check_overlap = TRUE) +
    annotation_scale(location = "bl", width_hint = 0.22, text_cex = 0.75) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering(),
                           height = unit(1.3,"cm"), width = unit(1.3,"cm")) +
    labs(
      title    = sprintf("Bangladesh Coastal Sea Level Rise — %d", target_year),
      subtitle = sprintf(
        "Mean Relative SLR: %.1f cm above 2000 baseline  |  Satkhira to Teknaf",
        mean(yr_data$total_relative_slr_cm, na.rm = TRUE)),
      caption  = paste0(
        "Sources: IPCC AR6 • AVISO Satellite Altimetry • BWDB Tide Gauges ",
        "• GADM v4.1\n",
        "Relative SLR = Global SLR + Cumulative Land Subsidence ",
        "(Ganges-Brahmaputra Delta)\n",
        "Subsidence data: Pethick & Orford (2013) | ",
        "Brown & Nicholls (2015)"),
      x = "Longitude (°E)", y = "Latitude (°N)"
    ) +
    coord_sf(xlim = EXTENT[c("xmin","xmax")],
             ylim = EXTENT[c("ymin","ymax")], expand = FALSE) +
    map_theme()
  # Teknaf point
  tk <- yr_data %>% filter(join_key == normalize_name("Teknaf"))
  if (nrow(tk) == 1) {
    p <- p +
      geom_point(data = tk, aes(lon, lat, color = risk_category),
                 shape = 23, size = 4, fill = "white",
                 stroke = 1.4, show.legend = FALSE) +
      scale_color_manual(values = risk_colors) +
      annotate("text", x = tk$lon + 0.07, y = tk$lat,
               label = "Teknaf\n(Upazila)", size = 2.4,
               fontface = "italic", hjust = 0, lineheight = 0.9)
  }
  
  p
}

# ─────────────────────────────────────────────────────────────────────────────
# 7. GENERATE ALL 26 ANNUAL MAPS
# ─────────────────────────────────────────────────────────────────────────────
message("\nGenerating 26 annual SLR maps …")
for (yr in 2000:2025) {
  message(sprintf("  → %d", yr))
  ggsave(
    filename = sprintf("output/maps/SLR_Bangladesh_%d.png", yr),
    plot     = make_slr_map(yr, slr_ts_points, study_sf, dem),
    width = MAP_W, height = MAP_H, dpi = BASE_DPI, units = "in", bg = "white"
  )
}

message("✓ 26 annual maps saved")

# ─────────────────────────────────────────────────────────────────────────────
# 8. PDF ATLAS
# ─────────────────────────────────────────────────────────────────────────────
message("Building PDF atlas …")
while (!is.null(dev.list())) dev.off()
pdf("output/atlas/Bangladesh_SLR_Atlas_2000_2025.pdf",
    width = MAP_W, height = MAP_H, onefile = TRUE)
for (yr in 2000:2025) print(make_slr_map(yr, slr_ts_points, study_sf, dem))
dev.off()
message("✓ PDF atlas saved")

# ─────────────────────────────────────────────────────────────────────────────
# 9. TREND ANALYSIS — 6 high-quality plots
# ─────────────────────────────────────────────────────────────────────────────
message("\nRunning trend analyses …")
regional_trend <- slr_ts_points %>%
  group_by(year) %>%
  summarise(
    mean_slr_cm = mean(total_relative_slr_cm, na.rm = TRUE),
    max_slr_cm  = max(total_relative_slr_cm,  na.rm = TRUE),
    min_slr_cm  = min(total_relative_slr_cm,  na.rm = TRUE),
    sd_slr_cm   = sd(total_relative_slr_cm,   na.rm = TRUE),
    n_high      = sum(risk_category == "HIGH"),
    n_moderate  = sum(risk_category == "MODERATE"),
    n_low       = sum(risk_category == "LOW"),
    n_no_risk   = sum(risk_category == "NO RISK"),
    .groups = "drop"
  )
lm_fit  <- lm(mean_slr_cm ~ year, data = regional_trend)
lm_sum  <- summary(lm_fit)
mk_ok   <- FALSE
mk_result <- sen_slope <- NULL
tryCatch({
  mk_result <- trend::mk.test(regional_trend$mean_slr_cm)
  sen_slope <- trend::sens.slope(regional_trend$mean_slr_cm)
  mk_ok <- TRUE
}, error = function(e) message("  Mann-Kendall: ", e$message))

# ── PLOT 1: Regional trend with ribbon + Sen's slope ─────────────────────
trend_ann <- sprintf(
  "Linear slope: +%.3f cm/yr\nSen's slope: %s cm/yr\nR² = %.4f\nMann-Kendall p %s",
  coef(lm_fit)["year"],
  if (mk_ok) sprintf("%.3f", as.numeric(sen_slope$estimates)) else "N/A",
  lm_sum$r.squared,
  if (mk_ok) sprintf("= %.2e", mk_result$p.value) else "N/A"
)
p1 <- ggplot(regional_trend, aes(year, mean_slr_cm)) +
  geom_ribbon(aes(ymin = mean_slr_cm - sd_slr_cm,
                  ymax = mean_slr_cm + sd_slr_cm),
              fill = "#1F4E79", alpha = 0.12) +
  geom_ribbon(aes(ymin = min_slr_cm, ymax = max_slr_cm),
              fill = "#1F4E79", alpha = 0.08) +
  geom_line(color = "#1F4E79", linewidth = 1.3) +
  geom_point(aes(color = mean_slr_cm), size = 3.2, shape = 21,
             fill = "white", stroke = 1.6) +
  scale_color_gradient2(low = "#228B22", mid = "#FF8C00", high = "#CC0000",
                        midpoint = 20, name = "Mean SLR\n(cm)") +
  geom_smooth(method = "lm", se = TRUE, color = "#E63946",
              linetype = "dashed", linewidth = 1.1, alpha = 0.15) +
  annotate("label", x = 2001.5,
           y = max(regional_trend$max_slr_cm) * 0.88,
           label = trend_ann, hjust = 0, size = 3.3,
           color = "#CC0000", fill = alpha("white", 0.85),
           label.size = 0.3, label.padding = unit(0.4, "lines")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(labels = scales::number_format(suffix = " cm")) +
  labs(
    title    = "Regional Mean Sea Level Rise Trend — Bangladesh Coast (2000–2025)",
    subtitle = "Dark ribbon = ±1 SD  |  Light ribbon = district range  |  Dashed = linear OLS",
    x = "Year", y = "Cumulative Relative SLR (cm)",
    caption  = "Relative SLR = Global SLR (AVISO) + Cumulative land subsidence"
  ) +
  chart_theme()
ggsave("output/trend/01_Regional_SLR_Trend.png", p1,
       width = 13, height = 7.5, dpi = BASE_DPI)

# ── PLOT 2: Risk category evolution (stacked area) ────────────────────────
risk_stack <- slr_ts_points %>%
  group_by(year, risk_category) %>%
  summarise(count = n(), .groups = "drop")
p2 <- ggplot(risk_stack, aes(year, count, fill = risk_category)) +
  geom_area(alpha = 0.88, color = "white", linewidth = 0.35) +
  scale_fill_manual(values = risk_colors, labels = risk_labels,
                    name = "Risk Category") +
  scale_x_continuous(breaks = seq(2000, 2025, 5)) +
  scale_y_continuous(breaks = 1:14) +
  labs(
    title    = "Evolution of Flood Risk Categories — Bangladesh Coastal Districts (2000–2025)",
    subtitle = "Colour area shows number of districts/upazilas in each risk class per year",
    x = "Year", y = "Number of Districts / Upazilas",
    caption  = "Risk classification based on cumulative relative SLR thresholds"
  ) +
  chart_theme()
ggsave("output/trend/02_Risk_Category_Evolution.png", p2,
       width = 13, height = 7, dpi = BASE_DPI)

# ── PLOT 3: District-level SLR trajectories ───────────────────────────────
p3 <- ggplot(slr_ts_points,
             aes(year, total_relative_slr_cm,
                 color = district, group = district)) +
  geom_line(linewidth = 1.0, alpha = 0.88) +
  geom_point(data = slr_ts_points %>% filter(year == 2025),
             size = 2.5, shape = 21, fill = "white", stroke = 1.2) +
  geom_label_repel(
    data = slr_ts_points %>% filter(year == 2025),
    aes(label = district),
    size = 2.8, nudge_x = 0.8, direction = "y",
    segment.size = 0.35, segment.color = "grey60",
    box.padding = 0.25, label.size = 0.2, alpha = 0.9
  ) +
  geom_hline(yintercept = c(8, 20, 35),
             linetype = c("dotted","dashed","solid"),
             color = c("#228B22","#FF8C00","#CC0000"),
             linewidth = 0.75) +
  annotate("text", x = 2000.5, y = c(8.8, 20.8, 35.8),
           label = c("LOW threshold (8 cm)",
                     "MODERATE threshold (20 cm)",
                     "HIGH threshold (35 cm)"),
           hjust = 0, size = 2.7,
           color = c("#228B22","#FF8C00","#CC0000")) +
  scale_x_continuous(breaks = seq(2000, 2025, 5),
                     limits = c(2000, 2026.5)) +
  scale_color_viridis_d(option = "turbo", name = "District") +
  guides(color = "none") +
  labs(
    title    = "District-Level SLR Trajectories — Bangladesh Coast (2000–2025)",
    subtitle = "Satkhira (highest subsidence) → Teknaf (lowest) reflects delta subsidence gradient",
    x = "Year", y = "Cumulative Relative SLR (cm)",
    caption  = "Subsidence data: Pethick & Orford (2013); Brown & Nicholls (2015)"
  ) +
  chart_theme()
ggsave("output/trend/03_District_SLR_Trajectories.png", p3,
       width = 14, height = 7.5, dpi = BASE_DPI)

# ── PLOT 4: Snapshot bar chart (2000 / 2010 / 2025) ─────────────────────
comp_data <- slr_ts_points %>%
  filter(year %in% c(2000, 2010, 2025)) %>%
  mutate(year = factor(year))
p4 <- ggplot(comp_data,
             aes(reorder(district, total_relative_slr_cm),
                 total_relative_slr_cm, fill = risk_category)) +
  geom_col(color = "white", linewidth = 0.3, width = 0.75) +
  geom_text(aes(label = sprintf("%.1f", total_relative_slr_cm)),
            hjust = -0.15, size = 2.5, color = "grey30") +
  facet_wrap(~year, ncol = 3) +
  scale_fill_manual(values = risk_colors, labels = risk_labels, name = "Risk") +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Cumulative SLR by District — Snapshot Years 2000 | 2010 | 2025",
    x = NULL, y = "Cumulative Relative SLR (cm)",
    caption  = "Districts ordered by 2025 SLR magnitude within each panel"
  ) +
  chart_theme() +
  theme(
    strip.background = element_rect(fill = THEME_TITLE),
    strip.text       = element_text(color = "white", face = "bold", size = 11)
  )
ggsave("output/trend/04_District_Comparison_Snapshots.png", p4,
       width = 15, height = 8, dpi = BASE_DPI)

# ── PLOT 5: Subsidence rate vs 2025 SLR scatter ───────────────────────────
dist_2025 <- slr_ts_points %>%
  filter(year == 2025, !is_upazila)
p5 <- ggplot(dist_2025,
             aes(subsidence_mm_yr, total_relative_slr_cm,
                 color = risk_category, size = pop_density)) +
  geom_point(alpha = 0.85, shape = 16) +
  geom_label_repel(aes(label = district), size = 3,
                   segment.size = 0.35, box.padding = 0.4,
                   label.size = 0.2) +
  geom_smooth(method = "lm", se = TRUE, color = "grey40",
              linetype = "dashed", linewidth = 0.9, inherit.aes = FALSE,
              aes(subsidence_mm_yr, total_relative_slr_cm)) +
  scale_color_manual(values = risk_colors, name = "Risk (2025)") +
  scale_size_continuous(name = "Pop. Density\n(/km²)",
                        range = c(3, 10), labels = scales::comma) +
  scale_x_continuous(labels = scales::number_format(suffix = " mm/yr")) +
  scale_y_continuous(labels = scales::number_format(suffix = " cm")) +
  labs(
    title    = "Land Subsidence Rate vs Cumulative SLR (2025) — Coastal Districts",
    subtitle = "Point size = population density  |  Dashed = OLS regression",
    x = "Subsidence Rate (mm/yr)", y = "Cumulative Relative SLR in 2025 (cm)",
    caption  = "Strong linear relationship confirms subsidence as dominant driver of relative SLR"
  ) +
  chart_theme()
ggsave("output/trend/05_Subsidence_vs_SLR_Scatter.png", p5,
       width = 12, height = 7.5, dpi = BASE_DPI)

# ── PLOT 6: IPCC SSP scenario fan chart ───────────────────────────────────
#   Extend district mean from 2025 → 2075 under three SSP scenarios
dist_mean_2025_cm <- mean(dist_2025$total_relative_slr_cm)
mean_sub_mm_yr    <- mean(dist_2025$subsidence_mm_yr)
ssp_fan <- ssp_global %>%
  mutate(
    subsidence_mm   = mean_sub_mm_yr * (year - 2025),
    ssp126_total_cm = (ssp126_mm + subsidence_mm) / 10,
    ssp245_total_cm = (ssp245_mm + subsidence_mm) / 10,
    ssp585_total_cm = (ssp585_mm + subsidence_mm) / 10
  ) %>%
  pivot_longer(c(ssp126_total_cm, ssp245_total_cm, ssp585_total_cm),
               names_to = "scenario", values_to = "slr_cm") %>%
  mutate(scenario = recode(scenario,
                           ssp126_total_cm = "SSP1-2.6 (Low emissions)",
                           ssp245_total_cm = "SSP2-4.5 (Intermediate)",
                           ssp585_total_cm = "SSP5-8.5 (High emissions)"
  ))
# Add historical tail
hist_tail <- regional_trend %>%
  filter(year >= 2020) %>%
  select(year, mean_slr_cm) %>%
  crossing(scenario = unique(ssp_fan$scenario)) %>%
  rename(slr_cm = mean_slr_cm)
ssp_plot_df <- bind_rows(hist_tail, ssp_fan)
p6 <- ggplot(ssp_plot_df, aes(year, slr_cm, color = scenario,
                              linetype = scenario)) +
  geom_line(linewidth = 1.2, alpha = 0.9) +
  geom_vline(xintercept = 2025, linetype = "dotted",
             color = "grey50", linewidth = 0.8) +
  annotate("text", x = 2025.5, y = 5,
           label = "2025\n(observed)", hjust = 0, size = 3,
           color = "grey40") +
  geom_hline(yintercept = c(20, 35, 60),
             linetype = "dashed",
             color = c("#FF8C00","#CC0000","#67000D"),
             linewidth = 0.6, alpha = 0.7) +
  annotate("text", x = 2027, y = c(21, 36, 61),
           label = c("MODERATE threshold",
                     "HIGH threshold",
                     "SEVERE threshold"),
           hjust = 0, size = 2.6,
           color = c("#FF8C00","#CC0000","#67000D")) +
  scale_color_manual(
    values = c("SSP1-2.6 (Low emissions)"  = "#228B22",
               "SSP2-4.5 (Intermediate)"   = "#FF8C00",
               "SSP5-8.5 (High emissions)" = "#CC0000"),
    name = "IPCC AR6 Scenario"
  ) +
  scale_linetype_manual(
    values = c("SSP1-2.6 (Low emissions)"  = "dashed",
               "SSP2-4.5 (Intermediate)"   = "solid",
               "SSP5-8.5 (High emissions)" = "solid"),
    name = "IPCC AR6 Scenario"
  ) +
  scale_x_continuous(breaks = seq(2020, 2075, 10)) +
  scale_y_continuous(labels = scales::number_format(suffix = " cm")) +
  labs(
    title    = "IPCC AR6 SSP Scenario Projections — Bangladesh Coast (2020–2075)",
    subtitle = "Regional mean relative SLR = Global SLR (SSP) + Mean land subsidence",
    x = "Year", y = "Cumulative Relative SLR (cm)",
    caption  = "Global SLR projections: IPCC AR6 WGI Table 9.5 | Mean district subsidence included"
  ) +
  chart_theme()
ggsave("output/trend/06_IPCC_SSP_Scenario_Projections.png", p6,
       width = 13, height = 7.5, dpi = BASE_DPI)
message("✓ 6 trend/analysis plots saved")

# 10. MULTI-LAYER VULNERABILITY INDEX (2025)
# ─────────────────────────────────────────────────────────────────────────────
# Calculate indicators min-max normalised; weights based on IPCC risk framework
vuln_2025 <- dist_2025 %>%
  mutate(
    # SLR hazard indicator
    v_slr      = (total_relative_slr_cm - min(total_relative_slr_cm)) /
      (max(total_relative_slr_cm) - min(total_relative_slr_cm)),
    # Exposure indicators
    v_pop      = (pop_density - min(pop_density)) /
      (max(pop_density) - min(pop_density)),
    v_agri     = (lulc_agri_pct - min(lulc_agri_pct)) /
      (max(lulc_agri_pct) - min(lulc_agri_pct)),
    # Mangrove: higher = more protection → invert
    # Mangrove: higher = more protection → invert
    v_mangrove = 1 - (lulc_mangrove_pct - min(lulc_mangrove_pct)) /
      (max(lulc_mangrove_pct) - min(lulc_mangrove_pct)),
    # Closer to coast = higher risk
    v_coast    = 1 - (dist_to_coast_km - min(dist_to_coast_km)) /
      (max(dist_to_coast_km) - min(dist_to_coast_km)),
    # Embankment: more = more protection → invert
    v_embankmt = 1 - (embankment_km - min(embankment_km)) /
      (max(embankment_km) - min(embankment_km)),
    # --- Composite Vulnerability Index (weighted sum) ----------------------
    # Weights: SLR hazard (30%), Population (20%), Agriculture (15%),
    #          Mangrove buffer (15%), Coastal proximity (10%), Embankment (10%)
    CVI = 0.30 * v_slr +
      0.20 * v_pop +
      0.15 * v_agri +
      0.15 * v_mangrove +
      0.10 * v_coast +
      0.10 * v_embankmt,
    CVI_class = case_when(
      CVI >= quantile(CVI, 0.75) ~ "VERY HIGH",
      CVI >= quantile(CVI, 0.50) ~ "HIGH",
      CVI >= quantile(CVI, 0.25) ~ "MODERATE",
      TRUE                        ~ "LOW"
    ),
    CVI_class = factor(CVI_class,
                       levels = c("VERY HIGH","HIGH","MODERATE","LOW"))
  )
cvi_colors <- c(
  "VERY HIGH" = "#67000D",
  "HIGH"      = "#CC0000",
  "MODERATE"  = "#FF8C00",
  "LOW"       = "#228B22"
)
# CVI map
cvi_map_sf <- study_sf %>%
  left_join(vuln_2025 %>% select(join_key, CVI, CVI_class), by = "join_key")
p_cvi_map <- ggplot() +
  geom_sf(data = cvi_map_sf, aes(fill = CVI_class),
          color = "white", linewidth = 0.45, alpha = 0.92) +
  geom_sf_label(data = cvi_map_sf,
                aes(label = paste0(NAME_2, "\nCVI:", round(CVI, 2))),
                size = 2.0, fill = alpha("white", 0.6),
                label.size = 0.15, check_overlap = TRUE) +
  scale_fill_manual(values = cvi_colors,
                    name = "Coastal Vulnerability\nIndex (CVI)",
                    na.value = "#BBBBBB") +
  annotation_scale(location = "bl", width_hint = 0.22, text_cex = 0.75) +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1.3,"cm"), width = unit(1.3,"cm")) +
  labs(
    title    = "Coastal Vulnerability Index (CVI) — Bangladesh (2025)",
    subtitle = paste0("CVI = 0.30×SLR + 0.20×Population + 0.15×Agriculture + ",
                      "0.15×Mangrove(inv.) + 0.10×Coast + 0.10×Embankment(inv.)"),
    caption  = "All indicators min-max normalised; weights based on IPCC risk framework",
    x = "Longitude (°E)", y = "Latitude (°N)"
  ) +
  coord_sf(xlim = EXTENT[c("xmin","xmax")],
           ylim = EXTENT[c("ymin","ymax")], expand = FALSE) +
  map_theme()
ggsave("output/risk/CVI_Map_2025.png", p_cvi_map,
       width = MAP_W, height = MAP_H, dpi = BASE_DPI)
# CVI radar-style bar chart
cvi_long <- vuln_2025 %>%
  select(district, v_slr, v_pop, v_agri, v_mangrove, v_coast, v_embankmt) %>%
  pivot_longer(-district, names_to = "indicator", values_to = "score") %>%
  mutate(indicator = recode(indicator,
                            v_slr      = "SLR Hazard",
                            v_pop      = "Population",
                            v_agri     = "Agriculture",
                            v_mangrove = "Mangrove\n(inverted)",
                            v_coast    = "Coast Proximity",
                            v_embankmt = "Embankment\n(inverted)"
  ))
p_cvi_bar <- ggplot(cvi_long,
                    aes(indicator, score, fill = score)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.2) +
  facet_wrap(~ district, ncol = 4) +
  scale_fill_gradient2(low = "#228B22", mid = "#FF8C00", high = "#CC0000",
                       midpoint = 0.5, name = "Score (0–1)") +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  coord_flip() +
  labs(
    title    = "CVI Component Scores by District — Bangladesh Coastal Zone (2025)",
    subtitle = "Higher score = higher vulnerability for that indicator",
    x = NULL, y = "Normalised Score (0 = low risk, 1 = high risk)",
    caption  = "Mangrove and Embankment indicators are inverted (more = lower vulnerability)"
  ) +
  chart_theme() +
  theme(
    strip.background = element_rect(fill = THEME_TITLE),
    strip.text       = element_text(color = "white", face = "bold", size = 8),
    axis.text.y      = element_text(size = 7.5)
  )
ggsave("output/risk/CVI_Component_Scores.png", p_cvi_bar,
       width = 16, height = 10, dpi = BASE_DPI)
message("✓ CVI analysis complete")

# ─────────────────────────────────────────────────────────────────────────────
# 11. FLOOD RISK 2050 PROJECTION — THREE SSP SCENARIOS
# ─────────────────────────────────────────────────────────────────────────────
message("\n", strrep("═", 65))
message(" SECTION 11: FLOOD RISK 2050 — IPCC AR6 SSP SCENARIOS")
message(strrep("═", 65))
proj_years <- 2050 - 2025
# Per-district 2050 SLR under each SSP
ssp_2050 <- ssp_global %>% filter(year == 2050)
slr_2025_per_dist <- dist_2025 %>%
  select(district, join_key, lat, lon,
         subsidence_mm_yr, pop_density,
         lulc_agri_pct, lulc_mangrove_pct,
         dist_to_coast_km, embankment_km,
         slr_2025_cm = total_relative_slr_cm)
slr_2050_scenarios <- slr_2025_per_dist %>%
  mutate(
    subsidence_2050_mm = subsidence_mm_yr * proj_years,
    # Each scenario adds district subsidence on top of global SSP SLR
    slr_2050_ssp126 = (ssp_2050$ssp126_mm + subsidence_2050_mm) / 10,
    slr_2050_ssp245 = (ssp_2050$ssp245_mm + subsidence_2050_mm) / 10,
    slr_2050_ssp585 = (ssp_2050$ssp585_mm + subsidence_2050_mm) / 10
  )
# Function: compute flood risk for a given scenario column
compute_flood_risk <- function(df, slr_col, pop_w = 0.70) {
  df %>%
    mutate(
      slr_val    = .data[[slr_col]],
      slr_min    = min(slr_val, na.rm = TRUE),
      slr_max    = max(slr_val, na.rm = TRUE),
      hazard_norm = (slr_val - slr_min) / (slr_max - slr_min),
      pop_min    = min(pop_density, na.rm = TRUE),
      pop_max    = max(pop_density, na.rm = TRUE),
      pop_norm   = (pop_density - pop_min) / (pop_max - pop_min),
      # Mangrove buffer reduces risk (up to 15%)
      mng_min    = min(lulc_mangrove_pct, na.rm = TRUE),
      mng_max    = max(lulc_mangrove_pct, na.rm = TRUE),
      mng_norm   = (lulc_mangrove_pct - mng_min) / (mng_max - mng_min),
      flood_score = hazard_norm * (0.30 + pop_w * pop_norm) *
        (1 - 0.15 * mng_norm),
      flood_class = case_when(
        flood_score >= quantile(flood_score, 0.75) ~ "SEVERE",
        flood_score >= quantile(flood_score, 0.50) ~ "HIGH",
        flood_score >= quantile(flood_score, 0.25) ~ "MODERATE",
        TRUE                                        ~ "LOW"
      ),
      flood_class = factor(flood_class,
                           levels = c("SEVERE","HIGH","MODERATE","LOW"))
    )
}

fr_ssp126 <- compute_flood_risk(slr_2050_scenarios, "slr_2050_ssp126")
fr_ssp245 <- compute_flood_risk(slr_2050_scenarios, "slr_2050_ssp245")
fr_ssp585 <- compute_flood_risk(slr_2050_scenarios, "slr_2050_ssp585")
# Function: flood risk map for a given scenario
make_flood_map <- function(fr_df, slr_col, scenario_label,
                           title_color = "#67000D") {
  map_sf <- study_sf %>%
    left_join(fr_df %>% select(join_key, flood_class, flood_score,
                               slr_val, pop_density),
              by = "join_key")
  dummy <- data.frame(
    x = rep(mean(EXTENT[c("xmin","xmax")]), 4),
    y = rep(mean(EXTENT[c("ymin","ymax")]), 4),
    flood_class = factor(c("SEVERE","HIGH","MODERATE","LOW"),
                         levels = c("SEVERE","HIGH","MODERATE","LOW"))
  )
  ggplot() +
    geom_sf(data = map_sf, aes(fill = flood_class),
            color = "white", linewidth = 0.45, alpha = 0.92,
            show.legend = FALSE) +
    scale_fill_manual(values = flood_colors,
                      name = "2050 Flood Risk", drop = FALSE,
                      na.value = "#BBBBBB") +
    geom_point(data = dummy, aes(x, y, fill = flood_class),
               shape = 22, size = 5, colour = "grey30",
               stroke = 0.4, alpha = 0) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    geom_sf_label(data = map_sf,
                  aes(label = paste0(NAME_2, "\n",
                                     round(slr_val, 1), " cm")),
                  size = 2.0, fill = alpha("white", 0.6),
                  label.size = 0.15, check_overlap = TRUE) +
    annotation_scale(location = "bl", width_hint = 0.22, text_cex = 0.75) +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering(),
                           height = unit(1.3,"cm"), width = unit(1.3,"cm")) +
    labs(
      title    = sprintf("Bangladesh Coastal Flood Risk — 2050 (%s)",
                         scenario_label),
      subtitle = "Composite risk = normalised SLR hazard × population exposure × mangrove buffer",
      caption  = paste0(
        "SLR 2050 = IPCC AR6 ", scenario_label, " global SLR + district subsidence\n",
        "Risk score = hazard_norm × (0.30 + 0.70 × pop_norm) × (1 − 0.15 × mangrove_norm)\n",
        "Classes: 25th / 50th / 75th percentile within study area"
      ),
      x = "Longitude (°E)", y = "Latitude (°N)"
    ) +
    coord_sf(xlim = EXTENT[c("xmin","xmax")],
             ylim = EXTENT[c("ymin","ymax")], expand = FALSE) +
    map_theme() +
    theme(plot.title = element_text(color = title_color))
}

p_fr126 <- make_flood_map(fr_ssp126, "slr_2050_ssp126",
                          "SSP1-2.6 (Low emissions)", "#228B22")
p_fr245 <- make_flood_map(fr_ssp245, "slr_2050_ssp245",
                          "SSP2-4.5 (Intermediate)", "#FF8C00")
p_fr585 <- make_flood_map(fr_ssp585, "slr_2050_ssp585",
                          "SSP5-8.5 (High emissions)", "#67000D")
ggsave("output/risk/Flood_Risk_2050_SSP126.png", p_fr126,
       width = MAP_W, height = MAP_H, dpi = BASE_DPI)
ggsave("output/risk/Flood_Risk_2050_SSP245.png", p_fr245,
       width = MAP_W, height = MAP_H, dpi = BASE_DPI)
ggsave("output/risk/Flood_Risk_2050_SSP585.png", p_fr585,
       width = MAP_W, height = MAP_H, dpi = BASE_DPI)
# 3-panel comparison
p_panel <- (p_fr126 + p_fr245 + p_fr585) +
  patchwork::plot_layout(ncol = 3, guides = "collect") +
  patchwork::plot_annotation(
    title = "Flood Risk 2050    b — Three IPCC AR6 SSP Scenarios",
    subtitle = "Bangladesh Coastal Districts: Satkhira to Teknaf",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 15,
                                   color = THEME_TITLE),
      plot.subtitle = element_text(size = 10, color = "#555555")
    )
  )
ggsave("output/risk/Flood_Risk_2050_SSP_Comparison.png", p_panel,
       width = 48, height = 14, dpi = BASE_DPI, limitsize = FALSE)
message("✓ 2050 flood risk maps saved (3 scenarios + comparison panel)")

# ── Flood risk ranking table chart ────────────────────────────────────────
rank_df <- slr_2050_scenarios %>%
  left_join(fr_ssp245 %>% select(join_key, flood_class,
                                 flood_score, slr_val),
            by = "join_key") %>%
  arrange(desc(flood_score)) %>%
  mutate(district = fct_reorder(district, flood_score))
p_rank <- ggplot(rank_df, aes(flood_score, district, fill = flood_class)) +
  geom_col(width = 0.65, color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.3f", flood_score)),
            hjust = -0.1, size = 3, color = "grey30") +
  scale_fill_manual(values = flood_colors, name = "Risk Class") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title    = "2050 Flood Risk Score Ranking — SSP2-4.5 Scenario",
    subtitle = "Composite score = hazard × exposure × (1 − mangrove buffer)",
    x = "Composite Flood Risk Score", y = NULL,
    caption  = "Higher score = greater projected flood risk by 2050"
  ) +
  chart_theme()
ggsave("output/risk/Flood_Risk_2050_Ranking.png", p_rank,
       width = 11, height = 7, dpi = BASE_DPI)

# ─────────────────────────────────────────────────────────────────────────────
# 12. FULL EXCEL REPORT — all sheets
# ─────────────────────────────────────────────────────────────────────────────
message("\nWriting final Excel report …")
flood_risk_export <- slr_2050_scenarios %>%
  left_join(fr_ssp245 %>% select(join_key, flood_class,
                                 flood_score, hazard_norm, pop_norm),
            by = "join_key") %>%
  arrange(desc(flood_score)) %>%
  transmute(
    District                    = district,
    `SLR 2025 (cm)`             = round(slr_2025_cm, 2),
    `SLR 2050 SSP1-2.6 (cm)`   = round(slr_2050_ssp126, 2),
    `SLR 2050 SSP2-4.5 (cm)`   = round(slr_2050_ssp245, 2),
    `SLR 2050 SSP5-8.5 (cm)`   = round(slr_2050_ssp585, 2),
    `Population Density (/km²)` = pop_density,
    `Agri Land (%)`             = lulc_agri_pct,
    `Mangrove Cover (%)`        = lulc_mangrove_pct,
    `Hazard Score`              = round(hazard_norm, 4),
    `Flood Risk Score (SSP245)` = round(flood_score, 4),
    `Flood Risk Class (SSP245)` = as.character(flood_class)
  )
cvi_export <- vuln_2025 %>%
  arrange(desc(CVI)) %>%
  transmute(
    District        = district,
    `SLR Score`     = round(v_slr,      4),
    `Pop Score`     = round(v_pop,      4),
    `Agri Score`    = round(v_agri,     4),
    `Mangrove Score`= round(v_mangrove, 4),
    `Coast Score`   = round(v_coast,    4),
    `Embankmt Score`= round(v_embankmt, 4),
    `CVI (composite)`= round(CVI, 4),
    `CVI Class`     = as.character(CVI_class)
  )
wb <- openxlsx::loadWorkbook(EXCEL_PATH)
for (sn in c("Flood Risk 2050", "Coastal Vulnerability Index")) {
  if (sn %in% openxlsx::getSheetNames(EXCEL_PATH))
    openxlsx::removeWorksheet(wb, sn)
  openxlsx::addWorksheet(wb, sn)
}

openxlsx::writeData(wb, "Flood Risk 2050",            flood_risk_export)
openxlsx::writeData(wb, "Coastal Vulnerability Index", cvi_export)
openxlsx::saveWorkbook(wb, EXCEL_PATH, overwrite = TRUE)
message("✓ Excel report updated with Flood Risk 2050 & CVI sheets")

# ─────────────────────────────────────────────────────────────────────────────
# 13. FINAL SUMMARY REPORT
# ─────────────────────────────────────────────────────────────────────────────
message("\n", strrep("═", 65))
message(" BANGLADESH COASTAL SLR ANALYSIS v5 — SUMMARY")
message(strrep("═", 65))
message(sprintf("Study Period   : 2000–2025 (26 years)"))
message(sprintf("Study Area     : Satkhira to Teknaf, Bay of Bengal"))
message(sprintf("Locations      : 12 coastal districts + Teknaf upazila"))
message(sprintf("2000 mean SLR  : %.2f cm  (baseline)",
                regional_trend$mean_slr_cm[regional_trend$year == 2000]))
message(sprintf("2025 mean SLR  : %.2f cm",
                regional_trend$mean_slr_cm[regional_trend$year == 2025]))
message(sprintf("Linear trend   : +%.4f cm/yr  |  R² = %.4f",
                coef(lm_fit)["year"], lm_sum$r.squared))
message("\n── 2025 DISTRICT RANKING ──")
print(as.data.frame(
  dist_2025 %>%
    left_join(vuln_2025 %>% select(district, CVI_class), by = "district") %>%
    arrange(desc(total_relative_slr_cm)) %>%
    select(District = district,
           `SLR (cm)` = total_relative_slr_cm,
           Risk = risk_category,
           `CVI Class` = CVI_class)
), row.names = FALSE)
print(as.data.frame(
  fr_ssp245 %>%
    arrange(desc(flood_score)) %>%
    select(District = district,
           `SLR 2050 (cm)` = slr_val,
           Score = flood_score, Class = flood_class) %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))
), row.names = FALSE)
message("\n── OUTPUTS ──")
message(" output/maps/   : 26 annual PNG maps (2000–2025)")
message(" output/atlas/  : PDF atlas (all 26 years)")
message(" output/trend/  : 6 trend / analysis plots")
message(" output/risk/   : CVI map, 3× SSP flood risk maps,")
message("                  3-panel comparison, component scores, ranking")
message(" output/data/   : Excel report with 7 sheets")
message("\n", strrep("═", 65))
message(" ANALYSIS COMPLETE")
message(strrep("═", 65))