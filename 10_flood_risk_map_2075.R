## =============================================================================
## SCRIPT 10 of 11: Real-GADM-polygon choropleth flood risk map for 2075
##   -> Extension #19: Risk = SLR hazard x population density (quantile
##      classes), rendered on actual district polygons (GADM shapefile) when
##      available, matching a standard choropleth flood-risk map style
##      instead of the sized/colored point-map approximations used in
##      scripts 04 and 06.
## Run scripts 01-06 first (needs `district_forecast`, `district_cov`,
## `out_dir`, `fig_dir`, `district_shapefile` from script 01).
## =============================================================================

## ---- 0. DEFENSIVE OUTPUT-DIR SETUP -------------------------------------------
## See script 07's note -- recreates out_dir/fig_dir/data_dir if this script
## is sourced standalone or the working directory changed since script 01.
if (!exists("data_dir")) data_dir <- "data"
if (!exists("out_dir"))  out_dir  <- "outputs"
if (!exists("fig_dir"))  fig_dir  <- file.path(out_dir, "figures")
if (!exists("district_shapefile")) district_shapefile <- file.path(data_dir, "bgd_coastal_districts.shp")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,  showWarnings = FALSE, recursive = TRUE)

## ---- 1. LOAD GADM DISTRICT POLYGONS (falls back to a point map) --------------
## Real workflow: drop the 4 GADM shapefile parts (.shp/.dbf/.shx/.prj) into
## data/ as bgd_coastal_districts.shp (path already set in script 01). This
## tries a few common GADM/BBS attribute-name variants to find the
## district-name column and join it to `district` in district_cov /
## district_forecast -- edit `name_candidates` below if your file uses a
## different column, or check district name spelling consistency (e.g.
## "Cox's Bazar" vs "Coxs Bazar" vs "Cox'sbazar") after joining.

districts_shp <- NULL
have_shp <- requireNamespace("sf", quietly = TRUE) && file.exists(district_shapefile)

if (have_shp) {
  districts_shp <- tryCatch(sf::st_read(district_shapefile, quiet = TRUE), error = function(e) {
    message("[fallback] Could not read ", district_shapefile, " (", conditionMessage(e),
            ") -- rendering a point map instead.")
    NULL
  })
  if (!is.null(districts_shp)) {
    name_candidates <- c("NAME_2", "ADM2_EN", "District", "district", "NAME", "name", "DISTNAME")
    shp_name_field <- name_candidates[name_candidates %in% names(districts_shp)]
    if (length(shp_name_field) == 0) {
      message("[fallback] No recognizable district-name column found in the shapefile ",
              "(looked for: ", paste(name_candidates, collapse = ", "), ") -- rendering a point map instead. ",
              "Set `shp_name_field <- \"<your column>\"` manually if your GADM file uses a different name.")
      districts_shp <- NULL
    } else {
      districts_shp$district <- as.character(districts_shp[[shp_name_field[1]]])
    }
  }
} else {
  message("[synthetic fallback] GADM shapefile not found at ", district_shapefile,
          " (or the `sf` package is unavailable) -- rendering the choropleth as a point map ",
          "using approx_centroids, exactly as scripts 04/06 do. Drop the real ",
          ".shp/.dbf/.shx/.prj files into data/ to get true filled district polygons.")
}

## ---- 2. RISK = SLR HAZARD x POPULATION DENSITY (2075, SSP5-8.5) --------------
minmax10 <- function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))

flood_risk_2075 <- district_forecast %>%
  filter(year == 2075, scenario == "SSP5-8.5") %>%
  left_join(district_cov %>% select(district, pop_density_km2), by = "district") %>%
  mutate(
    slr_hazard_norm   = minmax10(district_relative_slr_cm),
    pop_exposure_norm = minmax10(pop_density_km2),
    flood_risk_score  = slr_hazard_norm * pop_exposure_norm,
    flood_risk_class  = cut(flood_risk_score,
                             breaks = quantile(flood_risk_score, probs = seq(0, 1, 0.2), na.rm = TRUE),
                             labels = c("Very Low", "Low", "Moderate", "High", "Very High"),
                             include.lowest = TRUE)
  ) %>%
  select(district, lon, lat, district_relative_slr_cm, pop_density_km2,
         slr_hazard_norm, pop_exposure_norm, flood_risk_score, flood_risk_class)

write.csv(flood_risk_2075, file.path(out_dir, "table_flood_risk_2075.csv"), row.names = FALSE)

cat("\n--- 2075 Flood Risk (SLR hazard x population density), SSP5-8.5 ---\n")
print(flood_risk_2075 %>% select(district, flood_risk_score, flood_risk_class) %>%
        arrange(desc(flood_risk_score)))

## ---- 3. CHOROPLETH MAP --------------------------------------------------------
risk_palette <- c("Very Low" = "#ffffcc", "Low" = "#fed976", "Moderate" = "#fd8d3c",
                   "High" = "#e31a1c", "Very High" = "#800026")

if (!is.null(districts_shp)) {
  map_data <- dplyr::left_join(districts_shp, flood_risk_2075, by = "district")
  n_matched <- sum(!is.na(map_data$flood_risk_score))
  if (n_matched == 0) {
    message("[fallback] Shapefile loaded but 0 of its district names matched district_cov$district ",
            "-- check spelling (e.g. \"Cox's Bazar\") -- rendering a point map instead.")
    districts_shp <- NULL
  } else {
    if (n_matched < nrow(flood_risk_2075)) {
      message("[warning] Only ", n_matched, " of ", nrow(flood_risk_2075),
              " districts matched the shapefile's name field -- unmatched polygons will show as grey.")
    }
    p_flood_risk <- ggplot2::ggplot(map_data) +
      ggplot2::geom_sf(ggplot2::aes(fill = flood_risk_class), color = "grey30", linewidth = 0.2) +
      ggplot2::scale_fill_manual(values = risk_palette, na.value = "grey90", drop = FALSE) +
      ggplot2::labs(title = "Projected 2075 Coastal Flood Risk by District (SSP5-8.5)",
                    subtitle = "Risk = normalized SLR hazard x normalized population density (quintile classes)",
                    fill = "Flood risk") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                     panel.grid = ggplot2::element_blank())
  }
}

if (is.null(districts_shp)) {
  p_flood_risk <- ggplot2::ggplot(flood_risk_2075,
                                   ggplot2::aes(lon, lat, color = flood_risk_class, size = flood_risk_score)) +
    ggplot2::geom_point() +
    ggplot2::geom_text(ggplot2::aes(label = district), vjust = -1, size = 3, color = "black") +
    ggplot2::scale_color_manual(values = risk_palette, drop = FALSE) +
    ggplot2::labs(title = "Projected 2075 Coastal Flood Risk by District (SSP5-8.5)",
                  subtitle = paste("Risk = normalized SLR hazard x normalized population density",
                                    "(quintile classes)\n[point-map approximation -- supply the GADM",
                                    "shapefile for true polygons]"),
                  x = "Longitude", y = "Latitude", color = "Flood risk", size = "Risk score") +
    ggplot2::theme_minimal()
}

ggplot2::ggsave(file.path(fig_dir, "fig_flood_risk_2075_projection.png"), p_flood_risk,
                 width = 8, height = 6.5, dpi = 300)

save(flood_risk_2075, districts_shp, file = file.path(out_dir, "script10_workspace.RData"))

cat("\nFlood risk map complete. See table_flood_risk_2075.csv and fig_flood_risk_2075_projection.png\n")
