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

## ---- 0. DEFENSIVE OUTPUT-DIR SETUP -------------------------------------------
## See script 07's note -- recreates out_dir/fig_dir/data_dir if this script
## is sourced standalone or the working directory changed since script 01.
if (!exists("data_dir")) data_dir <- "data"
if (!exists("out_dir"))  out_dir  <- "outputs"
if (!exists("fig_dir"))  fig_dir  <- file.path(out_dir, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,  showWarnings = FALSE, recursive = TRUE)

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
