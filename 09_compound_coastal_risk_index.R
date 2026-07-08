## =============================================================================
## SCRIPT 9 of 9: Compound Coastal Risk Index (SLR + rainfall + SST + discharge)
##   -> Combines `district_forecast` (script 06: SLR + subsidence, by district/
##      year/SSP) with `climate_covariates_by_ssp` (script 08: SSP-scaled SST,
##      rainfall, river discharge) into a single compound risk score per
##      district/year/scenario, instead of ranking districts by SLR alone.
## Run scripts 01-08 first (needs `district_forecast`, `climate_covariates_by_ssp`,
## `out_dir`, `fig_dir`).
## =============================================================================

## ---- 0. DEFENSIVE OUTPUT-DIR SETUP -------------------------------------------
## See script 07's note -- recreates out_dir/fig_dir/data_dir if this script
## is sourced standalone or the working directory changed since script 01.
if (!exists("data_dir")) data_dir <- "data"
if (!exists("out_dir"))  out_dir  <- "outputs"
if (!exists("fig_dir"))  fig_dir  <- file.path(out_dir, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,  showWarnings = FALSE, recursive = TRUE)

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
