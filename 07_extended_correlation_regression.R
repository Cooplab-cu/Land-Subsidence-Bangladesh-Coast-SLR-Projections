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

## ---- 0. DEFENSIVE OUTPUT-DIR SETUP -------------------------------------------
## Guards against the "cannot open the connection ... No such file or
## directory" error when this script is sourced by itself (or in a fresh R
## session where 01 wasn't re-run, or the working directory changed) --
## recreates the same out_dir/fig_dir/data_dir script 01 sets up, no-ops if
## they already exist. Always setwd() to the project folder before sourcing.
if (!exists("data_dir")) data_dir <- "data"
if (!exists("out_dir"))  out_dir  <- "outputs"
if (!exists("fig_dir"))  fig_dir  <- file.path(out_dir, "figures")
dir.create(data_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fig_dir,  showWarnings = FALSE, recursive = TRUE)

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
