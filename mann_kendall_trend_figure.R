
## =============================================================================

if (!require("ggplot2", quietly = TRUE)) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("tidyr",   quietly = TRUE)) install.packages("tidyr",   repos = "https://cloud.r-project.org")
if (!require("dplyr",   quietly = TRUE)) install.packages("dplyr",   repos = "https://cloud.r-project.org")
if (!require("patchwork", quietly = TRUE)) install.packages("patchwork", repos = "https://cloud.r-project.org")
library(ggplot2)
library(tidyr)
library(dplyr)
library(patchwork)

data_dir <- file.path(Sys.getenv("USERPROFILE"), "Desktop", "Sealevel Rise", "Statistical Data")
out_dir  <- data_dir

if (!exists("results")) {
  results <- read.csv(file.path(out_dir, "table1_mk_sen_trends_MODIFIED.csv"), stringsAsFactors = FALSE)
}

station_order <- results$station
results$station <- factor(results$station, levels = station_order)


tau_long <- results %>%
  select(station, std_mk_tau, mmk_tau, tfpw_tau) %>%
  pivot_longer(-station, names_to = "method", values_to = "tau") %>%
  mutate(method = recode(method,
                         std_mk_tau = "Standard MK",
                         mmk_tau    = "Modified MK\n(Hamed & Rao 1998)",
                         tfpw_tau   = "Trend-free\npre-whitening"
  ))
tau_long$method <- factor(tau_long$method,
                          levels = c("Standard MK", "Modified MK\n(Hamed & Rao 1998)", "Trend-free\npre-whitening"))

p1 <- ggplot(tau_long, aes(x = station, y = tau, fill = method)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  scale_fill_manual(values = c("#8c9eff", "#3f51b5", "#1a237e")) +
  labs(title = "Trend statistic (tau) \u2014 robust to serial-correlation correction",
       x = NULL, y = "Kendall's tau", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.78, 0.28),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.6, "lines"),
        plot.title = element_text(size = 11))


slope_long <- results %>%
  select(station, mmk_sen_slope_mm_yr, tfpw_sen_slope_mm_yr) %>%
  pivot_longer(-station, names_to = "method", values_to = "slope") %>%
  mutate(method = recode(method,
                         mmk_sen_slope_mm_yr  = "Modified MK",
                         tfpw_sen_slope_mm_yr = "Trend-free pre-whitening"
  ))

p2 <- ggplot(slope_long, aes(x = station, y = slope, fill = method)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.2f", slope),
                vjust = ifelse(slope >= 0, -0.4, 1.2)),
            position = position_dodge(width = 0.7), size = 3) +
  scale_fill_manual(values = c("#3f51b5", "#1a237e")) +
  labs(title = "Corrected sea-level trend rate (Sen's slope)",
       x = NULL, y = "Sen's slope (mm/year)", fill = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top",
        plot.title = element_text(size = 11))


n_text <- paste0("n = ", paste(results$n_years, collapse = ", "),
                 " years respectively. Significance unchanged after ",
                 "serial-correlation correction at all ",
                 sum(!results$significance_changed), " of ", nrow(results), " stations.")

combined <- (p1 | p2) +
  plot_annotation(
    title = " Mann-Kendall Trend Analysis \u2014 Bangladesh Tide Gauges (PSMSL)",
    caption = n_text,
    theme = theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                  plot.caption = element_text(size = 9, hjust = 0.5, face = "italic"))
  )

ggsave(file.path(out_dir, "mk_results_figure.png"), combined,
       width = 12, height = 5, dpi = 200, bg = "white")

cat("Saved figure to:", file.path(out_dir, "mk_results_figure.png"), "\n")
print(combined)