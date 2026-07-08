# ============================================================
# Bangladesh Coastal Sea Level Rise — PSMSL Tide Gauge Records
# Reproduces the cumulative SLR anomaly chart at 600 DPI
# ============================================================

library(readxl)
library(ggplot2)
library(scales)
library(dplyr)

# ---- 0. Auto-set working directory to this script's folder ---------
# Works when you click the "Source" button in RStudio (with this file
# saved as PSML.R directly inside the "Sealevel Rise" folder).
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
  if (nzchar(script_dir)) setwd(script_dir)
}
cat("Working directory is now:", getwd(), "\n")

# ---- 1. Load data --------------------------------------------------
# Assumes your R working directory is set to: Desktop/Sealevel Rise
# (In RStudio: Session > Set Working Directory > To Source File Location,
#  as long as this script is saved directly inside the "Sealevel Rise" folder)
data_path <- "Statistical Data/PSMSL_tidegage_data.xlsx"
df <- read_excel(data_path, sheet = "PSMSL Annual Data")

# Keep only the columns we need and enforce factor order/colors
df <- df %>%
  select(year, station_name, slr_anomaly_cm) %>%
  mutate(station_name = factor(station_name,
                               levels = c("Charchanga", "Cox's Bazar", "Hiron Point")))

# ---- 2. Station colors (matches original chart) --------------------
station_colors <- c(
  "Charchanga"  = "#4C9A6A",   # green
  "Cox's Bazar" = "#B23A3A",   # red
  "Hiron Point" = "#2C5F8A"    # blue
)

# ---- 3. Build the plot ---------------------------------------------
p <- ggplot(df, aes(x = year, y = slr_anomaly_cm, color = station_name)) +
  geom_line(linewidth = 0.5, alpha = 0.9) +
  geom_point(size = 1.3) +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed",
              linewidth = 0.7, alpha = 0.15) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey50", linewidth = 0.4) +
  scale_color_manual(values = station_colors, name = "Tide Gauge Station") +
  scale_fill_manual(values = station_colors, guide = "none") +
  scale_x_continuous(breaks = seq(1970, 2025, 5)) +
  labs(
    title = "Bangladesh Coastal Sea Level Rise — PSMSL Tide Gauge Records",
    subtitle = "Annual mean sea level anomaly relative to 1970–1979 station baseline",
    x = "Year",
    y = "Cumulative SLR anomaly (cm)",
    caption = "Source: Permanent Service for Mean Sea Level (PSMSL) — www.psmsl.org\nRLR (Revised Local Reference) annual mean sea level data\nDashed lines: ordinary least squares linear trend with 95% CI ribbon"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", color = "#1F3864", size = 15),
    plot.subtitle = element_text(color = "grey30", size = 10),
    plot.caption = element_text(color = "grey50", size = 7, hjust = 0),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3)
  )

# ---- 4. Save at 600 DPI ---------------------------------------------
out_path <- "Figures/Bangladesh_SLR_PSMSL_600dpi.png"

ggsave(
  filename = out_path,
  plot = p,
  width = 10, height = 6.2, units = "in",
  dpi = 600
)

if (file.exists(out_path)) {
  cat("\n✅ Saved successfully to:\n", normalizePath(out_path), "\n")
} else {
  cat("\n❌ Save failed — file not found at:\n", file.path(getwd(), out_path), "\n")
}