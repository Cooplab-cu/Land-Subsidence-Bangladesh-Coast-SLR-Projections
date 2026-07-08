##############################################################################
# Study Area Map: Nijhum Dwip, Sandwip, Sitakunda
# নিঝুম দ্বীপ, সন্দ্বীপ, সীতাকুণ্ড — REAL SHAPES (no bbox rectangles)
##############################################################################

# ---- 1. Packages ----------------------------------------------------------
# install.packages(c("sf","ggplot2","ggspatial","geodata","dplyr","cowplot","stringdist","ggrepel"))

library(sf)
library(ggplot2)
library(ggspatial)
library(geodata)
library(dplyr)
library(cowplot)
library(stringdist)
library(ggrepel)

dir.create("data", showWarnings = FALSE)
dir.create("outputs", showWarnings = FALSE)

# ---- 2. Bangladesh admin boundaries (GADM) ---------------------------------
# level 0 = country, level 1 = division, level 2 = district, level 3 = upazila
bd_adm0 <- gadm(country = "BGD", level = 0, path = "data", version = "4.1") |> st_as_sf()
bd_adm2 <- gadm(country = "BGD", level = 2, path = "data", version = "4.1") |> st_as_sf()
bd_adm3 <- gadm(country = "BGD", level = 3, path = "data", version = "4.1") |> st_as_sf()

# ---- 3. Helper: fuzzy-match an upazila name (handles GADM spelling drift) --
find_upazila <- function(name, adm3 = bd_adm3) {
  d <- stringdist(tolower(name), tolower(adm3$NAME_3), method = "lv")
  adm3[which.min(d), ]
}

sandwip_poly   <- find_upazila("Sandwip")
sitakunda_poly <- find_upazila("Sitakunda")
hatiya_poly    <- find_upazila("Hatiya")

cat("Matched -> Sandwip:", sandwip_poly$NAME_3,
    "| Sitakunda:", sitakunda_poly$NAME_3,
    "| Hatiya:", hatiya_poly$NAME_3, "\n")

# ---- 4. Nijhum Dwip: Hatiya upazila clipped to the known char/island bbox --
# (Nijhum Dwip is not its own upazila in GADM — it is a char/union inside
#  Hatiya upazila — so we clip Hatiya's polygon to the given coordinate box
#  to pull out just the southern island landmass.)
nijhum_bbox <- st_bbox(c(xmin = 90.90, xmax = 91.12, ymin = 21.92, ymax = 22.12),
                       crs = st_crs(hatiya_poly))
nijhum_poly <- st_intersection(st_geometry(hatiya_poly), st_as_sfc(nijhum_bbox))
nijhum_poly <- st_sf(NAME_3 = "Nijhum Dwip", geometry = nijhum_poly)

# ---- 5. Assemble the three real-shape study areas --------------------------
study_sf <- bind_rows(
  st_sf(area_bn = "নিঝুম দ্বীপ", area_en = "Nijhum Dwip",
        geometry = st_geometry(nijhum_poly)),
  st_sf(area_bn = "সন্দ্বীপ",    area_en = "Sandwip",
        geometry = st_geometry(sandwip_poly)),
  st_sf(area_bn = "সীতাকুণ্ড",   area_en = "Sitakunda",
        geometry = st_geometry(sitakunda_poly))
) |> st_set_crs(4326)

study_sf$area_en <- factor(study_sf$area_en,
                           levels = c("Nijhum Dwip", "Sandwip", "Sitakunda"))

# centroid for labels
cent <- st_coordinates(st_centroid(study_sf))
study_sf$lon_c <- cent[, 1]
study_sf$lat_c <- cent[, 2]

# ---- 6. Colors (same as before) --------------------------------------------
area_colors <- c(
  "Nijhum Dwip" = "#440154",  # dark purple
  "Sandwip"     = "#21918C",  # teal
  "Sitakunda"   = "#FDE725"   # yellow
)

# ---- 7. Zoom extent for the main map ---------------------------------------
bb <- st_bbox(study_sf)
buf <- 0.30
zoom_xmin <- bb["xmin"] - buf
zoom_xmax <- bb["xmax"] + buf
zoom_ymin <- bb["ymin"] - buf
zoom_ymax <- bb["ymax"] + buf

# ---- 8. Main map: real shapes over Bangladesh coastline --------------------
p_main <- ggplot() +
  geom_sf(data = bd_adm2, fill = "grey90", color = "grey65", linewidth = 0.25) +
  geom_sf(data = bd_adm0, fill = NA, color = "grey30", linewidth = 0.5) +
  geom_sf(data = study_sf, aes(fill = area_en), color = "black", linewidth = 0.5) +
  geom_label_repel(
    data = study_sf,
    aes(x = lon_c, y = lat_c, label = paste0(area_bn, "\n(", area_en, ")")),
    size = 3.3, fontface = "bold", label.size = 0.3,
    seed = 42, min.segment.length = 0, box.padding = 0.6
  ) +
  scale_fill_manual(values = area_colors, name = "Area") +
  coord_sf(xlim = c(zoom_xmin, zoom_xmax), ylim = c(zoom_ymin, zoom_ymax),
           expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering(),
                         height = unit(1.1, "cm"), width = unit(1.1, "cm")) +
  labs(title = "Study Area Map",
       subtitle = "Nijhum Dwip, Sandwip and Sitakunda",
       x = "Longitude", y = "Latitude") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    panel.grid = element_line(color = "grey85", linewidth = 0.2),
    panel.background = element_rect(fill = "#DCEEFB", color = NA),
    legend.position = "right"
  ) +
  annotate("text", x = zoom_xmax - 0.15, y = zoom_ymin + 0.12,
           label = "Bay of Bengal", fontface = "italic",
           color = "#3E6D9C", size = 4.5, hjust = 1)

# ---- 9. Locator inset: whole Bangladesh with the zoom box highlighted -----
zoom_box <- st_as_sfc(st_bbox(c(xmin = zoom_xmin, xmax = zoom_xmax,
                                ymin = zoom_ymin, ymax = zoom_ymax),
                              crs = 4326))

p_inset <- ggplot() +
  geom_sf(data = bd_adm0, fill = "grey85", color = "grey40", linewidth = 0.3) +
  geom_sf(data = zoom_box, fill = "red", color = "red", alpha = 0.55, linewidth = 0.5) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white", color = "black", linewidth = 0.6))

# ---- 10. Combine main + inset (inset placed top-left corner) --------------
final_map <- ggdraw(p_main) +
  draw_plot(p_inset, x = 0.015, y = 0.60, width = 0.24, height = 0.34)

print(final_map)

# ---- 11. Save ---------------------------------------------------------------
ggsave("outputs/study_area_map_nijhum_sandwip_sitakunda.png",
       final_map, width = 9.5, height = 8, dpi = 400, bg = "white")

cat("Map saved to outputs/study_area_map_nijhum_sandwip_sitakunda.png\n")