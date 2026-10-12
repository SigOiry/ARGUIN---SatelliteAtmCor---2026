#!/usr/bin/env Rscript

# Publication figures for the 27 March 2026 field--Sentinel-2 matchup.
# The processing intentionally follows scripts/Atmospheric_correction.qmd:
# field spectra are convolved with the official Sentinel-2B spectral response
# functions, satellite values are extracted at station coordinates, and AC03
# is omitted from the matchup pending documentation of the field QC decision.

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(MapRs)
  library(patchwork)
  library(readxl)
  library(stringr)
  library(terra)
  library(tidyr)
})

dir.create("figures", showWarnings = FALSE, recursive = TRUE)

station_key <- function(x) str_remove_all(str_to_upper(x), "_")

target_labels <- c(
  dry_sand = "Dry sand",
  mbp_mud = "Microphytobenthos mud",
  mpb_mud = "Microphytobenthos mud",
  algae_mud = "Algal mud",
  white_sand = "White sand",
  seagrass = "Seagrass"
)

format_target <- function(x) {
  out <- unname(target_labels[str_to_lower(str_trim(x))])
  ifelse(is.na(out), str_to_sentence(str_replace_all(x, "_", " ")), out)
}

station_palette <- c(
  AC01 = "#000000",
  AC02 = "#E69F00",
  AC03 = "#56B4E9",
  AC04 = "#009E73",
  AC05 = "#F0E442",
  AC06 = "#0072B2",
  AC07 = "#D55E00",
  AC08 = "#CC79A7"
)

reflectance_avg <- read.table(
  "Data/ASD/202603_Reflectance_avg.txt",
  header = TRUE,
  check.names = FALSE
)

asd_log <- read.delim(
  "Data/ASD/202603_Seagrass_cover.txt",
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

gps_log <- read_excel("Data/GPS/Info spectral measure_biomass core_BdA 2026.xlsx")

ac_lookup <- asd_log |>
  transmute(
    station = station,
    station_key = station_key(station),
    target = format_target(comment)
  ) |>
  filter(str_detect(station, "^AC0[1-8]$")) |>
  distinct(station_key, .keep_all = TRUE)

ac_hyper <- reflectance_avg |>
  select(wl, matches("^AC_", ignore.case = TRUE)) |>
  pivot_longer(-wl, names_to = "station_column", values_to = "reflectance") |>
  mutate(station_key = station_key(station_column)) |>
  inner_join(ac_lookup, by = "station_key") |>
  mutate(
    station = factor(station, levels = paste0("AC0", 1:8)),
    station_target = paste0(station, ": ", target)
  )

srf_path <- "Data/COPE-GSEG-EOPG-TN-15-0007 - Sentinel-2 Spectral Response Functions 2024 - 4.0.xlsx"
s2b_srf <- read_excel(srf_path, sheet = "Spectral Responses (S2B)")
s2b_centres <- read_excel(
  srf_path,
  sheet = "Equivalent Wavelengths",
  skip = 1,
  col_names = c("band", "S2A", "S2B", "S2C", "blank1", "blank2", "definition")
) |>
  filter(str_detect(band, "^B")) |>
  transmute(
    band = str_replace(band, "^B0", "B"),
    band_center = as.numeric(S2B)
  )

s2b_srf_long <- s2b_srf |>
  rename(wl = SR_WL) |>
  pivot_longer(-wl, names_to = "band_column", values_to = "srf") |>
  mutate(band = str_remove(band_column, "^S2B_SR_AV_"))

asd_wl_min <- min(ac_hyper$wl, na.rm = TRUE)
asd_wl_max <- max(ac_hyper$wl, na.rm = TRUE)

usable_bands <- s2b_srf_long |>
  group_by(band) |>
  summarise(
    overlap_weight = sum(ifelse(between(wl, asd_wl_min, asd_wl_max), srf, 0), na.rm = TRUE),
    .groups = "drop"
  ) |>
  filter(overlap_weight > 0) |>
  inner_join(s2b_centres, by = "band") |>
  arrange(band_center)

ac_resampled <- ac_hyper |>
  select(station, target, wl, reflectance) |>
  inner_join(
    s2b_srf_long |>
      inner_join(usable_bands |> select(band, band_center), by = "band") |>
      filter(between(wl, asd_wl_min, asd_wl_max)),
    by = "wl",
    relationship = "many-to-many"
  ) |>
  group_by(station, target, band, band_center) |>
  summarise(
    reflectance = sum(reflectance * srf, na.rm = TRUE) / sum(srf, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    band = factor(band, levels = usable_bands$band),
    station_target = paste0(station, ": ", target)
  )

publication_theme <- theme_minimal(base_family = "Arial", base_size = 9.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.25, colour = "#E7E7E7"),
    axis.title = element_text(face = "plain", colour = "#222222"),
    axis.text = element_text(colour = "#333333"),
    plot.title = element_text(face = "bold", size = 10.5, colour = "#111111"),
    plot.margin = margin(6, 8, 5, 6),
    legend.title = element_text(face = "bold"),
    legend.key.width = grid::unit(12, "pt"),
    legend.spacing.x = grid::unit(4, "pt")
  )

station_legend <- ac_lookup |>
  arrange(station) |>
  transmute(station, label = station) |>
  deframe()

figure2a <- ggplot(
  ac_hyper,
  aes(wl, reflectance, colour = station, group = station)
) +
  geom_line(linewidth = 0.7, alpha = 0.95) +
  scale_colour_manual(values = station_palette, labels = station_legend, drop = FALSE) +
  scale_x_continuous(breaks = seq(400, 900, 100), expand = expansion(mult = c(0.005, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    title = "a  Field hyperspectral measurements",
    x = "Wavelength (nm)",
    y = "Reflectance",
    colour = "Validation station"
  ) +
  publication_theme +
  theme(legend.position = "none")

figure2b <- ggplot(
  ac_resampled,
  aes(band_center, reflectance, colour = station, group = station)
) +
  geom_line(linewidth = 0.65, alpha = 0.9) +
  geom_point(size = 1.8, stroke = 0.25) +
  scale_colour_manual(values = station_palette, labels = station_legend, drop = FALSE) +
  scale_x_continuous(
    breaks = usable_bands$band_center,
    labels = usable_bands$band,
    expand = expansion(mult = c(0.02, 0.03))
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.01, 0.05))) +
  labs(
    title = "b  Convolution to Sentinel-2B bands",
    x = "MSI band",
    y = "Reflectance",
    colour = "Validation station"
  ) +
  publication_theme +
  theme(
    axis.text.x = element_text(size = 8),
    legend.position = "none",
    legend.box = "vertical"
  )

legend_data <- tidyr::crossing(
  station = factor(paste0("AC0", 1:8), levels = paste0("AC0", 1:8)),
  x = 1:2
) |>
  mutate(y = 1)

legend_plot <- ggplot(legend_data, aes(x, y, colour = station)) +
  geom_line(aes(group = station), linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = station_palette, labels = station_legend, drop = FALSE) +
  labs(colour = "Validation station") +
  theme_void(base_family = "Arial", base_size = 9.5) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.key.width = grid::unit(12, "pt")
  ) +
  guides(colour = guide_legend(nrow = 1, byrow = TRUE))

legend_grob <- cowplot::get_legend(legend_plot)

figure2_panels <- figure2a + figure2b + plot_layout(widths = c(1.03, 1))
figure2 <- figure2_panels / wrap_elements(full = legend_grob) +
  plot_layout(heights = c(1, 0.10))

ggsave(
  "figures/figure2_field_spectra.png",
  figure2,
  width = 183,
  height = 122,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  "figures/figure2_field_spectra.svg",
  figure2,
  width = 183,
  height = 122,
  units = "mm",
  bg = "white"
)

if ("--fig2-only" %in% commandArgs(trailingOnly = TRUE)) {
  quit(save = "no", status = 0)
}

# Exact point matchup used in the exploratory Quarto analysis.
ac_points <- gps_log |>
  transmute(
    station = station,
    target = format_target(comment),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  ) |>
  filter(station %in% paste0("AC0", 1:8), !is.na(lon), !is.na(lat)) |>
  distinct(station, .keep_all = TRUE) |>
  arrange(station)

safe_path <- file.path(
  "Data",
  "S2",
  "S2B_MSIL2A_20260327T113319_N0512_R080_T28QCH_20260327T152639.SAFE"
)
s2_image <- MapRs::Read_S2(safe_path)
s2_band_lookup <- tibble(
  raster_band = names(s2_image),
  band = str_replace(names(s2_image), "^B0", "B")
)
common_bands <- usable_bands |>
  inner_join(s2_band_lookup, by = "band") |>
  arrange(band_center)

points_vect <- vect(ac_points, geom = c("lon", "lat"), crs = "EPSG:4326")
points_vect <- project(points_vect, crs(s2_image))
s2_values <- terra::extract(s2_image[[common_bands$raster_band]], points_vect) |>
  as_tibble()
names(s2_values) <- c("ID", str_replace(names(s2_values)[-1], "^B0", "B"))

sen2cor_spectra <- bind_cols(ac_points, s2_values |> select(-ID)) |>
  pivot_longer(all_of(common_bands$band), names_to = "band", values_to = "satellite_reflectance") |>
  mutate(satellite_reflectance = (satellite_reflectance - 1000) / 10000) |>
  inner_join(common_bands |> select(band, band_center), by = "band")

acolite_files <- list.files(
  "Data/S2/ACOLITE/20260327",
  pattern = "rhos_[0-9.]+\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)
acolite_lookup <- tibble(
  raster_file = acolite_files,
  wavelength = as.numeric(str_match(basename(acolite_files), "rhos_([0-9.]+)\\.tif$")[, 2])
) |>
  filter(is.finite(wavelength)) |>
  rowwise() |>
  mutate(
    nearest = which.min(abs(usable_bands$band_center - wavelength)),
    band = usable_bands$band[[nearest]],
    band_center = usable_bands$band_center[[nearest]],
    band_delta = abs(band_center - wavelength)
  ) |>
  ungroup() |>
  filter(band_delta <= 5) |>
  arrange(band_center) |>
  distinct(band, .keep_all = TRUE)

acolite_image <- rast(acolite_lookup$raster_file)
names(acolite_image) <- acolite_lookup$band
acolite_points <- project(points_vect, crs(acolite_image))
acolite_values <- terra::extract(acolite_image, acolite_points) |> as_tibble()
names(acolite_values) <- c("ID", acolite_lookup$band)

acolite_spectra <- bind_cols(ac_points, acolite_values |> select(-ID)) |>
  pivot_longer(all_of(acolite_lookup$band), names_to = "band", values_to = "satellite_reflectance") |>
  inner_join(acolite_lookup |> select(band, band_center), by = "band")

asd_matchup <- ac_resampled |>
  select(station, target, band, band_center, asd_reflectance = reflectance) |>
  mutate(station = as.character(station), band = as.character(band))

make_matchup <- function(satellite_data, correction) {
  asd_matchup |>
    inner_join(
      satellite_data |> select(station, target, band, band_center, satellite_reflectance),
      by = c("station", "target", "band", "band_center")
    ) |>
    filter(station != "AC03", is.finite(asd_reflectance), is.finite(satellite_reflectance)) |>
    mutate(correction = correction)
}

matchup <- bind_rows(
  make_matchup(sen2cor_spectra, "Sen2Cor"),
  make_matchup(acolite_spectra, "ACOLITE")
) |>
  mutate(
    correction = factor(correction, levels = c("Sen2Cor", "ACOLITE")),
    band = factor(band, levels = usable_bands$band)
  )

metric_summary <- function(data) {
  difference <- data$satellite_reflectance - data$asd_reflectance
  tibble(
    n = nrow(data),
    r2 = summary(lm(satellite_reflectance ~ asd_reflectance, data = data))$r.squared,
    rmsd = sqrt(mean(difference^2)),
    bias = mean(difference),
    mad = mean(abs(difference)),
    rpd = mean(difference / data$asd_reflectance) * 100,
    apd = mean(abs(difference) / abs(data$asd_reflectance)) * 100
  )
}

metrics <- matchup |>
  group_by(correction) |>
  group_modify(~ metric_summary(.x)) |>
  ungroup()

write.csv(matchup, "figures/figure3_matchup_data.csv", row.names = FALSE)
write.csv(metrics, "figures/figure3_metrics.csv", row.names = FALSE)

band_palette <- c(
  B1 = "#4D4D4D",
  B2 = "#0072B2",
  B3 = "#009E73",
  B4 = "#D55E00",
  B5 = "#E69F00",
  B6 = "#CC79A7",
  B7 = "#8E6C8A",
  B8 = "#6A3D9A",
  B8A = "#3B1B64"
)

target_shapes <- c(
  "Dry sand" = 21,
  "Microphytobenthos mud" = 22,
  "Algal mud" = 23,
  "White sand" = 24,
  "Seagrass" = 25
)

axis_min <- min(c(0, matchup$asd_reflectance, matchup$satellite_reflectance), na.rm = TRUE)
axis_max <- max(c(matchup$asd_reflectance, matchup$satellite_reflectance), na.rm = TRUE)
axis_pad <- (axis_max - axis_min) * 0.04
axis_limits <- c(axis_min - axis_pad, axis_max + axis_pad)

metrics_for_plot <- metrics |>
  mutate(
    label = sprintf(
      "R^2 = %.3f\nRMSD = %.3f\nBias = %+.3f\nMAD = %.3f\nAPD = %.1f%%",
      r2, rmsd, bias, mad, apd
    ),
    x = axis_limits[1] + 0.035 * diff(axis_limits),
    y = axis_limits[2] - 0.035 * diff(axis_limits)
  )

figure3 <- ggplot(
  matchup,
  aes(asd_reflectance, satellite_reflectance, colour = band, shape = target)
) +
  geom_abline(slope = 1, intercept = 0, linewidth = 0.65, linetype = "22", colour = "#666666") +
  geom_smooth(
    aes(x = asd_reflectance, y = satellite_reflectance, group = correction),
    inherit.aes = FALSE,
    method = "lm",
    formula = y ~ x,
    se = FALSE,
    linewidth = 0.85,
    colour = "#111111"
  ) +
  geom_point(size = 2.45, stroke = 0.7, fill = "white", alpha = 0.94) +
  geom_label(
    data = metrics_for_plot,
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    size = 2.75,
    lineheight = 0.95,
    linewidth = 0,
    label.padding = grid::unit(3.5, "pt"),
    fill = scales::alpha("white", 0.88),
    colour = "#222222"
  ) +
  facet_wrap(
    ~ correction,
    nrow = 1,
    labeller = as_labeller(c(Sen2Cor = "a  Sen2Cor", ACOLITE = "b  ACOLITE"))
  ) +
  coord_equal(xlim = axis_limits, ylim = axis_limits, expand = FALSE) +
  scale_colour_manual(values = band_palette, drop = FALSE) +
  scale_shape_manual(values = target_shapes, drop = FALSE) +
  scale_x_continuous(breaks = scales::breaks_pretty(5)) +
  scale_y_continuous(breaks = scales::breaks_pretty(5)) +
  labs(
    x = "ASD reflectance resampled to Sentinel-2B",
    y = "Satellite-derived reflectance",
    colour = "MSI band",
    shape = "Validation surface"
  ) +
  publication_theme +
  theme(
    panel.grid.major = element_line(linewidth = 0.28, colour = "#E4E4E4"),
    panel.border = element_rect(fill = NA, linewidth = 0.45, colour = "#333333"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 10.5, hjust = 0),
    legend.position = "bottom",
    legend.box = "vertical",
    plot.margin = margin(5, 8, 3, 5)
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE, order = 1, override.aes = list(shape = 16, size = 2.5)),
    shape = guide_legend(nrow = 1, byrow = TRUE, order = 2, override.aes = list(colour = "#333333"))
  )

ggsave(
  "figures/figure3_asd_satellite_matchup.png",
  figure3,
  width = 183,
  height = 122,
  units = "mm",
  dpi = 600,
  bg = "white"
)
ggsave(
  "figures/figure3_asd_satellite_matchup.svg",
  figure3,
  width = 183,
  height = 122,
  units = "mm",
  bg = "white"
)

print(metrics)
