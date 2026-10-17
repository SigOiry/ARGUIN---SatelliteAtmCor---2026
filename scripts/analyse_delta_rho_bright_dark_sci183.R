#!/usr/bin/env Rscript

# SCI-183: contrast atmospheric-correction products over optically dark and
# bright surfaces. Existing Data/ inputs are read-only.
#
# Usage:
#   Rscript scripts/analyse_delta_rho_bright_dark_sci183.R
#   Rscript scripts/analyse_delta_rho_bright_dark_sci183.R \
#     --project-root=/path/to/project --output-dir=/path/to/output
#
# The surface definition is deliberately optical, not ecological. A pixel's
# brightness score is its across-date median of the symmetric processor mean
# in B02--B04. The primary contrast retains the darkest and brightest thirds;
# sensitivity analyses use 25/75, 40/60, and a median split. Quantiles are
# calculated within each AOI to prevent surface strata from becoming proxies
# for the large baseline brightness difference between Arel and Iwik.

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
arg_value <- function(prefix, default = NULL) {
  hit <- grep(paste0("^", prefix, "="), args, value = TRUE)
  if (!length(hit)) return(default)
  sub(paste0("^", prefix, "="), "", hit[[1]])
}

script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
script_arg <- gsub("~\\+~", " ", script_arg)
default_project <- normalizePath(file.path(dirname(script_arg), ".."), mustWork = TRUE)
project <- normalizePath(arg_value("--project-root", default_project), mustWork = TRUE)
out_dir <- arg_value("--output-dir", file.path(project, "workinprogress", "SCI-183_luna"))
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

input_root <- file.path(project, "Data", "Atm_Compare_AOI")
sen_dir <- file.path(input_root, "Sen2Cor")
aco_dir <- file.path(input_root, "ACOLITE")
stopifnot(dir.exists(sen_dir), dir.exists(aco_dir))

bands <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08",
           "B8A", "B09", "B11", "B12")
visible_bands <- c("B02", "B03", "B04")
band_order <- bands

sen_files <- list.files(sen_dir, pattern = "\\.csv$", full.names = TRUE)
aco_files <- list.files(aco_dir, pattern = "\\.csv$", full.names = TRUE)
common_names <- sort(intersect(basename(sen_files), basename(aco_files)))
if (!length(common_names)) stop("No exact same-date file pairs found")

inventory <- data.table(
  filename = sort(unique(c(basename(sen_files), basename(aco_files))))
)
inventory[, `:=`(
  Sen2Cor = filename %chin% basename(sen_files),
  ACOLITE = filename %chin% basename(aco_files)
)]
inventory[, exact_pair := Sen2Cor & ACOLITE]
fwrite(inventory, file.path(out_dir, "input_file_inventory.csv"))

read_pair <- function(filename) {
  s <- fread(file.path(sen_dir, filename), showProgress = FALSE)
  a <- fread(file.path(aco_dir, filename), showProgress = FALSE)
  required <- c("date", "AOI", "x", "y", bands)
  if (!all(required %chin% names(s))) stop("Missing Sen2Cor columns in ", filename)
  # ACOLITE lacks B09 in this archive; retain bands present in both products.
  common_bands <- intersect(bands, intersect(names(s), names(a)))
  keys <- c("date", "AOI", "x", "y")
  s <- s[, c(keys, common_bands), with = FALSE]
  a <- a[, c(keys, common_bands), with = FALSE]
  setnames(s, common_bands, paste0(common_bands, "_Sen2Cor"))
  setnames(a, common_bands, paste0(common_bands, "_ACOLITE"))
  z <- merge(s, a, by = keys, all = FALSE)
  if (!nrow(z)) stop("No coordinate-level matches in ", filename)
  z[, source_file := filename]
  z
}

wide <- rbindlist(lapply(common_names, read_pair), fill = TRUE)
paired_bands <- bands[paste0(bands, "_Sen2Cor") %chin% names(wide) &
                        paste0(bands, "_ACOLITE") %chin% names(wide)]
if (!all(visible_bands %chin% paired_bands)) stop("Visible brightness bands unavailable")

to_long <- function(processor) {
  cols <- paste0(paired_bands, "_", processor)
  x <- melt(wide, id.vars = c("date", "AOI", "x", "y", "source_file"),
            measure.vars = cols, variable.name = "band", value.name = "rho")
  x[, band := sub(paste0("_", processor, "$"), "", as.character(band))]
  setnames(x, "rho", paste0("rho_", processor))
  x
}
long <- merge(to_long("Sen2Cor"), to_long("ACOLITE"),
              by = c("date", "AOI", "x", "y", "source_file", "band"))
long[, date := as.IDate(date)]
long[, `:=`(
  delta_rho = rho_ACOLITE - rho_Sen2Cor,
  symmetric_rho = (rho_ACOLITE + rho_Sen2Cor) / 2
)]
long <- long[is.finite(rho_Sen2Cor) & is.finite(rho_ACOLITE)]

# Static pixel brightness reduces reclassification caused by individual dates.
brightness <- long[band %chin% visible_bands,
  .(brightness_score = median(symmetric_rho, na.rm = TRUE),
    n_brightness_observations = .N,
    n_brightness_dates = uniqueN(date)), by = .(AOI, x, y)]
stopifnot(all(is.finite(brightness$brightness_score)))
long <- brightness[long, on = .(AOI, x, y)]

threshold_rules <- data.table(
  threshold_id = c("q25_q75", "q33_q67_primary", "q40_q60", "median_split"),
  dark_quantile = c(0.25, 1/3, 0.40, 0.50),
  bright_quantile = c(0.75, 2/3, 0.60, 0.50),
  primary = c(FALSE, TRUE, FALSE, FALSE)
)
threshold_rules[, dummy := 1L]
aoi_table <- unique(brightness[, .(AOI)])
aoi_table[, dummy := 1L]
threshold_specs <- merge(threshold_rules, aoi_table, by = "dummy", allow.cartesian = TRUE)
threshold_specs[, dummy := NULL]
threshold_specs <- threshold_specs[brightness, on = "AOI", allow.cartesian = TRUE][,
  .(dark_quantile = first(dark_quantile), bright_quantile = first(bright_quantile),
    primary = first(primary),
    dark_threshold = quantile(brightness_score, first(dark_quantile), names = FALSE, type = 7),
    bright_threshold = quantile(brightness_score, first(bright_quantile), names = FALSE, type = 7)),
  by = .(threshold_id, AOI)]
threshold_specs <- threshold_specs[brightness, on = "AOI", allow.cartesian = TRUE][,
  .(dark_quantile = first(dark_quantile), bright_quantile = first(bright_quantile),
    primary = first(primary), dark_threshold = first(dark_threshold),
    bright_threshold = first(bright_threshold),
    n_dark_pixels = sum(brightness_score <= first(dark_threshold)),
    n_bright_pixels = sum(brightness_score >= first(bright_threshold))),
  by = .(threshold_id, AOI)]
fwrite(threshold_specs, file.path(out_dir, "bright_dark_thresholds.csv"))
fwrite(brightness, file.path(out_dir, "pixel_brightness_scores.csv"))

classified <- rbindlist(lapply(seq_len(nrow(threshold_specs)), function(i) {
  spec <- threshold_specs[i]
  x <- copy(long[AOI == spec$AOI])
  x[, threshold_id := spec$threshold_id]
  x[, surface_group := fifelse(brightness_score <= spec$dark_threshold, "Dark",
                        fifelse(brightness_score >= spec$bright_threshold, "Bright", "Middle"))]
  x[surface_group != "Middle"]
}))
classified[, surface_group := factor(surface_group, levels = c("Dark", "Bright"))]

summarise_group <- function(x) x[, .(
  n_observations = .N,
  n_pixels = uniqueN(paste(AOI, x, y)),
  n_dates = uniqueN(date),
  rho_Sen2Cor_mean = mean(rho_Sen2Cor),
  rho_Sen2Cor_median = median(rho_Sen2Cor),
  rho_ACOLITE_mean = mean(rho_ACOLITE),
  rho_ACOLITE_median = median(rho_ACOLITE),
  delta_rho_mean = mean(delta_rho),
  delta_rho_sd = sd(delta_rho),
  delta_rho_median = median(delta_rho),
  delta_rho_q25 = quantile(delta_rho, 0.25),
  delta_rho_q75 = quantile(delta_rho, 0.75)
), by = .(threshold_id, surface_group, band)]

summary_table <- summarise_group(classified)
summary_table[, band := factor(band, band_order)]
setorder(summary_table, threshold_id, band, surface_group)
fwrite(summary_table, file.path(out_dir, "delta_rho_bright_dark_summary.csv"))

# Date is the resampling unit because pixels are spatially repeated and highly
# autocorrelated. These intervals represent temporal sampling uncertainty; they
# are not pixel-independent confidence intervals.
date_level <- classified[, .(median_delta_rho = median(delta_rho)),
                         by = .(threshold_id, surface_group, band, date, AOI)]
contrast_by_date_aoi <- dcast(date_level, threshold_id + band + date + AOI ~ surface_group,
                          value.var = "median_delta_rho")
contrast_by_date_aoi <- contrast_by_date_aoi[is.finite(Dark) & is.finite(Bright)]
contrast_by_date_aoi[, dark_minus_bright := Dark - Bright]
contrast_by_date <- contrast_by_date_aoi[, .(
  Dark = mean(Dark), Bright = mean(Bright),
  dark_minus_bright = mean(dark_minus_bright), n_AOIs = .N
), by = .(threshold_id, band, date)]

set.seed(183)
bootstrap_contrast <- function(v, n_boot = 2000L) {
  n <- length(v)
  boot <- replicate(n_boot, median(sample(v, n, replace = TRUE)))
  c(estimate = median(v), ci_low = quantile(boot, 0.025, names = FALSE),
    ci_high = quantile(boot, 0.975, names = FALSE))
}
contrast_summary <- contrast_by_date[, as.list(bootstrap_contrast(dark_minus_bright)),
                                     by = .(threshold_id, band)]
contrast_summary[, `:=`(n_dates = contrast_by_date[, .N, by = .(threshold_id, band)]$N,
                         bootstrap_unit = "date", bootstrap_replicates = 2000L)]
contrast_summary[, band := factor(band, band_order)]
setorder(contrast_summary, threshold_id, band)
fwrite(contrast_summary, file.path(out_dir, "delta_rho_dark_minus_bright_contrasts.csv"))
fwrite(contrast_by_date, file.path(out_dir, "delta_rho_date_level_contrasts.csv"))
fwrite(contrast_by_date_aoi, file.path(out_dir, "delta_rho_date_aoi_level_contrasts.csv"))

primary <- classified[threshold_id == "q33_q67_primary"]
plot_delta <- ggplot(primary, aes(surface_group, delta_rho, fill = surface_group)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_boxplot(outlier.shape = NA, width = 0.65) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 1.6,
               fill = "white", colour = "black") +
  facet_wrap(~factor(band, band_order), scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(Dark = "#213B65", Bright = "#E6B85C"), guide = "none") +
  labs(x = NULL, y = expression(Delta*rho == rho[ACOLITE] - rho[Sen2Cor]),
       title = "Atmospheric-correction difference by optical surface stratum",
       subtitle = "Primary definition: darkest and brightest thirds of stable visible brightness; boxes pool dates and pixels") +
  theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), strip.background = element_rect(fill = "grey95"))

method_long <- melt(primary,
  id.vars = c("surface_group", "band", "date", "AOI", "x", "y"),
  measure.vars = c("rho_Sen2Cor", "rho_ACOLITE"),
  variable.name = "processor", value.name = "rho")
method_long[, processor := sub("^rho_", "", processor)]
plot_methods <- ggplot(method_long, aes(processor, rho, fill = surface_group)) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.7), width = 0.62) +
  facet_wrap(~factor(band, band_order), scales = "free_y", ncol = 4) +
  scale_fill_manual(values = c(Dark = "#213B65", Bright = "#E6B85C"), name = "Optical stratum") +
  labs(x = NULL, y = expression(rho),
       title = "Reflectance distributions by processor and optical stratum",
       subtitle = "Descriptive distributions; repeated pixels and dates are not independent") +
  theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 25, hjust = 1),
        legend.position = "bottom", strip.background = element_rect(fill = "grey95"))

plot_sensitivity <- ggplot(contrast_summary,
  aes(factor(band, band_order), estimate, ymin = ci_low, ymax = ci_high,
      colour = threshold_id, group = threshold_id)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.3) +
  geom_pointrange(position = position_dodge(width = 0.55), linewidth = 0.35) +
  scale_colour_brewer(palette = "Dark2", name = "Threshold rule") +
  labs(x = "Band", y = "Median date-level Δρ contrast (dark − bright)",
       title = "Sensitivity of the dark–bright contrast to threshold choice",
       subtitle = "Points: median across dates; bars: 95% date-bootstrap intervals (2,000 resamples)") +
  theme_bw(base_size = 9) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

save_plot <- function(plot, stem, width, height) {
  ggsave(file.path(out_dir, paste0(stem, ".png")), plot, width = width,
         height = height, units = "mm", dpi = 300, bg = "white")
  ggsave(file.path(out_dir, paste0(stem, ".svg")), plot, width = width,
         height = height, units = "mm", bg = "white")
}
save_plot(plot_delta, "DeltaRhoBrightDark_Fig901", 183, 160)
save_plot(plot_methods, "ProcessorReflectanceBrightDark_Fig902", 183, 160)
save_plot(plot_sensitivity, "ThresholdSensitivity_Fig903", 183, 105)

run_metadata <- c(
  paste0("run_time_utc: ", format(Sys.time(), tz = "UTC", usetz = TRUE)),
  paste0("git_commit: ", system2("git", c("-C", shQuote(project), "rev-parse", "HEAD"), stdout = TRUE)),
  paste0("input_root: Data/Atm_Compare_AOI"),
  paste0("sen2cor_files: ", length(sen_files)),
  paste0("acolite_files: ", length(aco_files)),
  paste0("exact_same_date_pairs: ", length(common_names)),
  paste0("paired_bands: ", paste(paired_bands, collapse = ",")),
  paste0("paired_rows_wide: ", nrow(wide)),
  paste0("unique_spatial_pixels: ", nrow(brightness)),
  "delta_definition: rho_ACOLITE - rho_Sen2Cor",
  "brightness_definition: per-pixel across-date median of symmetric processor mean across B02,B03,B04",
  "primary_threshold: within-AOI darkest and brightest thirds; middle third excluded",
  "sensitivity_thresholds: within-AOI q25/q75, q40/q60, and median split",
  "uncertainty: nonparametric bootstrap of date-level median dark-minus-bright contrasts; 2000 resamples; seed 183",
  "ecological_scope: optical darkness is not a validated seagrass label",
  capture.output(sessionInfo())
)
writeLines(run_metadata, file.path(out_dir, "RUN_METADATA.txt"))

message("SCI-183 outputs written to: ", normalizePath(out_dir))
