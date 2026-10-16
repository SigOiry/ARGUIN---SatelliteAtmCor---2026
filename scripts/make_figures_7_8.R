#!/usr/bin/env Rscript

# SCI-132: publication Figures 7 and 8 from validated SCI-121 products.
# Reproduces the approved SCI-123 iteration-7 design at 600 dpi and also
# exports editable SVG and vector PDF versions.
# Delta rho is unitless rho_ACOLITE - rho_Sen2Cor. No temporal smoothing is used.

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(readr)
  library(ragg); library(scales); library(svglite); library(terra); library(tidyr)
})

iteration <- 7L
script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value=TRUE))
# Rscript encodes spaces as ~+~ in --file when invoked through this WSL mount.
script_arg <- gsub("~\\+~", " ", script_arg)
script_dir <- dirname(normalizePath(script_arg, mustWork=TRUE))
project <- normalizePath(file.path(script_dir, ".."), mustWork=TRUE)
input_dir <- file.path(project, "workinprogress", "SCI-121_luna", "outputs", "deltarho_timeseries")
out_dir <- file.path(project, "workinprogress", "SCI-132_frida", "final")
dir.create(out_dir, recursive=TRUE, showWarnings=FALSE)

bands <- c("B02","B03","B04","B08")
wavelengths <- c(B02=492.34, B03=558.95, B04=664.56, B08=832.88)
band_names <- c(B02="Blue", B03="Green", B04="Red", B08="NIR")
band_labels <- setNames(sprintf("%s · %s · %.0f nm", bands, band_names, wavelengths), bands)
map_band_labels <- setNames(sprintf("%s - %.0f nm", bands, wavelengths), bands)
# Exact project colour mapping inherited from Figures 3--4 (B08 is black for >800 nm).
band_colours <- c(B02="#6999FE", B03="#A4D26F", B04="#E47121", B08="#111111")

base_size <- c(`1`=7.5, `2`=8.0, `3`=8.4, `4`=8.4,
               `5`=8.4, `6`=8.4, `7`=8.4, `8`=8.4,
               `9`=8.4, `10`=8.4)[as.character(iteration)]
display_factor <- c(`1`=8L, `2`=6L, `3`=5L, `4`=5L,
                    `5`=5L, `6`=5L, `7`=5L, `8`=5L,
                    `9`=5L, `10`=5L)[as.character(iteration)]
theme_pub <- theme_classic(base_size=base_size) + theme(
  text=element_text(colour="#242424"),
  axis.text=element_text(size=base_size-1, colour="#343434"),
  axis.title=element_text(size=base_size),
  strip.background=element_rect(fill="#F2F2F2", colour=NA),
  strip.text=element_text(face="bold", size=base_size),
  plot.title=element_text(face="bold", size=base_size),
  legend.title=element_text(face="bold", size=base_size-.2),
  legend.text=element_text(size=base_size-1),
  plot.margin=margin(3,4,3,4)
)

read_metric <- function(metric) {
  files <- file.path(input_dir, "aggregate_rasters",
                     paste0(metric, "_delta_rho_", bands, "_10m.tif"))
  stopifnot(all(file.exists(files)))
  x <- rast(files); names(x) <- bands
  x <- aggregate(x, fact=display_factor, fun=mean, na.rm=TRUE)
  as.data.frame(x, xy=TRUE, na.rm=FALSE) |>
    pivot_longer(all_of(bands), names_to="band", values_to="value") |>
    filter(is.finite(value)) |>
    mutate(metric=metric, easting_km=x/1000, northing_km=y/1000)
}
map_df <- bind_rows(read_metric("median"), read_metric("sd"))

# Shared limits within each metric permit direct comparison across the four bands.
# Iteration 3 uses robust limits to retain spatial structure; clipped tails are explicit.
median_limit <- if (iteration == 1) .06 else if (iteration == 2) .05 else .045
sd_limit <- if (iteration == 1) .05 else if (iteration == 2) .04 else if (iteration <= 7) .035 else .02
median_cols <- colorspace::divergingx_hcl(15, palette="RdBu", rev=TRUE)
sd_cols <- c("#2166AC", "#FFFFFF", "#B2182B")

make_map <- function(band, metric) {
  dat <- filter(map_df, .data$band==.env$band, .data$metric==.env$metric)
  if (metric == "median") {
    sc <- scale_fill_gradientn(colours=median_cols, limits=c(-median_limit,median_limit),
      oob=squish, breaks=c(-median_limit,0,median_limit),
      labels=c(sprintf("≤−.%.3d",round(median_limit*1000)),"0",
               sprintf("≥+.%.3d",round(median_limit*1000))),
      name="Median Δρ")
  } else {
    sc <- scale_fill_gradient2(low=sd_cols[[1]], mid=sd_cols[[2]], high=sd_cols[[3]],
      midpoint=if (iteration <= 7) .018 else .01, limits=c(0,sd_limit), oob=squish,
      breaks=c(0,if (iteration <= 7) .018 else .01,sd_limit),
      labels=c("0",if (iteration <= 7) ".018" else ".010",
               sprintf("≥.%.3d",round(sd_limit*1000))),
      name="SD of Δρ")
  }
  show_coords <- identical(metric,"sd") && identical(band,"B02")
  panel_label <- if (iteration >= 8) {
    annotate("label", x=Inf, y=Inf, label=map_band_labels[[band]], hjust=1.06, vjust=1.12,
             size=base_size/ggplot2::.pt, fontface="bold", colour="#242424",
             fill=scales::alpha("white",.82), label.size=0, label.padding=unit(.8,"mm"))
  } else NULL
  ggplot(dat, aes(easting_km,northing_km,fill=value)) + geom_raster() + sc + panel_label +
    scale_x_continuous(
      labels=if (show_coords) waiver() else NULL,
      breaks=if (show_coords) waiver() else NULL,
      expand=expansion(mult=if (show_coords) c(.002,.03) else c(.002,.012))) +
    scale_y_continuous(labels=if (show_coords) waiver() else NULL,
      breaks=if (show_coords) waiver() else NULL, expand=expansion(mult=.002)) +
    coord_equal(expand=TRUE) +
    labs(title=if (iteration <= 7) band_labels[[band]] else NULL,
         x=if (show_coords) "Easting (km)" else NULL,
         y=if (show_coords) "Northing (km)" else NULL) + theme_pub +
    theme(panel.grid=element_blank(), axis.line=element_blank(),
          axis.ticks=if (show_coords) element_line(linewidth=.25) else element_blank(),
          legend.position="bottom", legend.key.width=unit(6,"mm"),
          legend.key.height=unit(2.4,"mm"), plot.margin=margin(1.2,1.2,1.2,1.2)) +
    guides(fill=guide_colourbar(title.position="top",title.hjust=.5,barwidth=unit(20,"mm")))
}

med_maps <- lapply(bands, make_map, metric="median")
sd_maps <- lapply(bands, make_map, metric="sd")
fig6 <- wrap_plots(c(med_maps,sd_maps),nrow=2,guides="collect") +
  plot_annotation(theme=theme(plot.margin=margin(1,2,1,7))) &
  theme(legend.position="bottom",legend.box="horizontal",legend.box.spacing=unit(.5,"mm"))

summary_path <- file.path(input_dir,"per_date_band_summary.csv")
ts <- read_csv(summary_path,show_col_types=FALSE) |>
  filter(band %in% bands) |>
  mutate(date=as.Date(date),band=factor(band,bands,labels=unname(band_labels[bands])))
stopifnot(n_distinct(ts$date)==22L,nrow(ts)==88L)
display_colours <- setNames(unname(band_colours[bands]),unname(band_labels[bands]))

fig7 <- ggplot(ts,aes(date,median_delta_rho,colour=band,fill=band)) +
  geom_hline(yintercept=0,linetype="22",linewidth=.38,colour="#707070") +
  geom_ribbon(aes(ymin=p25_delta_rho,ymax=p75_delta_rho),alpha=.22,colour=NA) +
  geom_line(linewidth=.72) + geom_point(size=1.45,stroke=.2) +
  facet_wrap(~band,ncol=2) +
  scale_colour_manual(values=display_colours,guide="none") +
  scale_fill_manual(values=display_colours,guide="none") +
  scale_x_date(date_breaks="3 months",date_labels="%b\n%Y",expand=expansion(mult=c(.02,.03))) +
  scale_y_continuous(labels=label_number(accuracy=.01),breaks=pretty_breaks(5),
                     expand=expansion(mult=c(.08,.10))) +
  labs(title="Temporal variation in atmospheric-correction differences",
       subtitle="Per-date median (line and point); spatial IQR across pixels (shading); 22 matched dates; no smoothing",
       x=NULL,y="Δρ = ρACOLITE − ρSen2Cor (unitless)") + theme_pub +
  theme(panel.grid.major.y=element_line(colour="#E4E4E4",linewidth=.3),
        panel.grid.major.x=element_blank(),axis.line=element_line(linewidth=.3),
        plot.title=element_text(size=base_size+1.4,face="bold"),
        plot.subtitle=element_text(size=base_size,colour="#4A4A4A"),
        panel.spacing=unit(4,"mm"))

save_pair <- function(plot,stem,width_mm,height_mm) {
  ggsave(file.path(out_dir,paste0(stem,".png")),plot,width=width_mm,height=height_mm,
         units="mm",dpi=600,device=ragg::agg_png,bg="white")
  ggsave(file.path(out_dir,paste0(stem,".svg")),plot,width=width_mm,height=height_mm,
         units="mm",device=svglite::svglite,bg="white")
  ggsave(file.path(out_dir,paste0(stem,".pdf")),plot,width=width_mm,height=height_mm,
         units="mm",device=cairo_pdf,bg="white")
}
save_pair(fig6,"DeltaRhoMedianSdMaps_Fig007",183,91)
save_pair(fig7,"DeltaRhoSeasonality_Fig008",183,130)

writeLines(c(
  "SCI-132 600-dpi figure-generation note",
  "Δρ = ρ_ACOLITE − ρ_Sen2Cor (unitless).",
  "Selected bands: B02 (492 nm), B03 (559 nm), B04 (665 nm), B08 (833 nm).",
  "Figure 7: per-pixel across-date median and sample SD; common scale within each metric; robust clipping is shown by ≤/≥ legend labels; SD scale is blue at 0, white at 0.01, and red at or above 0.02.",
  "Figure 8: per-date intertidal median and IQR; 22 exact platform/date/tile matches; no smoothing.",
  "Maps: EPSG:32628; 10 m analysis grid, aggregated only for display; supplied intertidal polygon.",
  "Interpretation: descriptive processor differences, not error against independent truth; valid-date count varies spatially (5–22 among pixels with data).",
  paste0("Input: ",substring(summary_path,nchar(project)+2)),
  paste0("Approved design reproduced: SCI-123 iteration ",iteration),
  "PNG export: 600 dpi; physical widths/heights are 183 × 91 mm (Figure 7) and 183 × 130 mm (Figure 8).",
  "Vector exports: SVG and PDF."
), file.path(out_dir,"CAPTION_INPUTS_AND_PROVENANCE.txt"))
message("Rendered SCI-132 approved iteration-",iteration," design to ",out_dir)
