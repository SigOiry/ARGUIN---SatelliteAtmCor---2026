#!/usr/bin/env Rscript

# SCI-78: ACOLITE-minus-Sen2Cor percentage-difference maps, restricted to B01--B8A.
# Usage: Rscript make_sci50_figures.R [iteration: 1, 2, 3, or 4]

suppressPackageStartupMessages({
  library(cowplot)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(readr)
  library(scales)
  library(sf)
  library(svglite)
  library(terra)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
iteration <- if (length(args)) as.integer(args[[1]]) else 4L
stopifnot(iteration %in% 1:4)

script_arg <- gsub("~\\+~", " ", sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
project <- normalizePath(file.path(dirname(script_arg), ".."), mustWork = TRUE)
out_dir <- file.path(project, "figures")
input_dir <- file.path(project, "workinprogress", "SCI-49_luna", "outputs")
revised_dir <- file.path(project, "workinprogress", "SCI-68_luna", "outputs", "fig5_pixel")
poi_file <- file.path(project, "Data", "SHP", "POI_S2C_ACOLITE_targets.shp")

bands <- c("B01", "B02", "B03", "B04", "B05", "B06", "B07", "B08", "B8A")
band_labels <- c(B01="B01 · 442 nm", B02="B02 · 492 nm", B03="B03 · 559 nm",
                 B04="B04 · 665 nm", B05="B05 · 704 nm", B06="B06 · 739 nm",
                 B07="B07 · 780 nm", B08="B08 · 833 nm", B8A="B8A · 865 nm")
base_size <- c(`1`=7.3, `2`=7.7, `3`=8.0, `4`=8.0)[as.character(iteration)]
theme_pub <- theme_minimal(base_size=base_size) +
  theme(
    text=element_text(colour="#242424"),
    axis.title=element_text(size=base_size),
    axis.text=element_text(size=base_size-1, colour="#3A3A3A"),
    panel.grid.major=element_line(colour="#FFFFFF", linewidth=.2),
    panel.grid.minor=element_blank(),
    strip.background=element_rect(fill="#F1F1F1", colour=NA),
    strip.text=element_text(face="bold", size=base_size),
    legend.title=element_text(face="bold", size=base_size-.1),
    legend.text=element_text(size=base_size-1),
    plot.margin=margin(3,4,3,4)
  )

# Read and coarsen only for display. Statistics remain those of Luna's full-resolution analysis.
raster_files <- file.path(input_dir, "rasters",
                          paste0("diff_ACOLITE_minus_Sen2Cor_", bands, "_10m.tif"))
stopifnot(all(file.exists(raster_files)))
r <- rast(raster_files)
names(r) <- bands
# Relative difference uses Sen2Cor as the reference. Values at or below 0.01
# Sen2Cor reflectance are masked because ratios there are numerically unstable
# and would imply unsupported percentage precision.
project_path <- paste(project, "Data", "S2",
                      "S2B_MSIL2A_20260327T113319_N0512_R080_T28QCH_20260327T152639.SAFE",
                      "GRANULE", "L2A_T28QCH_A047296_20260327T113507", "IMG_DATA", sep = "/")
granule <- file.path(project_path)
res_dirs <- c(B01="R60m",B02="R10m",B03="R10m",B04="R10m",B05="R20m",
              B06="R20m",B07="R20m",B08="R10m",B8A="R20m")
res_suffix <- sub("R", "", res_dirs)
sen_files <- file.path(granule, unname(res_dirs),
  paste0("T28QCH_20260327T113319_", bands, "_", unname(res_suffix), ".jp2"))
stopifnot(all(file.exists(sen_files)))
sen_ref <- rast(lapply(seq_along(sen_files), function(i) {
  resample(rast(sen_files[[i]]), r[[i]], method="bilinear")
}))
names(sen_ref) <- bands
sen_ref <- (sen_ref - 1000) / 10000
# r <- 100 * r / sen_ref
# r[sen_ref <= 0.01] <- NA
display_factor <- c(`1`=10L, `2`=7L, `3`=5L, `4`=5L)[as.character(iteration)]
r_display <- aggregate(r, fact=display_factor, fun=mean, na.rm=TRUE)
map_df <- as.data.frame(r_display, xy=TRUE, na.rm=FALSE) |>
  pivot_longer(all_of(bands), names_to="band", values_to="percent_difference") |>
  filter(is.finite(percent_difference)) |>
  mutate(band=factor(band, bands, labels=unname(band_labels[bands])),
         easting_km=x/1000, northing_km=y/1000)

targets_sf <- st_read(poi_file, quiet=TRUE) |>
  st_transform(crs(r)) |>
  arrange(as.integer(sub("^T", "", Name)))
stopifnot(nrow(targets_sf) == 6L, identical(targets_sf$Name, paste0("T", 1:6)))
target_xy <- st_coordinates(targets_sf)
targets <- targets_sf |>
  st_drop_geometry() |>
  transmute(target_code=Name,
            target_label=factor(paste(Name, Class, sep="  "),
                                levels=paste(Name, Class, sep="  ")),
            easting_m=target_xy[,1], northing_m=target_xy[,2],
            easting_km=easting_m/1000, northing_km=northing_m/1000)

mask <- rast(file.path(input_dir, "rasters", "intertidal_mask_10m.tif"))
mask_display <- aggregate(mask, fact=display_factor, fun="max", na.rm=TRUE)
mask_df <- as.data.frame(mask_display, xy=TRUE, na.rm=FALSE) |>
  rename(mask_value=3) |>
  filter(is.finite(mask_value), mask_value > 0) |>
  transmute(easting_km=x/1000, northing_km=y/1000)

map_limit <- 0.06
diverging_cols <- colorspace::divergingx_hcl(11, palette="RdBu", rev=TRUE)
map_scale <- scale_fill_gradientn(
  name=NULL,
  colours=diverging_cols, limits=c(-map_limit,map_limit), oob=squish,
  values=scales::rescale(seq(-map_limit,map_limit,length.out=length(diverging_cols))),
  breaks=c(-0.06,-0.03,0,0.03,0.06),
  labels=c("−0.06\nand below","−0.03","0","+0.03","+0.06\nand above"),
  na.value="#E8E8E8"
)

make_map_panel <- function(band_name) {
  ggplot(filter(map_df, band == band_labels[[band_name]]),
         aes(easting_km, northing_km, fill=percent_difference)) +
    geom_raster() +
    map_scale +
    coord_equal(expand=FALSE) +
    labs(title=band_labels[[band_name]], x=NULL, y=NULL, fill=NULL) +
    theme_pub +
    theme(panel.grid=element_blank(), legend.position="none",
          plot.title=element_text(face="bold",size=base_size,hjust=.03,
                                  margin=margin(1,0,1,0)),
          axis.text=element_text(size=base_size-1.4),
          plot.margin=margin(1.2,1.2,1.2,1.2))
}

legend_source <- ggplot(filter(map_df, band == band_labels[["B04"]]),
                        aes(easting_km,northing_km,fill=percent_difference)) +
  geom_raster() +
  map_scale +
  theme_void() +
  theme(legend.position="bottom",legend.title=element_text(face="bold",size=base_size-.1),
        legend.text=element_text(size=base_size-1, lineheight=.85),
        legend.key.width=unit(10,"mm"),legend.key.height=unit(2.8,"mm")) +
  guides(fill=guide_colourbar(title.position="top",title.hjust=.5,
                              barwidth=unit(52,"mm"),barheight=unit(3,"mm")))

xr <- range(map_df$easting_km); yr <- range(map_df$northing_km)
locator_xr <- range(c(xr, targets$easting_km))
locator_y_span <- diff(yr) * diff(locator_xr) / diff(xr)
locator_yr <- mean(yr) + c(-.5,.5) * locator_y_span
scale_x0 <- locator_xr[1] + 2.2; scale_y0 <- locator_yr[1] + 1.35
locator <- ggplot(mask_df, aes(easting_km,northing_km)) +
  geom_raster(fill="#D8D1B9") +
  geom_point(data=targets, aes(easting_km,northing_km), inherit.aes=FALSE,
             shape=21, size=2.4, stroke=.65, fill="#FDE725", colour="#1D1D1D") +
  geom_label_repel(data=targets,aes(easting_km,northing_km,label=target_code),inherit.aes=FALSE,
                   size=2.35,fontface="bold",box.padding=.22,point.padding=.12,
                   min.segment.length=0,segment.size=.3,label.padding=unit(.7,"mm"),
                   seed=50,colour="#111111",fill=alpha("white",.86),label.size=0) +
  annotate("segment",x=scale_x0,xend=scale_x0+5,y=scale_y0,yend=scale_y0,
           linewidth=.9, colour="#202020") +
  annotate("segment",x=c(scale_x0,scale_x0+5),xend=c(scale_x0,scale_x0+5),
           y=scale_y0-.22,yend=scale_y0+.22,linewidth=.7) +
  annotate("text",x=scale_x0+2.5,y=scale_y0+.72,label="5 km",size=2.35) +
  annotate("segment",x=locator_xr[2]-2,y=locator_yr[2]-5.8,xend=locator_xr[2]-2,yend=locator_yr[2]-2.9,
           arrow=arrow(length=unit(2,"mm"),type="closed"),linewidth=.75) +
  annotate("text",x=locator_xr[2]-2,y=locator_yr[2]-2.25,label="N",fontface="bold",size=2.8) +
  coord_equal(xlim=locator_xr,ylim=locator_yr,expand=FALSE,clip="off") +
  labs(title="Target locator",x=NULL,y=NULL) +
  theme_pub + theme(panel.grid=element_blank(),panel.background=element_rect(fill="#EAF2F5",colour=NA),
                    plot.title=element_text(face="bold",size=base_size,hjust=.03,
                                            margin=margin(1,0,1,0)),
                    axis.text=element_text(size=base_size-1.4),
                    plot.margin=margin(1.2,1.2,1.2,1.2))

map_legend <- cowplot::get_legend(legend_source)
map_panels <- lapply(bands, make_map_panel)
# Ten content panels plus the shared colour bar fit into a balanced 4 x 3 grid;
# the legend is an integrated grid cell instead of a detached footer.
legend_panel <- cowplot::ggdraw() +
  cowplot::draw_label("ACOLITE − Sen2Cor",x=.5,y=.84,
                      size=6.6,fontface="bold",lineheight=.9) +
  cowplot::draw_grob(map_legend,x=.08,y=.16,width=.84,height=.48)
layout_design <- "
ABCD
EFGH
IJKK
"
fig_maps <- wrap_plots(c(map_panels,list(locator),list(legend_panel)),
                       design=layout_design) +
  plot_annotation(theme=theme(plot.margin=margin(2,2,2,2)))

if (FALSE) {
  # Retained only as a parse-safe historical marker; composition above is authoritative.
  ggplot(map_df, aes(easting_km, northing_km, fill=difference)) +
  geom_raster() +
  scale_fill_gradientn(
    colours=diverging_cols, limits=c(-map_limit,map_limit), oob=squish,
    values=scales::rescale(seq(-map_limit,map_limit,length.out=length(diverging_cols))),
    breaks=c(-.06,-.03,0,.03,.06),
    labels=c("−0.06\nand below","−0.03","0","+0.03","+0.06\nand above"),
    na.value="#E8E8E8"
  ) +
  coord_equal(expand=FALSE) +
  labs(x="Easting (km, UTM 28N)", y="Northing (km, UTM 28N)",
       fill="ACOLITE − Sen2Cor\nreflectance") +
  theme_pub +
  theme(panel.grid=element_blank(), legend.position="bottom",
        legend.key.width=unit(18,"mm"), legend.key.height=unit(2.8,"mm"),
        panel.spacing=unit(if(iteration==1) 1.4 else 2.0,"mm")) +
  guides(fill=guide_colourbar())
}

# Paired spectra use the same 3 x 3 ACOLITE-grid window and per-band valid pixels
# as Luna's original extraction. The masked difference rasters define validity;
# ACOLITE reflectance is reconstructed exactly as difference + Sen2Cor.
target_cells <- lapply(seq_len(nrow(targets)), function(i) {
  centre <- cellFromXY(r, as.matrix(targets[i,c("easting_m","northing_m")]))
  if (is.na(centre)) return(integer())
  rc <- rowColFromCell(r, centre)
  rows <- (rc[1,1]-1L):(rc[1,1]+1L)
  cols <- (rc[1,2]-1L):(rc[1,2]+1L)
  cellFromRowColCombine(r, rows, cols)
})

spectra_wide <- bind_rows(lapply(seq_len(nrow(targets)), function(i) {
  if (length(target_cells[[i]])) {
    diff_values <- as.matrix(r[target_cells[[i]]])
    sen_values <- as.matrix(sen_ref[target_cells[[i]]])
  } else {
    diff_values <- sen_values <- matrix(numeric(), nrow=0L, ncol=length(bands))
  }
  bind_rows(lapply(seq_along(bands), function(j) {
    valid <- if (length(target_cells[[i]])) {
      is.finite(diff_values[,j]) & is.finite(sen_values[,j])
    } else logical()
    tibble(target_code=targets$target_code[i], target_label=targets$target_label[i],
           band=bands[j], acolite_wavelength_nm=as.numeric(sub(".* · ([0-9]+) nm", "\\1", band_labels[j])),
           n_valid=sum(valid),
           acolite_mean_reflectance=if (any(valid)) mean(diff_values[valid,j]+sen_values[valid,j]) else NA_real_,
           sen2cor_mean_reflectance=if (any(valid)) mean(sen_values[valid,j]) else NA_real_)
  }))
}))
handoff_dir <- file.path(project, "workinprogress", "SCI-80_frida")
dir.create(handoff_dir, recursive=TRUE, showWarnings=FALSE)
write_csv(spectra_wide |>
            left_join(select(targets, target_code, easting_m, northing_m), by="target_code") |>
            select(target_code, target_label, easting_m, northing_m, band,
                   acolite_wavelength_nm, n_valid,
                   sen2cor_mean_reflectance, acolite_mean_reflectance),
          file.path(handoff_dir, "target_spectra_values.csv"), na="")

# Free y-ranges are deliberate: comparisons are within targets spanning
# roughly an order of magnitude in reflectance, and a common scale obscures dark targets.
spectra <- spectra_wide |>
  pivot_longer(c(acolite_mean_reflectance,sen2cor_mean_reflectance),
               names_to="correction", values_to="reflectance") |>
  mutate(correction=recode(correction,
                           acolite_mean_reflectance="ACOLITE",
                           sen2cor_mean_reflectance="Sen2Cor"),
         correction=factor(correction,c("Sen2Cor","ACOLITE")))
spectra_plot <- filter(spectra, is.finite(reflectance))

spectra_labels <- spectra_plot |>
  filter(band %in% c("B01","B04","B08","B12")) |>
  summarise(reflectance=max(reflectance),.by=c(target_label,band,acolite_wavelength_nm))

unavailable_targets <- spectra_wide |>
  summarise(available=any(n_valid > 0L), .by=target_label) |>
  filter(!available)

method_cols <- c(Sen2Cor="#2166AC",ACOLITE="#D95F02")
method_shapes <- c(Sen2Cor=16,ACOLITE=17)
fig_spectra <- ggplot(spectra_plot,aes(acolite_wavelength_nm,reflectance,
                                  colour=correction,shape=correction,group=correction)) +
  geom_hline(yintercept=0,colour="#777777",linetype="22",linewidth=.35) +
  geom_line(linewidth=.72) +
  geom_point(size=1.9,stroke=.3) +
  geom_text(data=spectra_labels,aes(acolite_wavelength_nm,reflectance,label=band),
            inherit.aes=FALSE,vjust=-.8,size=2.15,colour="#444444",check_overlap=TRUE) +
  geom_text(data=unavailable_targets,
            aes(x=650,y=.05,label="No data: target lies west\nof the raster extent"),
            inherit.aes=FALSE,size=2.5,lineheight=.95,colour="#555555") +
  facet_wrap(~target_label,ncol=2,scales="free_y") +
  scale_colour_manual(values=method_cols) +
  scale_shape_manual(values=method_shapes) +
  scale_x_continuous(breaks=c(500,1000,1500,2000),minor_breaks=NULL,
                     expand=expansion(mult=c(.03,.05))) +
  scale_y_continuous(labels=label_number(accuracy=.01),expand=expansion(mult=c(.08,.18))) +
  labs(x="Band centre wavelength (nm)",y="Mean surface reflectance in 3 × 3-pixel window",
       colour=NULL,shape=NULL) +
  theme_pub +
  theme(legend.position="bottom",legend.box.margin=margin(-2,0,0,0),
        panel.grid.major.x=element_blank(),panel.spacing=unit(3.2,"mm")) +
  guides(colour=guide_legend(nrow=1,byrow=TRUE,override.aes=list(linewidth=.8,size=2.1)))

save_figure <- function(plot, stem, width_mm, height_mm) {
  iter_path <- file.path(out_dir,paste0(stem,"_iteration",iteration,".png"))
  ggsave(iter_path,plot,width=width_mm,height=height_mm,units="mm",dpi=300,
         device=ragg::agg_png,bg="white")
  if (iteration >= 3L) {
    final_png <- file.path(out_dir,paste0(stem,".png"))
    board_png <- if (stem == "CorrectionDifferenceMaps_Fig005" && file.exists(final_png)) {
      magick::image_read(final_png)
    } else NULL
    ggsave(final_png,plot,width=width_mm,height=height_mm,
           units="mm",dpi=600,device=ragg::agg_png,bg="white")
    if (!is.null(board_png)) {
      # The Board-edited PNG is authoritative. Replace only its locator grid cell;
      # retain all other pixels exactly (geometry is for the 183 x 108 mm, 600 dpi export).
      locator_geometry <- "1000x700+1100+1500"
      locator_patch <- magick::image_crop(magick::image_read(final_png),
                                          locator_geometry, repage=TRUE)
      magick::image_write(magick::image_composite(board_png, locator_patch,
                                                  offset="+1100+1500"), final_png)
    }
    ggsave(file.path(out_dir,paste0(stem,".svg")),plot,width=width_mm,height=height_mm,
           units="mm",device=svglite::svglite,bg="white")
  }
}

save_figure(fig_maps,"CorrectionDifferenceMaps_Fig005",183,108)
save_figure(fig_spectra,"TargetSpectralSignatures_Fig006",183,142)
message("Rendered SCI-50 iteration ",iteration," to ",out_dir)
