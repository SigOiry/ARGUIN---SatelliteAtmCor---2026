#!/usr/bin/env Rscript

# SCI-37 / SCI-24 round 2: Figure 1 study-area map.
# Usage: Rscript make_figure_1.R [iteration: 1, 2, or 3]

suppressPackageStartupMessages({
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(sf)
})

args <- commandArgs(trailingOnly = TRUE)
iteration <- if (length(args)) as.integer(args[[1]]) else 3L
stopifnot(iteration %in% 1:3)

script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
project <- normalizePath(file.path(dirname(script_arg), ".."), mustWork = TRUE)
out_dir <- file.path(project, "figures")
luna_dir <- file.path(project, "workinprogress", "SCI-24r2_luna")
gpkg <- file.path(luna_dir, "frida_figure1_geodata.gpkg")
tile_dir <- file.path(project, "workinprogress", "SCI-24r2_frida", "esri_tile_cache")
dir.create(tile_dir, showWarnings = FALSE)

intertidal <- st_read(gpkg, layer = "intertidal_mask", quiet = TRUE) |> st_transform(3857)
extent_poly <- st_read(gpkg, layer = "banc_arguin_extent", quiet = TRUE) |> st_transform(3857)
stations <- st_read(gpkg, layer = "validation_stations", quiet = TRUE) |> st_transform(3857)
locator_land <- st_read(gpkg, layer = "west_africa_land_110m", quiet = TRUE)
stations_ll <- st_transform(stations, 4326)
station_xy <- cbind(st_drop_geometry(stations), as.data.frame(st_coordinates(stations)))
station_xy$surface_short <- sub("Microphytobenthos mud", "MPB mud", station_xy$surface_class, fixed=TRUE)
station_groups <- data.frame(
  label=c("AC01–02","AC05–08"),
  X=c(mean(station_xy$X[station_xy$station_id %in% c("AC01","AC02")]),
      mean(station_xy$X[station_xy$station_id %in% c("AC05","AC06","AC07","AC08")])),
  Y=c(mean(station_xy$Y[station_xy$station_id %in% c("AC01","AC02")]),
      mean(station_xy$Y[station_xy$station_id %in% c("AC05","AC06","AC07","AC08")]))
)

station_colours <- c(AC01="#332288", AC02="#88CCEE", AC03="#44AA99", AC04="#117733",
                     AC05="#999933", AC06="#DDCC77", AC07="#CC6677", AC08="#AA4499")

lonlat_to_tile <- function(lon, lat, zoom) {
  n <- 2^zoom
  c(x = floor((lon + 180) / 360 * n),
    y = floor((1 - log(tan(lat*pi/180) + 1/cos(lat*pi/180))/pi) / 2 * n))
}

tile_bounds_3857 <- function(x, y, zoom) {
  world <- 20037508.342789244
  size <- 2 * world / 2^zoom
  c(xmin = -world + x*size, xmax = -world + (x+1)*size,
    ymin = world - (y+1)*size, ymax = world - y*size)
}

get_tile <- function(x, y, zoom) {
  path <- file.path(tile_dir, sprintf("esri_z%02d_x%d_y%d.png", zoom, x, y))
  if (!file.exists(path) || file.info(path)$size == 0) {
    url <- sprintf("https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/%d/%d/%d", zoom, y, x)
    download.file(url, path, mode = "wb", quiet = TRUE)
  }
  signature <- readBin(path, "raw", n = 2)
  image <- if (identical(signature, as.raw(c(0xff, 0xd8)))) jpeg::readJPEG(path) else png::readPNG(path)
  list(image = image, bounds = tile_bounds_3857(x, y, zoom))
}

tile_layer <- function(plot, x, y, zoom) {
  tile <- get_tile(x, y, zoom)
  b <- tile$bounds
  plot + annotation_raster(tile$image, b[["xmin"]], b[["xmax"]], b[["ymin"]], b[["ymax"]])
}

tile_plot_for_bbox <- function(bbox, zoom) {
  ll <- st_transform(st_as_sfc(bbox), 4326) |> st_bbox()
  nw <- lonlat_to_tile(ll[["xmin"]], ll[["ymax"]], zoom)
  se <- lonlat_to_tile(ll[["xmax"]], ll[["ymin"]], zoom)
  p <- ggplot()
  for (x in seq(nw[["x"]], se[["x"]])) for (y in seq(nw[["y"]], se[["y"]])) {
    p <- tile_layer(p, x, y, zoom)
  }
  p
}

main_bbox <- st_bbox(extent_poly)
main_pad <- c(x = 3000, y = 3000)
main_bbox[c("xmin","xmax")] <- main_bbox[c("xmin","xmax")] + c(-1,1)*main_pad[["x"]]
main_bbox[c("ymin","ymax")] <- main_bbox[c("ymin","ymax")] + c(-1,1)*main_pad[["y"]]
scale_10km_merc <- 10000 / cos(mean(st_coordinates(stations_ll)[,"Y"]) * pi/180)
scale_xend <- main_bbox[["xmax"]] - 1800
scale_xstart <- scale_xend - scale_10km_merc

main <- tile_plot_for_bbox(main_bbox, 11) +
  geom_sf(data = intertidal, fill = NA, colour = NA) +
  geom_sf(data = stations, aes(fill = station_id), shape = 21, size = 2.8,
          stroke = .7, colour = "white") +
  geom_label_repel(data = station_groups, aes(X, Y, label = label),
                   min.segment.length = 0, seed = 24, box.padding = .28,
                   point.padding = .35, size = 2.1, fontface = "bold",
                   fill = scales::alpha("white", .86), label.size = 0,
                   colour = "#202020", segment.colour = "white") +
  scale_fill_manual(values = station_colours, guide = "none") +
  coord_sf(xlim = main_bbox[c("xmin","xmax")], ylim = main_bbox[c("ymin","ymax")],
           expand = FALSE, datum = NA) +
  annotate("label", x=main_bbox[["xmax"]]-900, y=main_bbox[["ymax"]]-900,
           label="B", hjust=1, vjust=1, colour="black", fontface="bold", size=3.0,
           fill=scales::alpha("white",.86), label.size=0, label.padding=unit(1.2,"mm")) +
  annotate("segment", x=scale_xstart, xend=scale_xend,
           y=main_bbox[["ymin"]]+1700, yend=main_bbox[["ymin"]]+1700,
           colour="black", linewidth=1.1) +
  annotate("text", x=(scale_xstart+scale_xend)/2, y=main_bbox[["ymin"]]+2400,
           label="10 km", colour="black", size=2.25, fontface="bold") +
  theme_void(base_size = 8.5) +
  theme(plot.margin = margin(0,0,0,0), panel.border = element_rect(colour="#333333", fill=NA, linewidth=.45))

loc_pt <- st_union(stations_ll) |> st_centroid()
locator <- ggplot() +
  geom_sf(data = locator_land, fill = "#E4E0D5", colour = "white", linewidth = .2) +
  geom_sf(data = loc_pt, shape = 21, fill = "#D43D32", colour = "white", size = 2.6, stroke = .7) +
  annotate("text", x = -14.7, y = 18.9, label = "Banc d'Arguin", hjust = 0, size = 2.25, fontface="bold") +
  coord_sf(xlim = c(-26,18), ylim = c(3,33), expand = FALSE) +
  annotate("label", x=-24.5, y=31.5, label="A", hjust=0, vjust=1, colour="black",
           fill=scales::alpha("white",.86), label.size=0, size=2.7, fontface="bold",
           label.padding=unit(1.1,"mm")) +
  theme_void(base_size = 8) +
  theme(panel.background = element_rect(fill="#DDEBF3",colour=NA),
        panel.border = element_rect(fill=NA,colour="#333333",linewidth=.4),
        plot.margin=margin(3,3,3,3))

make_zoom <- function(ids, panel_letter) {
  s <- stations[stations$station_id %in% ids,]
  s_xy <- cbind(st_drop_geometry(s),as.data.frame(st_coordinates(s)))
  bb <- st_bbox(s)
  span <- max(bb[["xmax"]]-bb[["xmin"]], bb[["ymax"]]-bb[["ymin"]])
  half <- max(900, span * .66)
  centre <- c(mean(bb[c("xmin","xmax")]), mean(bb[c("ymin","ymax")]))
  bb <- st_bbox(c(xmin=centre[[1]]-half, ymin=centre[[2]]-half,
                  xmax=centre[[1]]+half, ymax=centre[[2]]+half), crs=st_crs(3857))
  scale_length <- if (length(ids) <= 2) 500 else 1000
  scale_x0 <- bb[["xmin"]] + .08 * (bb[["xmax"]] - bb[["xmin"]])
  scale_y0 <- bb[["ymin"]] + .08 * (bb[["ymax"]] - bb[["ymin"]])
  tile_plot_for_bbox(bb, 16) +
    geom_sf(data=intertidal,fill=scales::alpha("#2DC7C9",.18),colour="#62D4D2",linewidth=.18) +
    geom_sf(data=s,aes(fill=station_id),shape=21,colour="white",size=3.1,stroke=.75) +
    geom_text_repel(data=s_xy,aes(X,Y,label=station_id),colour="white",fontface="bold",size=2.05,
                    seed=24,box.padding=.2,point.padding=.3,min.segment.length=0,
                    segment.colour="white",segment.size=.25,max.overlaps=Inf) +
    scale_fill_manual(values=station_colours,guide="none") +
    coord_sf(xlim=bb[c("xmin","xmax")],ylim=bb[c("ymin","ymax")],expand=FALSE,datum=NA) +
    annotate("label", x=bb[["xmin"]]+120, y=bb[["ymax"]]-120, label=panel_letter,
             hjust=0, vjust=1, colour="black", fontface="bold", size=2.7,
             fill=scales::alpha("white",.86), label.size=0,
             label.padding=unit(1.1,"mm")) +
    annotate("segment", x=scale_x0, xend=scale_x0+scale_length,
             y=scale_y0, yend=scale_y0, colour="white", linewidth=1.5) +
    annotate("segment", x=scale_x0, xend=scale_x0+scale_length,
             y=scale_y0, yend=scale_y0, colour="black", linewidth=.65) +
    annotate("label", x=scale_x0+scale_length/2,
             y=scale_y0+.06*(bb[["ymax"]]-bb[["ymin"]]),
             label=paste0(scale_length/1000," km"), colour="black", fontface="bold",
             fill=scales::alpha("white",.78), label.size=0, label.padding=unit(.45,"mm"),
             size=2.1) +
    theme_void(base_size=7.2) +
    theme(panel.border=element_rect(colour="#333333",fill=NA,linewidth=.4),
          plot.margin=margin(0,0,0,0))
}

zooms <- list(make_zoom(c("AC01","AC02"), "C"),
              make_zoom(c("AC03","AC04","AC05","AC06","AC07","AC08"), "D"))
zoom_grid <- wrap_plots(zooms, ncol=1) +
  plot_annotation(theme=theme(plot.margin=margin(0,0,0,0)))

left <- main + inset_element(locator, left=0, bottom=-0.1, right=.43, top=.36, align_to="panel")
fig1 <- (left | zoom_grid) + plot_layout(widths=c(1.34,1))

stem <- paste0("StudyArea_Fig001_iteration", iteration)
ggsave(file.path(out_dir,paste0(stem,".png")),fig1,width=183,height=132,units="mm",dpi=300,
       device=ragg::agg_png,bg="white")



if (iteration == 3L) {
  ggsave(file.path(out_dir,"StudyArea_Fig001.png"),fig1,width=183,height=132,units="mm",dpi=600,
         device=ragg::agg_png,bg="white")
  ggsave(file.path(out_dir,"StudyArea_Fig001.svg"),fig1,width=183,height=132,units="mm",
         device=svglite::svglite,bg="white")
}
message("Rendered Figure 1, iteration ",iteration," to ",out_dir)
