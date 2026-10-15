#!/usr/bin/env Rscript

# SCI-37 / SCI-24 round 2: reproducible previews for Figures 2--4.
# Usage: Rscript make_figures_2_4.R [iteration: 1, 2, or 3]

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(readxl)
  library(stringr)
  library(tidyr)
})

args <- commandArgs(trailingOnly = TRUE)
iteration <- if (length(args)) as.integer(args[[1]]) else 3L
stopifnot(iteration %in% 1:3)

script_arg <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
project <- normalizePath(file.path(dirname(script_arg), ".."), mustWork = TRUE)
out_dir <- file.path(project, "figures")
p <- function(...) file.path(project, ...)

stations <- sprintf("AC%02d", 1:8)
surfaces <- c(AC01 = "Dry sand", AC02 = "Dry sand", AC03 = "MPB mud",
              AC04 = "MPB mud", AC05 = "Macroalgae", AC06 = "White sand",
              AC07 = "Seagrass", AC08 = "Seagrass")
station_colours <- c(AC01="#332288", AC02="#88CCEE", AC03="#44AA99", AC04="#117733",
                     AC05="#999933", AC06="#DDCC77", AC07="#CC6677", AC08="#AA4499")
surface_shapes <- c("Dry sand"=21, "MPB mud"=22, "Macroalgae"=23,
                    "White sand"=24, "Seagrass"=25)
bands <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8A")
band_wavelength <- c(B1=442.25, B2=492.34, B3=558.95, B4=664.56, B5=703.89,
                     B6=739.10, B7=779.73, B8=832.88, B8A=864.76)
band_name <- c(B1="Coastal aerosol", B2="Blue", B3="Green", B4="Red",
               B5="Vegetation red edge", B6="Vegetation red edge",
               B7="Vegetation red edge", B8="NIR", B8A="Narrow NIR")
band_display <- c(B1="Coastal", B2="Blue", B3="Green", B4="Red",
                  B5="Red edge", B6="Red edge", B7="Red edge",
                  B8="NIR", B8A="Narrow NIR")
band_labels <- paste0(bands, "  ", unname(band_display[bands]))
# Ordered dark-blue to dark-red ramp through B8; B8A is black as requested.
band_colours <- c(B1="#4c17e1", B2="#6999fe", B3="#a4d26f", B4="#e47121",
                  B5="#b80a0a", B6="#7e0000", B7="#111111", B8="#111111", B8A="#111111")

base_size <- c(`1`=8.3, `2`=8.6, `3`=8.8)[as.character(iteration)]
theme_pub <- theme_classic(base_size = base_size) +
  theme(axis.text = element_text(colour="#262626", size=base_size-1),
        axis.title = element_text(size=base_size),
        plot.title = element_text(size=base_size+.4, face="bold", hjust=0),
        strip.background = element_rect(fill="#F2F2F2", colour=NA),
        strip.text = element_text(face="bold", size=base_size),
        legend.title = element_text(face="bold", size=base_size-.2),
        legend.text = element_text(size=base_size-1),
        legend.key.height = unit(3.7, "mm"),
        legend.key.width = unit(6.2, "mm"),
        panel.spacing = unit(3.5, "mm"),
        plot.margin = margin(4, 5, 4, 5))

ASD_path <- p("Data","ASD","202603_Reflectance_avg.txt")
srf_path <- p("Data","COPE-GSEG-EOPG-TN-15-0007 - Sentinel-2 Spectral Response Functions 2024 - 4.0.xlsx")
# Figure 2: field spectra and SRF-convolved values. One shared compact station legend.
asd <- read.table(ASD_path, header=TRUE, check.names=FALSE) |>
  select(wl, matches("^AC_")) |>
  pivot_longer(-wl, names_to="raw", values_to="reflectance") |>
  mutate(station=str_replace(str_to_upper(raw), "AC_", "AC"))

srf <- read_excel(srf_path,
                  sheet="Spectral Responses (S2B)") |>
  rename(wl=SR_WL) |>
  pivot_longer(-wl, names_to="raw", values_to="srf") |>
  mutate(band=str_remove(raw,"^S2B_SR_AV_")) |>
  filter(band %in% bands, is.finite(srf), srf>0)
conv <- asd |>
  inner_join(select(srf,wl,band,srf), by="wl", relationship="many-to-many") |>
  summarise(reflectance=weighted.mean(reflectance,srf), .by=c(station,band)) |>
  mutate(band=factor(band,bands))
widths <- summarise(srf, xmin=min(wl), xmax=max(wl), .by=band)
station_labels <- setNames(unname(surfaces[stations]), stations)
station_lines <- c(AC03="dashed", setNames(rep("solid",7), stations[stations != "AC03"]))
surface_colours <- c("Dry sand"="#332288","Macroalgae"="#999933",
                     "White sand"="#DDCC77","Seagrass"="#AA4499")
asd <- asd |> mutate(surface=factor(unname(surfaces[station]),levels=names(surface_colours))) |> 
  dplyr::filter(surface != "MPB mud")

conv <- conv |> mutate(surface=factor(unname(surfaces[station]),levels=names(surface_colours)))|> 
  dplyr::filter(surface != "MPB mud")

p2a <- ggplot(
  filter(asd, between(wl, 400, 900)),
  aes(wl, reflectance, colour = surface, linetype = station)
) +
  geom_line(linewidth = .62) +
  scale_colour_manual(values = surface_colours, drop = FALSE) +
  scale_linetype_manual(
    values = station_lines,
    breaks = stations,
    labels = station_labels,
    guide = "none"
  ) +
  labs(
    title = "A  Field spectra",
    x = "Wavelength (nm)",
    y = "ASD reflectance",
    colour = NULL,
    linetype = NULL
  ) +
  guides(
    colour = guide_legend(
      ncol = 1,
      override.aes = list(linewidth = .8)
    )
  ) +
  theme_pub +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.02, 0.98),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.8),
      colour = NA
    ),
    legend.key = element_blank(),
    legend.spacing.y = unit(0, "pt"),
    legend.text = element_text(size = 8),
    plot.margin = margin(4, 8, 4, 5)
  )

p2b <- ggplot(
  conv,
  aes(band, reflectance, colour = surface,
      linetype = station, group = station)
) +
  geom_line(linewidth = .58) +
  geom_point(size = 1.45) +
  scale_colour_manual(values = surface_colours, drop = FALSE) +
  scale_linetype_manual(
    values = station_lines,
    breaks = stations,
    labels = station_labels,
    guide = "none"
  ) +
  labs(
    title = "B  MSI band-equivalent spectra",
    x = "Sentinel-2B MSI band",
    y = "Sentinel-2 simulated reflectance",
    colour = NULL,
    linetype = NULL
  ) +
  theme_pub +
  theme(
    legend.position = "none",
    plot.margin = margin(4, 5, 4, 8)
  )

# Combine panels without a separate legend row
fig2 <- p2a + p2b +
  patchwork::plot_layout(widths = c(1.12, 1))

fig2

# Figures 3--4: all eight stations and all nine bands from the canonical matchup table.
d_path <- p("figures","matchup_all_stations.csv")
d <- read_csv(d_path,show_col_types=FALSE) |>
  mutate(target=recode(target,"Microphytobenthos mud"="MPB mud"),
         band=factor(band,bands), correction=factor(correction,c("Sen2Cor","ACOLITE")),
         wavelength=unname(band_wavelength[as.character(band)])) |> 
  dplyr::filter(target != "MPB mud")

metric_labels <- d |>
  summarise(RMSD=sqrt(mean(residual^2)), Bias=mean(residual), .by=correction) |>
  mutate(label=sprintf("RMSD %.3f  |  Bias %+.3f",RMSD,Bias))

make_p3 <- function(correction_name, show_shape=FALSE, show_band=FALSE) ggplot(filter(d,correction==correction_name),aes(asd_reflectance,satellite_reflectance)) +
  geom_abline(slope=1,intercept=0,linetype="22",colour="#777777",linewidth=.45) +
  geom_point(aes(shape=target,fill=band),size=if(iteration==1) 2.15 else 2.35,
             stroke=.45,colour="#202020",alpha=1) +
  geom_label(
    data = filter(metric_labels, correction == correction_name),
    aes(x = .018, y = .685, label = label),
    inherit.aes = FALSE,
    hjust = 0, vjust = 1,
    size = 2.45,
    label.size = 0,
    fill = scales::alpha("white", .86)
  ) +
  labs(title=correction_name) +
  scale_shape_manual(values=surface_shapes,drop=FALSE) +
  scale_fill_manual(values=band_colours,breaks=bands,labels=band_labels,drop=FALSE) +
  scale_x_continuous(breaks=seq(0,.6,.1),expand=expansion(mult=0)) +
  scale_y_continuous(breaks=seq(0,.7,.1),expand=expansion(mult=0)) +
  coord_equal(xlim=c(0,.6),ylim=c(0,.7),clip="off") +
  labs(x="ASD field reflectance",y="Satellite surface reflectance",
       shape="Surface class",fill="Band wavelength") + theme_pub +
  theme(legend.position=c(.975,.025),legend.justification=c(1,0),
        legend.background=element_rect(fill=scales::alpha("white",.9),colour="#BBBBBB",linewidth=.25),
        legend.margin=margin(1,1,1,1),legend.spacing.y=unit(.2,"mm"),
        legend.title=element_text(size=6.0,face="bold"),legend.text=element_text(size=5.2),
        legend.key.height=unit(2.35,"mm"),legend.key.width=unit(3.0,"mm")) +
  guides(shape=if(show_shape) guide_legend(title="Surface class",ncol=1,override.aes=list(fill="white",size=1.5)) else "none",
         fill=if(show_band) guide_legend(title="Band",ncol=3,byrow=TRUE,override.aes=list(shape=21,size=1.5)) else "none")

fig3 <- make_p3("Sen2Cor",show_shape=TRUE) + make_p3("ACOLITE",show_band=TRUE) +
  plot_layout(guides="keep") & theme(plot.margin=margin(4,7,4,7))

fig4 <- ggplot(d,aes(band,residual,colour=band)) +
  geom_hline(yintercept=0,linetype="22",colour="#666666",linewidth=.45) +
  geom_point(position=position_jitter(width=.13,height=0,seed=24),size=1.35,alpha=.65) +
  geom_boxplot(width=.68,outlier.shape=NA,fill=NA,linewidth=.55) +
  facet_grid(correction~.) +
  scale_colour_manual(values=band_colours,breaks=bands,labels=band_labels,drop=FALSE) +
  scale_y_continuous(breaks=seq(-.15,.15,.05),labels=function(x)sprintf("%.2f",x),
                     expand=expansion(mult=c(.06,.08))) +
  labs(x="Sentinel-2B MSI band",y="Residual (satellite − ASD reflectance)",colour="Band wavelength") +
  theme_pub + theme(legend.position="bottom",legend.box.margin=margin(-2,0,0,0),
                    legend.key.width=unit(4,"mm"),legend.text=element_text(size=6.6),
                    legend.title=element_text(size=7,face="bold")) +
  guides(colour=guide_legend(nrow=2,byrow=TRUE,override.aes=list(linewidth=1.1,alpha=1)))

save_preview <- function(plot, final_stem, width_mm, height_mm) {
  iter_stem <- paste0(final_stem,"_iteration",iteration)
  ggsave(file.path(out_dir,paste0(iter_stem,".png")),plot,width=width_mm,height=height_mm,
         units="mm",dpi=300,device=ragg::agg_png,bg="white")
  if (iteration == 3L) {
    ggsave(file.path(out_dir,paste0(final_stem,".png")),plot,width=width_mm,height=height_mm,
           units="mm",dpi=600,device=ragg::agg_png,bg="white")
    ggsave(file.path(out_dir,paste0(final_stem,".svg")),plot,width=width_mm,height=height_mm,
           units="mm",device=svglite::svglite,bg="white")
  }
}

save_preview(fig2,"FieldSpectra_Fig002",183,if(iteration==1) 105 else 118)
save_preview(fig3,"MatchupByBandAndSurface_Fig003",183,if(iteration==1) 115 else if(iteration==2) 140 else 120)
save_preview(fig4,"ResidualBoxplotsByBand_Fig004",183,if(iteration==1) 118 else 132)
message("Rendered Figures 2--4, iteration ",iteration," to ",out_dir)
