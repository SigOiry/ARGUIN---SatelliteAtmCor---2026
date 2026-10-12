"""Create the publication study-area map from the 27 March 2026 Sentinel-2 image."""

from __future__ import annotations

import io
import tempfile
import urllib.request
import zipfile
from pathlib import Path

import geopandas as gpd
import matplotlib.patheffects as path_effects
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import rasterio
from matplotlib.lines import Line2D
from matplotlib.patches import Patch
from rasterio.enums import Resampling
from rasterio.windows import from_bounds


ROOT = Path(__file__).resolve().parents[1]
FIGURE_DIR = ROOT / "figures"
FIGURE_DIR.mkdir(parents=True, exist_ok=True)

TCI = next(
    (ROOT / "Data" / "S2").glob(
        "S2B_MSIL2A_20260327T113319_N0512_R080_T28QCH_20260327T152639.SAFE/"
        "GRANULE/*/IMG_DATA/R10m/*_TCI_10m.jp2"
    )
)
INTERTIDAL = ROOT / "Data" / "SHP" / "mask" / "Intertidal_merged.shp"
EXTENT = ROOT / "Data" / "SHP" / "extent.shp"
GPS = ROOT / "Data" / "GPS" / "Info spectral measure_biomass core_BdA 2026.xlsx"

STATION_COLOURS = {
    "AC01": "#000000",
    "AC02": "#E69F00",
    "AC03": "#56B4E9",
    "AC04": "#009E73",
    "AC05": "#F0E442",
    "AC06": "#0072B2",
    "AC07": "#D55E00",
    "AC08": "#CC79A7",
}

TARGET_LABELS = {
    "dry_sand": "dry sand",
    "mbp_mud": "microphytobenthos mud",
    "mpb_mud": "microphytobenthos mud",
    "algae_mud": "algal mud",
    "white_sand": "white sand",
    "seagrass": "seagrass",
}


def read_natural_earth() -> gpd.GeoDataFrame:
    """Read Natural Earth country polygons without adding a project dependency."""
    url = "https://naturalearth.s3.amazonaws.com/110m_cultural/ne_110m_admin_0_countries.zip"
    with urllib.request.urlopen(url, timeout=60) as response:
        payload = response.read()
    with tempfile.TemporaryDirectory() as temp_dir:
        with zipfile.ZipFile(io.BytesIO(payload)) as archive:
            archive.extractall(temp_dir)
        shp = next(Path(temp_dir).glob("*.shp"))
        return gpd.read_file(shp)


def read_rgb(bounds: tuple[float, float, float, float], max_pixels: int = 1600):
    """Read a map window at display resolution and return image and extent."""
    with rasterio.open(TCI) as src:
        left, bottom, right, top = bounds
        window = from_bounds(left, bottom, right, top, transform=src.transform)
        window = window.round_offsets().round_lengths()
        scale = min(1.0, max_pixels / max(window.width, window.height))
        out_width = max(1, int(window.width * scale))
        out_height = max(1, int(window.height * scale))
        rgb = src.read(
            [1, 2, 3],
            window=window,
            out_shape=(3, out_height, out_width),
            resampling=Resampling.bilinear,
            boundless=True,
            fill_value=0,
        )
        transform = src.window_transform(window)
        x0 = transform.c
        y1 = transform.f
        x1 = x0 + transform.a * window.width
        y0 = y1 + transform.e * window.height
    rgb = np.moveaxis(rgb, 0, -1)
    return rgb, (x0, x1, y0, y1)


def draw_scale_bar(ax, length_km: float, location=(0.07, 0.075)) -> None:
    x0, x1 = ax.get_xlim()
    y0, y1 = ax.get_ylim()
    length = length_km * 1000
    start_x = x0 + location[0] * (x1 - x0)
    start_y = y0 + location[1] * (y1 - y0)
    ax.plot(
        [start_x, start_x + length],
        [start_y, start_y],
        color="white",
        linewidth=5.0,
        solid_capstyle="butt",
        zorder=20,
    )
    ax.plot(
        [start_x, start_x + length],
        [start_y, start_y],
        color="#202020",
        linewidth=2.2,
        solid_capstyle="butt",
        zorder=21,
    )
    ax.text(
        start_x + length / 2,
        start_y + 0.018 * (y1 - y0),
        f"{length_km:g} km",
        ha="center",
        va="bottom",
        fontsize=7.5,
        color="#111111",
        zorder=22,
        path_effects=[path_effects.withStroke(linewidth=2.7, foreground="white")],
    )


def draw_north_arrow(ax, location=(0.93, 0.11)) -> None:
    ax.annotate(
        "N",
        xy=location,
        xytext=(location[0], location[1] - 0.075),
        xycoords="axes fraction",
        textcoords="axes fraction",
        ha="center",
        va="center",
        fontsize=8.5,
        fontweight="bold",
        color="#111111",
        arrowprops=dict(arrowstyle="-|>", lw=1.3, color="#111111"),
        path_effects=[path_effects.withStroke(linewidth=2.8, foreground="white")],
        zorder=30,
    )


def outline_intertidal(ax, intertidal: gpd.GeoDataFrame) -> None:
    intertidal.plot(ax=ax, facecolor="#00A6A6", alpha=0.18, edgecolor="none", zorder=5)
    intertidal.boundary.plot(ax=ax, color="white", linewidth=1.25, alpha=0.92, zorder=6)
    intertidal.boundary.plot(ax=ax, color="#007F7F", linewidth=0.55, alpha=1.0, zorder=7)


def clean_projected_axis(ax) -> None:
    ax.set_xticks([])
    ax.set_yticks([])
    for spine in ax.spines.values():
        spine.set_linewidth(0.65)
        spine.set_edgecolor("#4A4A4A")
    ax.set_aspect("equal")


def main() -> None:
    plt.rcParams.update(
        {
            "font.family": "DejaVu Sans",
            "font.size": 8.5,
            "axes.titlesize": 10,
            "axes.titleweight": "bold",
            "axes.titlelocation": "left",
            "savefig.facecolor": "white",
        }
    )

    intertidal = gpd.read_file(INTERTIDAL)
    intertidal["geometry"] = intertidal.geometry.make_valid()
    extent = gpd.read_file(EXTENT).to_crs(intertidal.crs)

    gps = pd.read_excel(GPS)
    gps["date"] = pd.to_datetime(gps["date"], errors="coerce")
    gps = gps.loc[
        gps["lat"].notna()
        & gps["lon"].notna()
        & gps["date"].between("2026-03-20", "2026-03-30")
    ].copy()
    points = gpd.GeoDataFrame(
        gps,
        geometry=gpd.points_from_xy(gps["lon"], gps["lat"]),
        crs="EPSG:4326",
    ).to_crs(intertidal.crs)
    ac = points.loc[points["station"].astype(str).str.match(r"^AC0[1-8]$")].copy()
    ac["target_label"] = ac["comment"].astype(str).str.lower().map(TARGET_LABELS).fillna(ac["comment"])

    minx, miny, maxx, maxy = extent.total_bounds
    full_bounds = (minx - 750, miny - 750, maxx + 750, maxy + 750)
    ac_west = ac.loc[ac["station"].isin(["AC03", "AC04", "AC05", "AC06", "AC07", "AC08"])].copy()
    ac_east = ac.loc[ac["station"].isin(["AC01", "AC02"])].copy()

    west_minx, west_miny, west_maxx, west_maxy = ac_west.total_bounds
    east_minx, east_miny, east_maxx, east_maxy = ac_east.total_bounds
    west_bounds = (west_minx - 900, west_miny - 750, west_maxx + 900, west_maxy + 750)
    east_bounds = (east_minx - 750, east_miny - 650, east_maxx + 750, east_maxy + 650)

    west_rgb, west_image_extent = read_rgb(west_bounds, max_pixels=1300)
    east_rgb, east_image_extent = read_rgb(east_bounds, max_pixels=1300)
    countries = read_natural_earth()

    fig = plt.figure(figsize=(7.20, 3.72), constrained_layout=False)
    grid = fig.add_gridspec(
        1,
        3,
        width_ratios=(0.82, 1.32, 1.18),
        left=0.035,
        right=0.99,
        bottom=0.18,
        top=0.925,
        wspace=0.08,
    )
    ax_a = fig.add_subplot(grid[0, 0])
    ax_b = fig.add_subplot(grid[0, 1])
    validation_grid = grid[0, 2].subgridspec(2, 1, height_ratios=(1, 1), hspace=0.30)
    ax_c = fig.add_subplot(validation_grid[0, 0])
    ax_d = fig.add_subplot(validation_grid[1, 0])

    # (a) Regional locator.
    west_africa = countries.cx[-26:18, 3:33]
    west_africa.plot(ax=ax_a, color="#F0EEE9", edgecolor="#8A8A8A", linewidth=0.35)
    mauritania = countries.loc[countries["ADMIN"].eq("Mauritania")]
    mauritania.plot(ax=ax_a, color="#D4C9B8", edgecolor="#4A4A4A", linewidth=0.65)
    study_wgs = gpd.GeoSeries([extent.geometry.union_all().centroid], crs=extent.crs).to_crs("EPSG:4326")
    study_lon = study_wgs.x.iloc[0]
    study_lat = study_wgs.y.iloc[0]
    ax_a.scatter(study_lon, study_lat, s=33, c="#D55E00", edgecolor="white", linewidth=0.8, zorder=8)
    ax_a.text(
        study_lon + 1.6,
        study_lat + 0.3,
        "Banc d’Arguin",
        fontsize=7.4,
        fontweight="bold",
        color="#222222",
        va="center",
    )
    ax_a.set_xlim(-26, 18)
    ax_a.set_ylim(3, 33)
    ax_a.set_aspect("equal", adjustable="box")
    ax_a.set_xticks([-20, -10, 0, 10])
    ax_a.set_yticks([5, 15, 25])
    ax_a.tick_params(length=2.5, width=0.5, labelsize=7, colors="#555555")
    ax_a.set_xlabel("Longitude", fontsize=7.5)
    ax_a.set_ylabel("Latitude", fontsize=7.5)
    ax_a.set_title("a  Regional setting", pad=6)
    for spine in ax_a.spines.values():
        spine.set_linewidth(0.65)
        spine.set_edgecolor("#4A4A4A")

    # (b) Banc d'Arguin intertidal domain and campaign stations. A vector-only
    # overview avoids introducing contextual imagery from dates outside the
    # field campaign; the same-day image is reserved for the validation insets.
    ax_b.set_facecolor("#EAF2F3")
    intertidal.plot(
        ax=ax_b,
        facecolor="#34B6B2",
        edgecolor="white",
        linewidth=0.45,
        alpha=0.92,
        zorder=5,
    )
    intertidal.boundary.plot(ax=ax_b, color="#007F7F", linewidth=0.45, zorder=6)
    ax_b.scatter(
        points.geometry.x,
        points.geometry.y,
        s=7.5,
        facecolor="#F6F6F6",
        edgecolor="#222222",
        linewidth=0.35,
        alpha=0.9,
        zorder=10,
    )
    ax_b.set_xlim(full_bounds[0], full_bounds[2])
    ax_b.set_ylim(full_bounds[1], full_bounds[3])
    ax_b.set_title("b  Intertidal study domain", pad=6)
    clean_projected_axis(ax_b)
    draw_scale_bar(ax_b, 10)
    draw_north_arrow(ax_b)

    # (c--d) Same-day validation stations shown at useful local scales.
    for axis, subset, image_data, image_extent, bounds, title, scale in (
        (ax_c, ac_west, west_rgb, west_image_extent, west_bounds, "c  Offshore sites (AC03–08)", 1),
        (ax_d, ac_east, east_rgb, east_image_extent, east_bounds, "d  Coastal sites (AC01–02)", 0.5),
    ):
        axis.imshow(image_data, extent=image_extent, origin="upper", interpolation="bilinear")
        outline_intertidal(axis, intertidal)
        for _, row in subset.iterrows():
            station = str(row["station"])
            colour = STATION_COLOURS[station]
            text_colour = "#111111" if station in {"AC02", "AC03", "AC05"} else "white"
            axis.scatter(
                row.geometry.x,
                row.geometry.y,
                s=54,
                facecolor=colour,
                edgecolor="white",
                linewidth=0.85,
                zorder=15,
            )
            axis.text(
                row.geometry.x,
                row.geometry.y,
                station[-1],
                fontsize=6.2,
                fontweight="bold",
                color=text_colour,
                ha="center",
                va="center",
                zorder=16,
            )
        axis.set_xlim(bounds[0], bounds[2])
        axis.set_ylim(bounds[1], bounds[3])
        axis.set_title(title, pad=4)
        clean_projected_axis(axis)
        draw_scale_bar(axis, scale)

    legend_handles = [
        Patch(facecolor="#00A6A6", edgecolor="#007F7F", alpha=0.35, label="Intertidal domain"),
        Line2D(
            [0],
            [0],
            marker="o",
            color="none",
            markerfacecolor="#F6F6F6",
            markeredgecolor="#222222",
            markersize=4.5,
            label="Field station (20–30 March)",
        ),
    ]
    station_handles = []
    for _, row in ac.sort_values("station").iterrows():
        station = str(row["station"])
        station_handles.append(
            Line2D(
                [0],
                [0],
                marker="o",
                color="none",
                markerfacecolor=STATION_COLOURS[station],
                markeredgecolor="white",
                markeredgewidth=0.7,
                markersize=6,
                label=station,
            )
        )

    fig.legend(
        handles=legend_handles,
        loc="lower left",
        bbox_to_anchor=(0.035, 0.022),
        frameon=False,
        ncol=2,
        fontsize=7.2,
        handlelength=1.7,
        columnspacing=1.2,
    )
    fig.legend(
        handles=station_handles,
        loc="lower right",
        bbox_to_anchor=(0.99, 0.01),
        frameon=False,
        ncol=4,
        fontsize=7.0,
        handletextpad=0.35,
        columnspacing=0.8,
    )

    png = FIGURE_DIR / "figure1_study_area.png"
    fig.savefig(png, dpi=600)
    plt.close(fig)


if __name__ == "__main__":
    main()
