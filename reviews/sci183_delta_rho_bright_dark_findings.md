# SCI-183 — Δρ over optically dark and bright surfaces

## Objective

Test whether the paired atmospheric-correction difference, Δρ = ρ_ACOLITE − ρ_Sen2Cor, differs between optically dark and bright surfaces, with particular attention to the relevance and limitations of the result for seagrass.

## Data and provenance

- Immutable inputs: `Data/Atm_Compare_AOI/{Sen2Cor,ACOLITE}/*.csv` at Git commit `cf273626600fb165401187477ee4ffbbce872ae7` (manuscript v15).
- The archive contains 24 Sen2Cor files and 22 ACOLITE files. There are 21 exact filename/date pairs. Sen2Cor-only dates are 2025-01-21, 2025-01-31 and 2025-02-20; the ACOLITE-only date is 2025-02-15. No near-date substitution was made.
- The 21 paired dates contain 1,463 spatial pixels across Arel (730) and Iwik (733). Eleven bands are paired (B01–B08, B8A, B11 and B12); ACOLITE B09 is absent.
- Coordinates are projected metres in the input tables. The CRS is not encoded in the CSVs; linked project products document EPSG:32628, but this analysis does not independently verify that attribution and performs no area calculation.
- `Data/Matchup_timeseries` was inspected for context but not joined: it represents point time series and lacks a documented pixel-level link to the two AOI grids.

## Method

The reproducible script is `scripts/analyse_delta_rho_bright_dark_sci183.R`. It exact-matches processor observations by date, AOI and projected x/y coordinate and never edits `Data/`.

“Dark” and “bright” are optical strata. For each pixel, the brightness score is the across-date median of the symmetric processor mean, averaged over B02, B03 and B04. Thresholds are calculated separately within Arel and Iwik so that strata do not simply reproduce the large site-level brightness difference. The primary rule retains the darkest and brightest thirds and excludes the middle third. Sensitivity rules use the lowest/highest quartiles, lowest/highest 40%, and a median split. The primary thresholds are 0.36834/0.38375 at Arel and 0.33386/0.34023 at Iwik; each stratum contains 489 pixels.

For each band and stratum, the script reports both processors' reflectance and paired Δρ. It first computes median Δρ within each date × AOI × stratum, forms the dark-minus-bright contrast within AOI, then averages the two AOI contrasts. The reported estimate is the median of 21 date-level contrasts. A nonparametric date bootstrap (2,000 replicates; seed 183) gives a temporal-sampling interval. Pixels are not treated as independent replicates.

## Findings

ACOLITE reflectance was lower than Sen2Cor reflectance in both optical strata for all four focal bands. Under the primary rule, pooled descriptive medians of Δρ were −0.0605 (dark) versus −0.0727 (bright) in B02, −0.0540 versus −0.0662 in B03, −0.0550 versus −0.0665 in B04, and −0.0627 versus −0.0709 in B08.

Accordingly, dark-minus-bright Δρ was positive: the ACOLITE–Sen2Cor difference was less negative over darker pixels. Primary date-level median contrasts (95% date-bootstrap interval) were:

| Band | Dark − bright Δρ |
|---|---:|
| B02 | 0.01225 (0.01135, 0.01382) |
| B03 | 0.01205 (0.01095, 0.01444) |
| B04 | 0.01196 (0.01058, 0.01437) |
| B08 | 0.00763 (0.00680, 0.00871) |

The direction was unchanged under all threshold rules. For B02/B03/B04/B08, estimates ranged respectively from 0.00893–0.01460, 0.00907–0.01377, 0.00849–0.01348 and 0.00606–0.00871. Across all 11 paired bands and all four threshold rules, the date-bootstrap intervals remained above zero. This is a consistency result for these observations, not a general performance guarantee.

## Scientific interpretation and limitations

The supported result is a difference in between-pixel contrast between processors within two intertidal AOIs. A continuous re-analysis found ACOLITE-on-Sen2Cor OLS slopes of 0.584 (spatial-block interval 0.510–0.657) at Arel and 0.594 (0.505–0.700) at Iwik. These slopes are subject to regression dilution because OLS treats Sen2Cor as error-free; the corresponding ACOLITE/Sen2Cor spatial SD ratios were 0.665 and 0.689, and paired-product correlations were 0.879 and 0.862. A constant relative offset and contrast compression are not separable with these data. The observations therefore establish processor disagreement, not its mechanism.

Important limitations:

1. Optical darkness is not a habitat label. No pixel-level seagrass ground truth is joined, so the result cannot be stated as “the seagrass correction difference.” Water depth, turbidity, wet sediment, shadow and other dark targets are plausible alternatives.
2. Neither processor is independent truth. Δρ quantifies disagreement, not bias or accuracy.
3. The brightness score uses the same two products that define Δρ, creating possible mathematical coupling. A sensitivity analysis using an independent surface-brightness reference or field/UAV habitat map is recommended.
4. Thresholds are data-relative and AOI-specific; they are not transferable absolute reflectance limits.
5. The date bootstrap addresses temporal sampling only. Residual spatial autocorrelation and the small number of AOIs (two) limit spatial generalization.
6. Acquisition geometry, atmosphere, tide/exposure, adjacency and quality masking were not available as covariates in these CSVs. Every paired date retained exactly 730 Arel and 733 Iwik coordinate rows, and no QA field or masking record was evident. These identical counts do not establish that all observations are cloud-, shadow-, glint-, water- or exposure-screened.
7. The reflectances are unexpectedly high for a simple submerged-seagrass interpretation. This reinforces the need to validate habitat/exposure status before ecological attribution.
8. Spatial-block inference used only 16 coordinate-quantile blocks per AOI. The AOI-specific intervals overlap, so no difference between Arel and Iwik is claimed.

## Hermione-ready interpretation brief

Across 21 exactly paired scenes, ACOLITE returned lower reflectance than Sen2Cor in both within-AOI brightness strata. The lower-minus-higher brightness contrast in Δρ (ACOLITE − Sen2Cor) was approximately 0.012 in the blue, green and red bands and 0.0076 in NIR, with the direction stable across four threshold rules. Continuous diagnostics show that ACOLITE has lower between-pixel spatial variation, but OLS regression dilution and the non-separability of relative offset from contrast compression prevent a unique mechanistic interpretation. This is not a seagrass-specific bias, an AOI-difference result or an accuracy ranking: habitat is unvalidated, neither product is ground truth, only 16 spatial blocks represent each AOI, and no evident QA masking metadata accompany the identical per-date pixel counts.

## Outputs and recommended next work

The task bundle contains the complete summary and date-level tables, input inventory, thresholds, pixel brightness scores and run metadata. Figures 901–903 are retained only as an audit trail and are withdrawn from downstream use because their labels pre-date the reviewed terminology.

Recommended follow-up: overlay an independently validated seagrass/habitat layer on the AOI pixels; repeat stratification using an independent brightness reference; model Δρ against continuous brightness with date and spatial structure; and add acquisition/tide/quality covariates where provenance permits. Frida should treat the supplied figures as analytical candidates and use the CSVs for any publication design.
