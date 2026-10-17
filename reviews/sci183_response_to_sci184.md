# SCI-183 response to SCI-184

## Disposition

Hermione's requested corrections are accepted. The earlier phrase “brightness-dependent correction mechanism” is withdrawn. The supported result is a difference in between-pixel contrast between processors within two intertidal AOIs. It does not identify mechanism, accuracy, or a seagrass-specific effect.

## Restored base and reproducibility

Work was restarted from a clean, detached restoration of `origin/main` at `cf273626600fb165401187477ee4ffbbce872ae7`. The restored tree matched `origin/main`; the pre-existing dirty shared checkout was not reset or edited. The supplementary script is `scripts/review_revision_delta_rho_sci183.R` and reads only the 21 exact Sen2Cor/ACOLITE date pairs.

## Primary continuous effect

For each pixel, B02–B04 reflectance was averaged within acquisition and then median-composited across dates. OLS slopes of ACOLITE on Sen2Cor pixel medians were 0.584 for Arel and 0.594 for Iwik. Spatial-block bootstrap intervals were 0.510–0.657 and 0.505–0.700, respectively (2,000 resamples of 4 × 4 within-AOI coordinate-quantile blocks). Because OLS treats Sen2Cor as error-free, these slopes are subject to regression dilution and are reported alongside the ACOLITE/Sen2Cor spatial SD ratios (0.665 and 0.689) and the paired-product correlations (`r = 0.879` and `0.862`, obtained as slope/SD ratio). Together, these diagnostics show lower spatial variation in ACOLITE than Sen2Cor, but they do not establish a causal mechanism.

## Coupling and scorer sensitivity

Coupling was substantial: `r(mean, delta)` was −0.660 in Arel and −0.601 in Iwik. The lower-minus-higher tercile contrast remained positive when the stratifier was ACOLITE alone, despite that scorer's opposite mechanical bias: +0.01334 (Arel) and +0.00509 (Iwik). Corresponding symmetric-mean contrasts were +0.01849 and +0.00675; Sen2Cor-only contrasts were +0.02036 and +0.00790. The magnitude is therefore scorer-dependent and no single tercile contrast should be presented without this range.

Median relative differences `(ACOLITE − Sen2Cor) / Sen2Cor` are included in `brightness_scorer_triplet.csv`. Under the symmetric score they were −0.273 versus −0.283 in Arel's lower/higher terciles and −0.086 versus −0.099 in Iwik's lower/higher terciles.

## Terminology and scientific scope

The strata are renamed **lower- and higher-brightness terciles within AOI**. “Dark water,” “dark vegetation,” and “seagrass correction difference” are not supported. The high visible reflectance is more consistent with optically bright, potentially emerged intertidal substrate than with a validated submerged-seagrass target. Exposed-sediment moisture, microphytobenthos, unrecorded tide state, sunglint/shadow, adjacency, mixed pixels, resampling, and scene-processing differences remain plausible alternatives.

Neither processor is ground truth. The result quantifies disagreement only. A constant relative offset and contrast compression are not separately identifiable from these paired observational data, so neither is claimed as the unique explanation. Each AOI contributes only 16 spatial blocks and the AOI-specific block-bootstrap intervals overlap; no difference between Arel and Iwik is inferred. The x/y coordinates are treated as projected metres; EPSG:32628 is inferred from linked project products but is not encoded in the CSV metadata and therefore remains an explicit assumption.

Every paired date retained exactly 730 Arel and 733 Iwik coordinate rows. No QA field or masking record was evident in the supplied CSVs, so the analysis cannot demonstrate cloud, shadow, glint, water or exposure screening. The identical counts should not be interpreted as proof of uniformly valid observations.

## Revised outputs

- `outputs/processor_spatial_contrast_slopes.csv`: slope, spatial-block interval, SD ratio, and coupling correlations.
- `outputs/brightness_scorer_triplet.csv`: mean-, Sen2Cor-, and ACOLITE-defined terciles, absolute and relative differences.
- `outputs/valid_acquisitions_by_date_aoi.csv`: valid counts for each of the 21 paired dates and two AOIs.
- `outputs/SCI183_REVIEW_REVISION_METADATA.txt`: definitions, resampling design, and CRS assumption.

The earlier Figures 901–903 retain obsolete “dark/bright” labels and are withdrawn from downstream use. Frida should use the revised CSVs only until replacement graphics receive separate sign-off.

## Review disposition

Hermione approved the revision in SCI-185 subject to the four reporting conditions now incorporated above: regression-dilution qualification; non-separability of relative offset and contrast compression; no AOI-difference claim; and explicit disclosure of the identical counts and absent evident QA masking.
