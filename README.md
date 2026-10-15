# Lembang Fault Bayesian Geo-Information Screening

Reproducible R code and data package for the manuscript:

**Bayesian Geo-Information Screening of Seismic Shaking, Exposure, and Vulnerability in the Lembang Fault Region, West Java, Indonesia**

This repository implements a **comparative sub-district seismic screening workflow**. It is not an ETAS earthquake-occurrence forecast and it is not a physical-loss model. The workflow integrates a deterministic ground-motion proxy, Vs30, exposure, a 2024 telecommunication-access vulnerability proxy, and second-stage Bayesian spatial-temporal smoothing using INLA-SPDE and RW1.

## Key distinction from Jaya et al. (2026, Sustainability)

Jaya et al. use a Bayesian spatio-temporal ETAS model for conditional short-horizon seismicity forecasting and a Dynamic Preparedness-Priority Index for Bandung City. This repository instead performs **ground-motion-exceedance screening**, site-condition integration, exposure/vulnerability integration, and district-level Bayesian smoothing across the broader Lembang Fault study region. It does not estimate triggering, branching ratios, or future event counts.

## Repository structure

- `analysis/` — full reproducible R Markdown analysis and original sensitivity syntax.
- `R/` — helper functions used by the Shiny/analysis workflow.
- `app.R` and `run_app.R` — companion Shiny dashboard source and launcher.
- `www/` — dashboard static assets.
- `scripts/` — package installation, checks, and data extraction.
- `data/archive/Lembang_raw_data.zip` — compressed source-data bundle.
- `data/example_outputs/` — selected outputs from the completed aligned workflow.
- `run_analysis.R` — one-command rendering script for the full analysis.

## Data preparation

The administrative GeoPackage is larger than GitHub's normal 100 MB per-file limit when uncompressed. Therefore, all five raw inputs are distributed in one compressed archive below that limit.

From the repository root run:

```r
source("scripts/unpack_data.R")
```

This creates `data/raw/` with:

1. `Data BPS.csv`
2. `DATA_VS30_LENGKAP.csv`
3. `BMKG_Earthquake_Catalog.csv`
4. `Batas_Desa_Kelurahan_JABAR.gpkg`
5. `titik_sesar_lembang.kmz`

`data/raw/` is intentionally ignored by Git after extraction.

## R requirements

Core packages include `sf`, `dplyr`, `tidyr`, `zoo`, `psych`, `ggplot2`, `viridis`, `patchwork`, `ggrepel`, `knitr`, `purrr`, `readr`, `tibble`, `scales`, `rmarkdown`, and `INLA`.

Run the helper installers if needed:

```r
source("scripts/install_packages.R")
source("scripts/install_inla.R")
```

## Reproduce the full analysis

From the repository root:

```r
source("scripts/unpack_data.R")
source("run_analysis.R")
```

The full R Markdown file is:

`analysis/Lembang_IJGI_FULL_REPRODUCIBLE_RUN_v4_5_1_FIXED.Rmd`

Baseline settings encoded in the workflow include:

- study support: 227 harmonized sub-districts;
- socioeconomic panel: 2019–2025;
- earthquake-derived hazard fraction: through 2024, with the 2024 district fraction carried forward as a labeled 2025 temporal proxy;
- scenario magnitude: Mw 6.8;
- scenario depth: 10 km;
- PGA screening threshold: 0.092 g;
- baseline H:E:V weights: 60:30:10;
- likelihood-strength weight: 3;
- PC-prior thresholds: 20 km spatial range and 1.5 spatial SD.

## Companion dashboard

After extracting the data, run:

```r
source("run_app.R")
```

The dashboard provides interactive 2D/3D spatial views, data exploration, ground-motion screening, posterior summaries, INLA diagnostics, ranking, and sensitivity analysis.

## Interpretation limits

The modeled annual exceedance fraction is a **model-derived screening quantity**, not an empirical accelerograph exceedance probability and not an earthquake-occurrence probability. The integrated score is a **relative screening index**, not a calibrated seismic-risk, damage, casualty, or economic-loss estimate. The 2024 vulnerability component is a deliberately narrow inverse BPS `Sinyal` / telecommunication-access proxy and should not be interpreted as a comprehensive social or structural vulnerability index.

## Data provenance

See `DATA_PROVENANCE.md`. Users should verify redistribution and attribution requirements of each upstream source before creating a public release or DOI archive.

## Citation

If this repository is used, cite the associated manuscript and the upstream data/method references listed in the manuscript. See `CITATION.cff` for manuscript metadata.
