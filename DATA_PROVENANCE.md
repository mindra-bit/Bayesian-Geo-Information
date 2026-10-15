# Data provenance and redistribution note

This repository bundles the exact working inputs used by the reproducible workflow. The manuscript should remain the authoritative source for complete citations and methodological definitions.

## Raw inputs

| File | Role in analysis | Upstream source / provenance |
|---|---|---|
| `Data BPS.csv` | Annual sub-district socioeconomic attributes: facility counts, population density, telecommunication signal/connectivity, education facilities | Statistics Indonesia / BPS-derived working compilation supplied for the study |
| `DATA_VS30_LENGKAP.csv` | Gridded Vs30 site-condition data | USGS Global Vs30-derived working dataset |
| `BMKG_Earthquake_Catalog.csv` | Earthquake event catalog used to construct annual modeled shaking-exceedance fractions | BMKG Indonesian earthquake catalog working extract |
| `Batas_Desa_Kelurahan_JABAR.gpkg` | West Java village/kelurahan administrative polygons aggregated to sub-districts | Administrative boundary working layer supplied for the study |
| `titik_sesar_lembang.kmz` | Lembang Fault geometry | Study fault-trace working layer |

## Important notes

- `Sinyal` is used only as a provisional 2024 **telecommunication-access vulnerability proxy**; lower connectivity is interpreted as higher relative vulnerability after inverse scaling.
- The bundled working files may have redistribution conditions set by their original providers. Before making the GitHub repository public, verify the current terms and attribution requirements for BMKG, BPS, USGS, and administrative/fault geometry sources.
- No raw source should be represented as created by the manuscript authors unless that is explicitly documented by the original source.
