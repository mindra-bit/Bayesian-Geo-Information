# Raw-data packaging

The preferred public layout is now `data/source/`, where each dataset is inspectable separately.

Expected public files:

- `data/source/Data BPS.csv`
- `data/source/DATA_VS30_LENGKAP.csv`
- `data/source/BMKG_Earthquake_Catalog.csv.gz`
- `data/source/titik_sesar_lembang.kml`
- `data/source/boundaries/*.geojson`

The administrative boundary dataset is partitioned by kabupaten/kota because the original GeoPackage is about 107 MB. Run:

```r
source("scripts/unpack_data.R")
```

The script rebuilds the working GeoPackage in `data/raw/` and prepares the other raw inputs.

## Legacy archive

The older workflow can still use:

`data/archive/Lembang_raw_data.zip`

Expected SHA-256:

`82672c241e8a72b37afc396b82c6b9cb6ac33eb3525510445746012ca8670bb4`

The legacy archive contains the five original source files. It is optional when the preferred `data/source/` layout is available.
