# Raw-data archive

The reproducible workflow expects the following file at this location:

`data/archive/Lembang_raw_data.zip`

Expected SHA-256:

`82672c241e8a72b37afc396b82c6b9cb6ac33eb3525510445746012ca8670bb4`

The archive contains:

- `Data BPS.csv`
- `DATA_VS30_LENGKAP.csv`
- `BMKG_Earthquake_Catalog.csv`
- `Batas_Desa_Kelurahan_JABAR.gpkg`
- `titik_sesar_lembang.kmz`

The archive is not committed automatically by the ChatGPT GitHub connector because that connector supports repository text-file writes but not direct upload of an arbitrary local binary ZIP. Add the prepared archive manually to this path (or distribute it through a release/DOI archive) after verifying upstream redistribution terms.

After the archive is present, run:

```r
source("scripts/unpack_data.R")
```
