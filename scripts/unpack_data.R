archive <- file.path("data", "archive", "Lembang_raw_data.zip")
outdir <- file.path("data", "raw")
if (!file.exists(archive)) stop("Missing archive: ", archive)
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
unzip(archive, exdir = outdir)
needed <- c(
  "Data BPS.csv",
  "DATA_VS30_LENGKAP.csv",
  "BMKG_Earthquake_Catalog.csv",
  "Batas_Desa_Kelurahan_JABAR.gpkg",
  "titik_sesar_lembang.kmz"
)
missing <- needed[!file.exists(file.path(outdir, needed))]
if (length(missing)) stop("Data extraction incomplete: ", paste(missing, collapse=", "))
cat("Raw data extracted to", normalizePath(outdir), "\n")
