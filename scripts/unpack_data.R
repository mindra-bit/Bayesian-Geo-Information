source_dir <- file.path("data", "source")
archive <- file.path("data", "archive", "Lembang_raw_data.zip")
outdir <- file.path("data", "raw")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

needed <- c(
  "Data BPS.csv",
  "DATA_VS30_LENGKAP.csv",
  "BMKG_Earthquake_Catalog.csv",
  "Batas_Desa_Kelurahan_JABAR.gpkg",
  "titik_sesar_lembang.kml"
)

if (file.exists(archive)) {
  unzip(archive, exdir = outdir)
  kmz <- file.path(outdir, "titik_sesar_lembang.kmz")
  if (file.exists(kmz) && !file.exists(file.path(outdir, "titik_sesar_lembang.kml"))) {
    td <- tempfile("lembang_kmz_")
    dir.create(td)
    unzip(kmz, exdir = td)
    kml <- list.files(td, pattern = "\\.kml$", full.names = TRUE, recursive = TRUE)
    if (length(kml)) file.copy(kml[1], file.path(outdir, "titik_sesar_lembang.kml"), overwrite = TRUE)
  }
} else if (dir.exists(source_dir)) {
  direct <- c("Data BPS.csv", "DATA_VS30_LENGKAP.csv")
  for (f in direct) {
    src <- file.path(source_dir, f)
    if (file.exists(src)) file.copy(src, file.path(outdir, f), overwrite = TRUE)
  }

  bmkg_csv <- file.path(source_dir, "BMKG_Earthquake_Catalog.csv")
  bmkg_gz <- paste0(bmkg_csv, ".gz")
  if (file.exists(bmkg_csv)) {
    file.copy(bmkg_csv, file.path(outdir, basename(bmkg_csv)), overwrite = TRUE)
  } else if (file.exists(bmkg_gz)) {
    con_in <- gzfile(bmkg_gz, "rb")
    con_out <- file(file.path(outdir, basename(bmkg_csv)), "wb")
    repeat {
      buf <- readBin(con_in, what = "raw", n = 1024 * 1024)
      if (!length(buf)) break
      writeBin(buf, con_out)
    }
    close(con_in); close(con_out)
  }

  kml <- file.path(source_dir, "titik_sesar_lembang.kml")
  if (file.exists(kml)) file.copy(kml, file.path(outdir, basename(kml)), overwrite = TRUE)

  boundary_dir <- file.path(source_dir, "boundaries")
  boundary_files <- list.files(boundary_dir, pattern = "\\.geojson$", full.names = TRUE)

  # Reviewer-friendly alternative: upload small ZIP parts directly under data/source/.
  boundary_zips <- list.files(source_dir, pattern = "^boundaries_part_[0-9]+\\.zip$", full.names = TRUE)
  if (!length(boundary_files) && length(boundary_zips)) {
    tmp_boundaries <- tempfile("lembang_boundaries_")
    dir.create(tmp_boundaries)
    for (z in boundary_zips) unzip(z, exdir = tmp_boundaries)
    boundary_files <- list.files(tmp_boundaries, pattern = "\\.geojson$", full.names = TRUE)
  }

  if (length(boundary_files)) {
    if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' is required to rebuild the GeoPackage.")
    parts <- lapply(boundary_files, sf::st_read, quiet = TRUE)
    all_boundaries <- do.call(rbind, parts)
    gpkg <- file.path(outdir, "Batas_Desa_Kelurahan_JABAR.gpkg")
    if (file.exists(gpkg)) file.remove(gpkg)
    sf::st_write(all_boundaries, gpkg, layer = "Batas_Desa_Kelurahan_JABAR", quiet = TRUE)
  }
} else {
  stop("No raw-data archive or data/source directory found.")
}

# Backward compatibility for analyses that still reference the KMZ filename.
if (!file.exists(file.path(outdir, "titik_sesar_lembang.kmz")) &&
    file.exists(file.path(outdir, "titik_sesar_lembang.kml"))) {
  message("Using KML fault trace: data/raw/titik_sesar_lembang.kml")
}

check <- c(
  "Data BPS.csv",
  "DATA_VS30_LENGKAP.csv",
  "BMKG_Earthquake_Catalog.csv",
  "Batas_Desa_Kelurahan_JABAR.gpkg"
)
missing <- check[!file.exists(file.path(outdir, check))]
if (length(missing)) stop("Data preparation incomplete: ", paste(missing, collapse = ", "))
cat("Raw data prepared in", normalizePath(outdir), "\n")
