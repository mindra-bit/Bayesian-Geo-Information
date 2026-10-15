cat("Working directory:", normalizePath(getwd()), "\n\n")
required_files <- c(
  "app.R", "R/data.R", "R/helpers.R", "R/data_helpers.R",
  file.path("data","raw","Data BPS.csv"),
  file.path("data","raw","BMKG_Earthquake_Catalog.csv"),
  file.path("data","raw","DATA_VS30_LENGKAP.csv"),
  file.path("data","raw","Batas_Desa_Kelurahan_JABAR.gpkg"),
  file.path("data","raw","titik_sesar_lembang.kmz"),
  file.path("analysis","Lembang_IJGI_FULL_REPRODUCIBLE_RUN_v4_5_1_FIXED.Rmd")
)
missing <- required_files[!file.exists(required_files)]
if(length(missing)) stop("Missing project files:\n", paste0(" - ", missing, collapse="\n"))

pkgs <- c("shiny","bslib","DT","ggplot2","dplyr","readr","data.table","scales","jsonlite","plotly","leaflet","sf","htmltools","rmarkdown","callr","tidyr")
ok <- vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))
print(data.frame(package=pkgs, installed=ok))
if(!all(ok)) stop("Install missing packages with source('scripts/install_packages.R').")

source("R/data_helpers.R")
cat("\nData checks:\n")
cat("  BPS rows :", nrow(load_bps()), "\n")
cat("  BMKG rows:", nrow(load_bmkg()), "\n")
cat("  Vs30 rows:", nrow(load_vs30()), "\n")
cat("\nProject check passed.\n")
cat("INLA installed:", requireNamespace("INLA", quietly=TRUE), "\n")
