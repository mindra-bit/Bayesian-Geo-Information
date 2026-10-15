cat("Checking INLA background-run prerequisites...\n\n")

required <- c(
  "INLA","rmarkdown","callr","sf","dplyr","tidyr","zoo","psych",
  "ggplot2","viridis","patchwork","ggrepel","knitr","purrr","readr","tibble"
)

ok <- vapply(required,requireNamespace,quietly=TRUE,FUN.VALUE=logical(1))
print(data.frame(package=required,installed=ok),row.names=FALSE)

if(!all(ok)){
  stop(
    "Missing package(s): ",
    paste(required[!ok],collapse=", "),
    "\nRun source('install_packages.R') and source('install_inla.R') as needed."
  )
}

required_files <- c(
  file.path("analysis","Lembang_IJGI_Reproducible_Analysis.Rmd"),
  file.path("data","raw","Data BPS.csv"),
  file.path("data","raw","BMKG_Earthquake_Catalog.csv"),
  file.path("data","raw","DATA_VS30_LENGKAP.csv"),
  file.path("data","raw","Batas_Desa_Kelurahan_JABAR.gpkg"),
  file.path("data","raw","titik_sesar_lembang.kmz")
)

missing <- required_files[!file.exists(required_files)]
if(length(missing)){
  stop("Missing file(s):\n",paste0(" - ",missing,collapse="\n"))
}

dir.create(file.path("data","derived"),showWarnings=FALSE,recursive=TRUE)
testfile <- file.path("data","derived",".write_test")
writeLines("ok",testfile)
unlink(testfile)

cat("\nMain R session:\n")
cat("R:",R.version.string,"\n")
cat(".libPaths():\n",paste(.libPaths(),collapse="\n"),"\n")
cat("INLA version:",as.character(utils::packageVersion("INLA")),"\n")

cat("\nTesting a clean background R session with the same library paths...\n")
child <- callr::r(
  function(libpaths){
    .libPaths(unique(c(libpaths,.libPaths())))
    list(
      R=R.version.string,
      libPaths=.libPaths(),
      INLA=requireNamespace("INLA",quietly=TRUE),
      rmarkdown=requireNamespace("rmarkdown",quietly=TRUE),
      knitr=requireNamespace("knitr",quietly=TRUE)
    )
  },
  args=list(libpaths=.libPaths()),
  libpath=.libPaths()
)

print(child)

if(!isTRUE(child$INLA)){
  stop("INLA is visible in the Shiny session but NOT in a clean callr child session.")
}
if(!isTRUE(child$rmarkdown) || !isTRUE(child$knitr)){
  stop("rmarkdown/knitr are not visible in the child session.")
}

cat("\nBackground-session preflight passed.\n")
cat("The dashboard can now launch INLA in a separate R process.\n")
