cat("Running the complete Lembang IJGI v4.5.1 reproducible analysis...\n")

required <- c("rmarkdown","INLA")
miss <- required[!vapply(required, requireNamespace, logical(1), quietly=TRUE)]
if(length(miss)) stop("Missing package(s): ", paste(miss, collapse=", "))

project_dir <- normalizePath(".", mustWork=TRUE)
rmd_file <- file.path(project_dir, "analysis", "Lembang_IJGI_FULL_REPRODUCIBLE_RUN_v4_5_1_FIXED.Rmd")
if(!file.exists(rmd_file)){
  stop("Place Lembang_IJGI_FULL_REPRODUCIBLE_RUN_v4_5_1_FIXED.Rmd inside the analysis/ folder.")
}

raw_dir <- normalizePath(file.path(project_dir, "data", "raw"), mustWork=TRUE)

cat("Project directory:", project_dir, "\n")
cat("Raw data directory:", raw_dir, "\n")

rmarkdown::render(
  input = rmd_file,
  output_format = "html_document",
  params = list(data_dir = raw_dir),
  envir = new.env(parent = globalenv()),
  quiet = FALSE,
  clean = FALSE
)

cat("\nFULL ANALYSIS COMPLETED.\n")
cat("Derived outputs: data/derived/\n")
cat("Manuscript-ready outputs: data/manuscript/\n")
