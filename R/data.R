APP_VERSION <- "4.5.0"

resolve_data_file <- function(filename) {
  candidates <- c(
    file.path("data", "derived", filename),
    file.path("data", "example_outputs", filename),
    file.path("data", filename)
  )
  hit <- candidates[file.exists(candidates)]
  if (!length(hit)) stop("Missing data file: ", filename)
  hit[1]
}

top10 <- readr::read_csv(
  resolve_data_file("top10_screening.csv"),
  show_col_types = FALSE
)

sensitivity <- readr::read_csv(
  resolve_data_file("sensitivity_analysis.csv"),
  show_col_types = FALSE
)

models <- readr::read_csv(
  resolve_data_file("model_comparison.csv"),
  show_col_types = FALSE
)

metadata <- jsonlite::fromJSON(
  file.path("data", "metadata.json"),
  simplifyVector = FALSE
)

best_dic <- dplyr::slice_min(models, DIC, n = 1, with_ties = FALSE)
best_waic <- dplyr::slice_min(models, WAIC, n = 1, with_ties = FALSE)
best_cpo <- dplyr::slice_max(models, sum_log_CPO, n = 1, with_ties = FALSE)
