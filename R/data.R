APP_VERSION <- "4.5.0"

top10 <- readr::read_csv(
  file.path("data", "top10_screening.csv"),
  show_col_types = FALSE
)

sensitivity <- readr::read_csv(
  file.path("data", "sensitivity_analysis.csv"),
  show_col_types = FALSE
)

models <- readr::read_csv(
  file.path("data", "model_comparison.csv"),
  show_col_types = FALSE
)

metadata <- jsonlite::fromJSON(
  file.path("data", "metadata.json"),
  simplifyVector = FALSE
)

best_dic <- dplyr::slice_min(models, DIC, n = 1, with_ties = FALSE)
best_waic <- dplyr::slice_min(models, WAIC, n = 1, with_ties = FALSE)
best_cpo <- dplyr::slice_max(models, sum_log_CPO, n = 1, with_ties = FALSE)
