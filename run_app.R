if (!file.exists("app.R")) {
  stop("Run this script from the repository root, where app.R is located.")
}
if (!dir.exists(file.path("data","raw"))) {
  message("data/raw not found; extracting bundled data archive...")
  source(file.path("scripts","unpack_data.R"))
}
source(file.path("scripts","check_app.R"))
shiny::runApp(appDir = ".", launch.browser = TRUE)
