pkgs <- c(
  "shiny","bslib","DT","ggplot2","dplyr","readr","data.table","scales",
  "jsonlite","plotly","leaflet","sf","htmltools","rmarkdown","knitr",
  "tidyr","later","callr","zoo","psych","viridis","patchwork","ggrepel","purrr","tibble"
)
to_install <- pkgs[!vapply(pkgs, requireNamespace, quietly=TRUE, FUN.VALUE=logical(1))]
if(length(to_install)){
  install.packages(to_install,repos="https://cloud.r-project.org")
} else {
  message("All required CRAN packages are already installed.")
}
