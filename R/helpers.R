table_ui <- function(items) {
  tags$table(
    class = "table table-sm app-table",
    tags$tbody(
      lapply(names(items), function(nm) {
        tags$tr(tags$th(nm), tags$td(items[[nm]]))
      })
    )
  )
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
