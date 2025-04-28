box::use(
  shiny[...],
  shiny.semantic[...],
  .. / components / page_template[page_template],
  shiny.router[make_router, route, route_link],
)

# UI
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    div(
      div(class = "ui hidden divider"),
      div(class = "ui hidden divider"),
      h1("Documentation", style = "margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider"),
      h2("RNAseq-analysis ", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("... "),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;")
    )
    
  )
}
