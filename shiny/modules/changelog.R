box::use(
  shiny[NS, includeMarkdown],
  .. / components / page_template[page_template],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    includeMarkdown("CHANGELOG.md")
  )
}

#' @export
server <- function(input, output, session) {
  # empty
}
