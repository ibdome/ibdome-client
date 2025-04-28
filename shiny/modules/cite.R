box::use(
  shiny[...],
  shiny.semantic[...],
  .. / components / page_template[page_template]
  )

# UI
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    div(
      div(class = "ui hidden divider"),
      h1("Citation", class = "ui center aligned header"),
      div(class = "ui hidden divider"),
      h2("If you use the IBDome Explorer in your research, please cite:", class = "ui center aligned header"),
      div(
        class = "ui raised segment",  # Adds a box effect
        style = "max-width: 800px; margin: auto; padding: 20px; text-align: center;",
        p(HTML(
          "<strong>IBDome: An integrated molecular, histopathological, and clinical atlas of inflammatory bowel diseases</strong><br>
          C. Plattner, G. Sturm, A.A. Kühl, R. Atreya, S. Carollo et al.<br>
          <em>bioRxiv, 2025</em><br>
          <a href='https://doi.org/10.1101/2025.03.26.645544' target='_blank'>https://doi.org/10.1101/2025.03.26.645544</a>"
        ), style = "font-size: 16px;")
      ),
      div(class = "ui hidden divider")
    )
  )
}

#' @export
server <- function(input, output, session) {
  # empty
}
