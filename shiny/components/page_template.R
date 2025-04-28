box::use(
  shiny[...],
  shiny.semantic[menu_item, icon],
  shiny.router[route_link],
  .. / config[VERSION]
)

.is_active <- function(menu_id, page_id) {
  ifelse(page_id == menu_id, "active", "")
}

# Menu template function
menu_template <- function(page_id) {
  div(
    class = "ui vertical menu",  # Adjusted to fit the fluidPage layout
    style = "background-color: #d3d3d3;",
    menu_item(
      div(
        a(href = route_link("/"),  # This makes the image clickable
          img(src = "IBDdome_Logo_RZ.png", height = "90px", width = "142px")
        ), 
        br(),
        br(),
        span(VERSION, class = "ui large grey text")
      ),
      item_feature = "header"
    ),
    menu_item(
      href = route_link("/"), icon("home"), "Home",
      item_feature = .is_active("home", page_id),
      style = "color: #000000;"
    ),
    menu_item(
      href = route_link("overview"), icon("info circle"), "Overview",
      item_feature = .is_active("overview", page_id),
      style = "color: #000000;"
    ),
    menu_item(
      href = route_link("gene_expression"), icon("sort amount up"), "Gene Expression",
      item_feature = .is_active("gene_expression", page_id),
      style = "color: #000000;"
    ),
    menu_item(
      href = route_link("correlation"), icon("chart line"), "Correlation",
      item_feature = .is_active("correlation", page_id),
      style = "color: #000000;"
    ),
    menu_item(
      href = route_link("data_download"), icon("download"), "Data Download",
      item_feature = .is_active("data_download", page_id),
      style = "color: #000000;"
    ),
    # menu_item(
    #   href = route_link("documentation"), NULL, "Documentation",
    #   item_feature = .is_active("documentation", page_id),
    #   style = "color: #000000;"
    # ),
    menu_item(
      href = route_link("changelog"), NULL, "Changelog",
      item_feature = .is_active("changelog", page_id),
      style = "color: #000000;"
    ),
    menu_item(
      href = route_link("cite"), NULL, "Citation",
      item_feature = .is_active("Cite", page_id),
      style = "color: #000000;"
    )
  )
}


# Main page layout function using fluidPage and Flexbox for sidebar
#' @export
page_template <- function(page_id, ...) {
  # Use Flexbox for the sidebar and main content
  fluidPage(
    div(
      style = "display: flex; flex-direction: row; height: 100vh;",  # Flexbox layout for the sidebar and main content
      div(
        style = "background-color: #d3d3d3; padding: 10px; width: 250px; flex-shrink: 0;",  # Sidebar with fixed width
        menu_template(page_id)
      ),
      div(
        style = "flex-grow: 1; display: flex; flex-direction: column;",  # Main content with a column layout for content + footer
        div(
          style = "flex-grow: 1; padding: 20px; overflow: auto;",  # Main content container
          fluidPage(  # Use fluidPage within the content to ensure responsiveness
            fluidRow(
              column(
                width = 12,  
                ...  # The dynamic content passed to the template
              )
            )
          )
        ),
        div(
          style = "background-color: #f0f0f0; padding: 10px; text-align: center; flex-shrink: 0; display: flex; flex-direction: column; align-items: center;",  # Footer styling
          
            div(
            style = "display: flex; justify-content: center; align-items: center; flex-wrap: wrap;",  
              a(href = route_link("imprint"),  "IMPRINT"),
              " | ",  # Separator between links
              a(href = route_link("privacy"),  "PRIVACY POLICY"),
            ),
          div(
              style = "display: flex; justify-content: center; align-items: center; gap: 5vw; flex-wrap: wrap;",
              "© 2025 TRR241. All rights reserved."
              ),
          
        )
      )
    )
  )
}