box::use(
  shiny[...],
  shiny.semantic[...],
  shiny.router[route, router_ui, router_server],
  . / modules / home,
  . / modules / overview,
  . / modules / gene_expression,
  . / modules / correlation,
  . / modules / changelog,
  . / modules / data_download,
  . / modules / imprint,
  . / modules / accessibility,
  . / modules / privacy,
  # . / modules / documentation,
  . / modules / cite,
  . / config[VERSION]
)

# with box the 'mean_ci' function
# is not found unless attached globally...
library(ggpubr)

# Router UI
router_ui_component <- router_ui(
  route("/", home$ui("home")),
  route("index", home$ui("home")),
  route("overview", overview$ui("overview")),
  route("gene_expression", gene_expression$ui("gene_expression")),
  route("correlation", correlation$ui("correlation")),
  route("data_download", data_download$ui("data_download")),
  route("changelog", changelog$ui("changelog")),
  route("imprint", imprint$ui("imprint")),
  route("accessibility", accessibility$ui("accessibility")),
  route("cite", cite$ui("cite")),
  route("privacy", privacy$ui("privacy"))
)

# Main UI
ui <- semanticPage(
  title = paste("IBDome", VERSION),
  router_ui_component
)

server <- function(input, output, session) {
  router_server()
  moduleServer("index", home$server)
  moduleServer("overview", overview$server)
  moduleServer("gene_expression", gene_expression$server)
  moduleServer("correlation", correlation$server)
  moduleServer("data_download", data_download$server)
  moduleServer("changelog", changelog$server)
  moduleServer("imprint", imprint$server)
  moduleServer("accessibility", accessibility$server)
  moduleServer("privacy", privacy$server)
  moduleServer("cite", cite$server)
}

# Run the app
shinyApp(ui, server)
