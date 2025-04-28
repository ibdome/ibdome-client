box::use(
  shiny[...],
  shiny.semantic[...],
  .. / components / page_template[page_template]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    div(class = "ui hidden divider", style="height: 20px;"),
    h1("IBDome explorer", class = "ui center aligned header"),
    div(class = "ui hidden divider", style="height: 40px;"),
    div(
      class = "ui center aligned container",
      style = "max-width: 90ch; font-size: 25px; text-align: justify;",
      p(HTML("The IBDome-explorer is a comprehensive resource developed as part of the <a href='https://www.transregio241.de/' target='_blank' style='color: darkblue; text-decoration: underline;'>TRR241 research initiative</a>. 
            It integrates large-scale omic datasets from patients with Inflammatory Bowel Diseases (IBD), such as Crohn’s disease and ulcerative colitis, along with detailed clinical information. TRR241, funded by the German 
            Research Foundation (DFG), brings together leading research institutions from Erlangen, Berlin, Kiel, and Innsbruck to enhance our understanding of the molecular and cellular mechanisms behind IBD.
            The database contains a wide array of data, including endoscopy-, histopathology- and stool-scores, protein abundances from the serum, whole exome- and RNA-sequencing data along  with high-resolution H&E images. 
            By combining these diverse data types, IBDome offers a unique platform for researchers to explore disease mechanisms and foster advancements in IBD diagnosis and treatment strategies.")
      )
    ),
    div(class = "ui hidden divider", style="height: 50px;"),
    div(
      style = "display: flex; justify-content: center; align-items: center; gap: 5vw; flex-wrap: wrap; width:100%; margin:0 auto;",
      img(src = "figure_overview.png", alt= "Overview figure", style= "max-width: 100%; height: auto;")
    ),
    div(class = "ui hidden divider", style="height: 50px;"),
    
    div(
      style = "display: flex; justify-content: center; align-items: center; gap: 5vw; flex-wrap: wrap; width:100%; margin:0 auto;",
      tags$a(
        href = "https://www.i-med.ac.at/mypoint/", 
        target = "_blank",
        img(src = "MUI_Logo_rz_en_ibk_rgb.png", alt = "Logo of the Medical University of Innsbruck", style = "max-width: 100%; height: auto; max-height: 80px;")
      ),
      
      tags$a(
        href = "https://www.charite.de/",  
        target = "_blank",
        img(src = "Logo_Charite.svg", alt = "Logo of the Charite Berlin", style = "max-width: 100%; height: auto; max-height: 40px;")
      ),
      
      tags$a(
        href = "https://www.fau.de/", 
        target = "_blank",
        img(src = "fau-digital.svg", alt = "Logo of the Friedrich-Alexander-Universität Erlangen-Nürnberg", style = "max-width: 100%; height: auto; max-height: 30px;")
      ),
      
      tags$a(
        href = "https://www.drfz.de/", 
        target = "_blank",
        img(src = "drfz-2.png", alt = "Logo of the DRFZ", style = "max-width: 100%; height: auto; max-height: 50px;")
      ),
      
      tags$a(
        href = "https://www.uksh.de/",
        target = "_blank",
        img(src = "Logo_UKSH.svg.png", alt = "Logo of the UKSH", style = "max-width: 100%; height: auto; max-height: 50px;")
      )
    )
  )
}


#' @export
server <- function(input, output, session) {
  # empty
}
