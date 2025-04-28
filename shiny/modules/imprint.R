box::use(
  shiny[...],
  shiny.semantic[...],
  .. / components / page_template[page_template],
  shiny.router[route, route_link]
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
      h1("Imprint", style = "margin-left: 200px; margin-right: 200px;"),
      h2("Owner", style = "margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider"),
      p(HTML("Institute of Bioinformatics, Biocenter, Medical University of Innsbruck<br>
        Innrain 80, 6020 Innsbruck, Austria, <a href='http://icbi.at' target='_blank' style='color: darkblue; text-decoration: underline;'>http://icbi.at</a><br>
        Zlatko Trajanoski (Head), email: icbi@i-med.ac.at, Tel: +43-512-9003-71400"), 
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      h2("Content", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("Institute of Bioinformatics, Biocenter, Medical University of Innsbruck<br>
        Innrain 80, 6020 Innsbruck, Austria, <a href='http://icbi.at' target='_blank' style='color: darkblue; text-decoration: underline;'>http://icbi.at</a><br>
        TRR241 (Content), email:icbi@i-med.ac.at, Tel: +43-512-9003-71400<br>
        Raphael Gronauer (Developer), email: raphael.gronauer@i-med.ac.at"),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      h2("Liability", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("The text provided on these websites has been reviewed with great care. However, we cannot guarantee the accuracy, completeness or validity of the information provided. 
        Therefore the Institute of Bioinformatics, Medical University of Innsbruck accepts no liability for the contents provided. Links to other websites have been carefully selected. However, 
        the Institute of Bioinformatics, Medical University of Innsbruck is not responsible for contents on any other websites.
        The software is provided ‘as is’ without warranty of any kind, either express or implied, including, but not limited to, the warranties of merchantability, fitness for a particular purpose, and non-infringement. 
        In no event shall the authors or copyright holders be liable for any claims, damages, or other liability, whether in contract, tort, or otherwise, arising out of the software or the use or other dealings in the software."),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      h2("Licence", style = "margin-left: 200px; margin-right: 200px;"),
      h3 ('Copyright (c) 2024 Institute of Bioinformatics, Medical University of Innsbruck', style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("This webside is provided under CC BY licence.<br>This license enables reusers to distribute, remix, adapt, and build upon the material in any medium or format, so long as attribution is given to the creator. The license allows for commercial use. <br>CC BY includes the following elements:<br>
        BY: credit must be given to the creator."),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      h2("Disclosure according to §25 Media Act", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("Media owner:<br>
        Institute of Bioinformatics, Biocenter, Medical University of Innsbruck<br>
        Medical University of Innsbruck<br>
        Innrain 80/82 <br>
        A - 6020 Innsbruck<br><br>
        Data Protection Officer of the Medical University of Innsbruck:<br>
        Mr Georg Fellner LL.M. Lawyer at bkp (Brauneis Klauser Prändl Rechtsanwälte GmbH), Vienna<br>
        E-mail: datenschutzbeauftragter@i-med.ac.at<br><br>
        To the <a href='",route_link("privacy"), "'  style='color: darkblue; text-decoration: underline;'>privacy policy</a>.<br>
        To the declaration of <a href='",route_link("accessibility"), "'  style='color: darkblue; text-decoration: underline;'>barrier-free access</a> to the website."),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      h2("Funding", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("This work was funded by the <a href='https://www.fwf.ac.at/' target='_blank' style='color: darkblue; text-decoration: underline;'>Austrian Science Fund</a> (FWF) [grant nr. I3978], 
             the <a href='https://www.dfg.de/' target='_blank' style='color: darkblue; text-decoration: underline;'>Deutsche Forschungsgemeinschaft</a> (DFG) [grant nr. 375876048] "),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;")
      )
  )
}

#' @export
server <- function(input, output, session) {
  # empty
}

