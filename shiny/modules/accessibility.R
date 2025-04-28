box::use(
  shiny[...],
  shiny.semantic[...],
  .. / components / page_template[page_template],
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
      h1("Declaration on accessibility", style = "margin-left: 200px; margin-right: 200px;"),
      h2("General information", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("We endeavour to make our websites accessible in accordance with 
      <a href = 'https://www.ris.bka.gv.at/GeltendeFassung.wxe?Abfrage=Bundesnormen&Gesetzesnummer=20010727' target='_blank' style='color: darkblue; text-decoration: underline;'>The Web Accessibility Directive (WAD)</a>
      as amended to implement 
      <a href = 'https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=celex:32016L2102' target='_blank' style='color: darkblue; text-decoration: underline;'>Directive (EU) 2016/2102 of the European Parliament and of the Council of 26 October 2016 on 
      the accessibility of the websites and mobile applications of public sector bodies (OJ L 327, 2.12.2016, p. 1)</a>.<br><br>
      This accessibility statement applies to the website <a href = 'https://www.ibdome.org' tyle='color: darkblue; text-decoration: underline;'>https://www.ibdome.org'</a><br><br>
      Status of compliance with the requirements:<br><br>
      This website is partially compliant with conformance level AA of the 
      <a href = 'https://www.w3.org/TR/WCAG21/' target='_blank' style='color: darkblue; text-decoration: underline;'>Web Content Accessibility Guidelines Web - WCAG 2.1</a> or with the applicable 
      <a href = 'https://www.etsi.org/deliver/etsi_en/301500_301599/301549/02.01.02_60/en_301549v020102p.pdf' target='_blank' style='color: darkblue; text-decoration: underline;'>European Standard EN 301 549 V2.1.2 (2018-08)</a>
      due to the following incompatibilities and exceptions.<br><br><br>
      <strong>Non-accessible content:</strong><br><br>
      Images and photos:<br>
      Decorative images and photos do not have captions or alternative <br>
      text and are therefore not accessible for screen reader users*.<br><br>
      Interactive plots and diagrams:<br>
      Plots and diagrams do not have alternative texts or text-based <br>
      descriptions, making them difficult to use by screen readers or text-based systems.<br><br>
      Input screens:<br>
      Due to technical limitations, the input masks are not optimally <br>
      adaptable for use with a keyboard or screen reader.<br><br>
      Drop-down menus:<br>
      Extensive drop-down menus impair usability, especially for users <br>
      with motor impairments or when navigating with a screen reader.<br><br>
      We are working to identify these barriers and develop <br>
      solutions to make it more accessible for all users. <br><br>
      Our long-term goal is to make interaction with the tool as barrier-free as possible.<br>
      If you have any feedback or specific suggestions for improvement, please do not hesitate to contact us.<br><br>
      Despite our best efforts, users may experience problems. Please contact us if you notice a problem that is not listed.<br><br><br>
      Creation of this accessibility statement<br><br>
      This statement was created on 25.11.2024."),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;")
      )
  )
}

#' @export
server <- function(input, output, session) {
  # empty
}

