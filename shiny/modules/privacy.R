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
      h1("Privacy policy", style = "margin-left: 200px; margin-right: 200px;"),
      h2("General information", style = "margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider"),
      p(HTML("This statement describes how the ICBI of the Medical University of Innsbruck processes your data in connection with this website. When handling personal data, we observe all data protection regulations as amended from time to time.
      The ICBI of the Medical University of Innsbruck undertakes to comply with the legal data protection regulations, but is not responsible if third parties - despite all security measures - nevertheless succeed in gaining access to data and information in an unlawful manner.
      Please note that complete protection of data against access by unauthorised third parties is not possible. We take all necessary and proportionate technical and organisational measures in accordance with Art. 
      32 GDPR to ensure an appropriate level of protection. The measures are regularly reviewed and updated and adapted to the current state of the art. Despite these measures, we cannot guarantee that individual systems and services 
      will always be available without restriction or interruption."), 
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider"),
      h2("Purpose of the processing of personal data", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("Your personal data will only be processed for the following purposes within the meaning of Art. 6 (1) GDPR:<br>
      To make the website available<br>
      To respond to enquiries<br>
      To be able to create usage statistics<br>
      For order processing<br>
      Furthermore, your personal data will only be stored for as long as is necessary to fulfil the respective purpose or within the scope of statutory retention periods.
      If consent is required for the lawful processing of your data, we will obtain your express consent prior to processing."),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider"),
      h2("Data processed by this website:", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("Browser type, version and language setting<br>
      Model designation of the end device and a generic device identifier<br>
      Operating system used<br>
      Website from which the user visits the website of the Medical University of Innsbruck (referrer URL)<br>
      Website visited by the user<br>
      Date and time of access<br>
      Anonymised IP address of the user's computer"),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider"),
      h2("Changes to the privacy policy", style = "margin-left: 200px; margin-right: 200px;"),
      p(HTML("We reserve the right to amend the data protection declaration in order to adapt it to changed legal situations or in the event of changes to the service and data processing. Users are therefore requested to inform themselves regularly about its content.
      We would like to point out that the use of this website is subject to copyright, name and trademark rights as well as other third-party rights. You undertake to refrain from misusing the entire content (in particular images, videos, fonts and trademarks).
      You may choose to prevent this website from aggregating and analysing the actions you take here. Doing so will protect your privacy, but will also prevent the owner from learning from your actions and creating a better experience for you and other users."),
        style = "font-size: 14px ; margin-left: 200px; margin-right: 200px;"),
      div(class = "ui hidden divider")
      )    
  )
}

#' @export
server <- function(input, output, session) {
  # empty
}
