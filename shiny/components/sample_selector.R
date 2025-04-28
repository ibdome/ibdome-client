box::use(
  shiny[
    h1, NS, div, uiOutput, renderUI, renderPlot,
    plotOutput, selectizeInput, updateSelectizeInput, strong,
    textOutput, renderText, h3, tags
  ],
  shiny.semantic[...],
  .. / config[DB],
  .. / entities[ENTITIES, MISSING_VALUE],
  dplyr[filter, tbl, inner_join, collect],
  rlang[expr, ensym],
  stringr[str_replace_all]
)

#' @export
sample_selector_ui <- function(ns) {
  div(
    class = "ui tab active",
    div(class = "ui sub header", "disease"),
    dropdown_input(
      ns("ss_select_disease"),
      choices = ENTITIES$diseases,
      value = c("Crohn's disease", "non-IBD", "Ulcerative colitis"),
      type = "selection multiple fluid"
    ),
    div(class = 'ui sub header', 'Sex'),
    dropdown_input(
      ns("ss_select_sex"),
      choices = ENTITIES$sex,
      value = ENTITIES$sex,
      type = "selection multiple fluid",
    ),
    div(class = "ui sub header", "tissue"),
    dropdown_input(
      ns("ss_select_tissue"),
      choices = ENTITIES$tissues,
      value = c('colon','ileum'),
      type = "selection multiple fluid",
    ),
    div(class = "ui sub header", "Inflammation Status"),
    dropdown_input(
      ns("ss_select_inflammation"),
      choices = ENTITIES$inflammation_status,
      value = ENTITIES$inflammation_status,
      type = "selection multiple fluid",
    ),
    div(class = "ui sub header", "Sampling procedure"),
     dropdown_input(
       ns("ss_select_sampling"),
       choices = ENTITIES$sampling_procedure,
       value = ENTITIES$sampling_procedure,
       type = "selection multiple fluid"
    ),
    div(class = "ui hidden divider"),
    div(class = "ui small header", "Filter for Crohn's disease samples only"),
    div(
      class = "ui grid",
      div(
        class = "twelve wide column",
        div(class = "ui sub header", "Localization L1-L3"),
        dropdown_input(
          ns("ss_localization_cd"),
          choices = ENTITIES$localizations_cd,
          value = ENTITIES$localizations_cd,
          type = "selection multiple fluid",
        ),
      ),
      div(
        class = "four wide column",
        div(class = "ui sub header", "L4 modifier"),
        dropdown_input(
          ns("ss_localization_cd_l4"),
          choices = c("yes", "no", "any"),
          value = "any",
          type = "selection compact fluid",
        ),
      )
    ),
    div(
      class = "ui grid",
      div(
        class = "twelve wide column",
        div(class = "ui sub header", "disease course B1-B3"),
        dropdown_input(
          ns("ss_disease_course"),
          choices = ENTITIES$disease_courses,
          value = ENTITIES$disease_courses,
          type = "selection multiple fluid",
        ),
      ),
      div(
        class = "four wide column",
        div(class = "ui sub header", "p modifier"),
        dropdown_input(
          ns("ss_disease_course_p"),
          choices = c("yes", "no", "any"),
          value = "any",
          type = "selection compact fluid",
        ),
      )
    ),
    #
    div(class = "ui hidden divider"),
    div(class = "ui small header", "Filter for ulcerative colitis samples only"),
    div(class = "ui sub header", "Localization E1-E3"),
    dropdown_input(
      ns("ss_localization_uc"),
      choices = ENTITIES$localizations_uc,
      value = ENTITIES$localizations_uc,
      type = "selection multiple fluid",
    ),
  )
}

#' Generate a dbplyr query that includes NULL depending on
#' if the [missing] placeholder is included in the list of selected values.
condition_or_null <- function(form_input, column) {
  # it appears there is a bug in foomatic UI that replaces single quotes
  # in form values by their corresponding hex value, but only if it is
  # not the last value in the list. This workaround replaces the hex character
  # back to ensure "Crohn's disease" is working as expected.
  form_input <- str_replace_all(form_input, "&#x27;", "'")
  if (MISSING_VALUE %in% form_input) {
    expr(!!ensym(column) %in% local(!!form_input) | is.na(!!ensym(column)))
  } else {
    expr(!!ensym(column) %in% local(!!form_input))
  }
}



#' @export
get_samples <- function(input) {
  selected_tissues <- c(input$ss_select_tissue, 'serum','whole blood','other','[NA]')
  query <- tbl(DB,"samples_subjects_tissues_with_inflammation") |>
    filter(
      !!condition_or_null(input$ss_select_disease, disease),
      !!condition_or_null(input$ss_select_sex, sex),
      !!condition_or_null(input$ss_select_inflammation, inflammation_status),
      !!condition_or_null(input$ss_select_sampling, sampling_procedure),
      !!condition_or_null(selected_tissues, tissue_coarse),
      !!condition_or_null(input$ss_localization_uc, localization_uc) | disease != "Ulcerative colitis",
      !!condition_or_null(input$ss_localization_cd, localization_cd) | disease != "Crohn's disease",
      !!condition_or_null(input$ss_disease_course, disease_course) | disease != "Crohn's disease"
    )
  if (input$ss_localization_cd_l4 != "any") {
    query <- query |>
      filter(localization_cd_l4 == local(input$ss_localization_cd_l4 == "yes") | disease != "Crohn's disease")
  }
  if (input$ss_disease_course_p != "any") {
    query <- query |>
      filter(disease_course_p == local(input$ss_disease_course_p == "yes") | disease != "Crohn's disease")
  }
  
  query
}
