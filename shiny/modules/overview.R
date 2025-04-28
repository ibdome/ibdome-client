box::use(
  shiny[...],
  dplyr[...],
  .. / config[DB],
  .. / entities[COLORS],
  .. / components / page_template[page_template],
  ggplot2[...],
  plotly[ggplotly, plotlyOutput, renderPlotly],
  cowplot[theme_cowplot, background_grid],
  tidyr[pivot_longer],
  forcats[fct_reorder]
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    h1("Data Overview"),
    p("Samples and clinical variables available in the IBDome database."),
    
    div(
      style = "display: flex; flex-direction: row; flex-wrap: wrap; gap: 2rem;",
      div(style = "flex: 1; min-width: 300px;",
          h2("Samples by sample type and tissue"),
          plotlyOutput(ns("sample_overview1"), height = 400)
      ),
      div(style = "flex: 1; min-width: 300px;",
          h2("Samples by sample type and disease"),
          plotlyOutput(ns("sample_overview2"), height = 400)
      )
    ),
    
    div(class = "ui hidden divider"),
    
    div(
      style = "display: flex; flex-direction:row; flex-wrap: wrap; gap: 2rem;",
      div(style = "flex: 1; min-width: 300px;",
          h2("Samples by sample type and study center"),
          plotlyOutput(ns("sample_overview3"), height = 400)
      ),
      div(style = "flex: 1; min-width: 300px;",
          h2("Samples by sample type and sex"),
          plotlyOutput(ns("sample_overview4"), height = 400)
      )
    ),
    
    div(class = "ui hidden divider"),
    
    h2("Patient overview"),
    p("Note that not all clinical variables are available for all subjects"),
    plotOutput(ns("clinical_overview1"), height = 450, width = "95%")
  )
}

#' @export
server <- function(input, output, session) {
  sample_summary <- tbl(DB,"samples_subjects_tissues") |>
      select(sample_id, sample_type, tissue = tissue_coarse, dataset, disease, sex) |>
      mutate(sample_type = case_when(sample_type == "Olink" ~ "protein abundance (Olink)",
                                     TRUE ~ sample_type),
             tissue = coalesce(tissue, "other")) |>
      collect()
  
  max_samples <- max(sample_summary |> count(sample_type) |> pull(n))

  patients_all <- tbl(DB, "subjects") |>
    select(-c("symptom_onset", "first_diagnosis", "steroids_disease_onset", "orig_id", "birth_year", "disease_course_p")) |>
    collect() 
  
  total_patients <- nrow(patients_all)
  
  clinical_summary <- patients_all |>
    mutate(
      female = as.integer(sex == "female"),
      male = as.integer(sex == "male"),
      `localization CD L1: terminal ileum` = as.integer(localization_cd == 'L1: terminal ileum'),
      `localization CD L2: colon` = as.integer(localization_cd == 'L2: colon'),
      `localization CD L3: ileocolon` = as.integer(localization_cd == 'L3: ileocolon'),
      `localization UC E1: proctitis` = as.integer(localization_uc == 'E1: proctitis'),
      `localization UC E2: left sided colitis` = as.integer(localization_uc == 'E2: left sided colitis'),
      `localization UC E3: extensive colitis` = as.integer(localization_uc == 'E3: extensive colitis'),
      `disease course B1: nonstricturing nonpenetrating` = as.integer(disease_course == 'B1: nonstricturing nonpenetrating'),
      `disease course B2: stricturing` = as.integer(disease_course == 'B2: stricturing'),
      `disease course B3: penetrating` = as.integer(disease_course == 'B3: penetrating'),
      `Crohn's disease` = as.integer(disease == "Crohn's disease"),
      `Ulcerative colitis` = as.integer(disease == "Ulcerative colitis"),
      `Indeterminate colitis` = as.integer(disease == "Indeterminate colitis"),
      Berlin = as.integer(dataset == "ibdome_berlin"),
      Erlangen = as.integer(dataset == "ibdome_erlangen"),
      `riskfactor pedigree: 1st degree` = as.integer(riskfactor_pedigree == "1st degree")) |>
    select(-c("subject_id", "sex", "disease_course", "disease", "localization_uc", "localization_cd", "dataset", "riskfactor_pedigree")) |>
    summarise(across(where(is.numeric), ~sum(.x, na.rm=TRUE))) |>
    pivot_longer(cols = everything(), names_to = "field", values_to = "n") |>
    mutate(
      field = recode(field, 
                     "total_patients" = "total patients",
                     "dataset" = "study centers",
                     "preexisting_diabetes_mellitus" = "preexisting diabetes mellitus",
                     "preexisting_arterial_hypertension" = "preexisting arterial hypertension",
                     "preexisting_coronary_heart_disease" = "preexisting coronary heart disease",
                     "preexisting_osteoporosis" = "preexisting osteoporosis",
                     "preexisting_depression" = "preexisting depression",
                     "preexisting_autoimmune_disease" = "preexisting autoimmune disease", 
                     "preexisting_malignant_tumor" = "preexisting malignant tumor", 
                     "riskfactor_nicotine" = "riskfactor nicotine",
                     "riskfactor_birth_control_pill" = "riskfactor birth_control pill",
                     "localization_cd_l4" = "localization CD L4")) |>
    add_row(field= "total patients", n=total_patients) |>
    arrange(n) |>
    mutate(field = factor(field, levels = field))
  

  output$sample_overview1 <- renderPlotly({
    p <- sample_summary |>
      group_by(sample_type, tissue) |>
      count() |>
      ungroup() |>
      group_by(sample_type) |>
      mutate(total_n = sum(n)) |>
      ungroup() |>
      mutate(sample_type = fct_reorder(sample_type, total_n, .desc=FALSE)) |>
      ggplot(aes(x = n, y = sample_type, fill = tissue, text = paste("n:", n, "<br>tissue:", tissue))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = COLORS$tissue) +
      theme_cowplot() +
      scale_x_continuous(limits = c(0, max_samples), expand=c(0,0)) +
      theme(legend.position = "right",plot.margin = margin(10, 10, 10, 10)) +
      coord_cartesian(clip = "off") +    
      ylab("")
    
    ggplotly(p, tooltip="text") 
  })

  
  output$sample_overview2 <- renderPlotly({
    p <- sample_summary |>
      group_by(sample_type, disease) |>
      count() |>
      ungroup() |>
      group_by(sample_type) |>
      mutate(total_n = sum(n)) |>
      ungroup() |>
      mutate(sample_type = fct_reorder(sample_type, total_n, .desc=FALSE)) |>
      ggplot(aes(x = n, y = sample_type, fill = disease, text = paste("n:", n, "<br>disease:", disease))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = COLORS$disease) +
      theme_cowplot() +
      scale_x_continuous(limits = c(0, max_samples), expand=c(0,0)) +
      theme(
        legend.position = "right", plot.margin = margin(10, 10, 10, 10)) +
      coord_cartesian(clip = "off") +    
      ylab("")
    
    ggplotly(p, tooltip="text") 
  })

  output$sample_overview3 <- renderPlotly({
    p <- sample_summary |>
      group_by(sample_type, dataset) |>
      count() |>
      ungroup() |>
      group_by(sample_type) |>
      mutate(total_n = sum(n)) |>
      ungroup() |>
      mutate(sample_type = fct_reorder(sample_type, total_n, .desc=FALSE)) |>
      ggplot(aes(x = n, y = sample_type, fill = dataset, text = paste("n: ", n, "<br>dataset:", dataset))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = COLORS$datasets) +
      theme_cowplot() +
      scale_x_continuous(limits = c(0, max_samples), expand=c(0,0)) +
      theme(legend.position = "right", plot.margin = margin(10, 10, 10, 10)) +
      coord_cartesian(clip = "off") +    
      ylab("")
    
    ggplotly(p, tooltip="text") 
  })
  
  output$sample_overview4 <- renderPlotly({
    p <- sample_summary |>
      group_by(sample_type, sex) |>
      count() |>
      ungroup() |>
      group_by(sample_type) |>
      mutate(total_n = sum(n)) |>
      ungroup() |>
      mutate(sample_type = fct_reorder(sample_type, total_n, .desc=FALSE)) |>
      ggplot(aes(x = n, y = sample_type, fill = sex, text = paste("n:", n, "<br>sex:", sex))) + 
      geom_bar(stat = "identity") +
      scale_fill_manual(values = COLORS$sex) +
      theme_cowplot() +
      scale_x_continuous(limits = c(0, max_samples), expand=c(0,0)) +
      theme(legend.position = "right", plot.margin = margin(10, 10, 10, 10))+  
      coord_cartesian(clip = "off") +    
      ylab("")
    
    ggplotly(p, tooltip="text")
  })

  
  output$clinical_overview1 <- renderPlot({
    clinical_summary |>
      ggplot(aes(x = n, y = field)) +
      geom_bar(stat = "identity", aes(fill = field == "total_patients")) +
      theme_cowplot() +
      geom_label(aes(label = n)) +
      background_grid() +
      ylab("") +
      scale_fill_brewer(type = "seq", palette = "Blues") +
      theme(legend.position = "none")
  }, width="auto", height=450)

}