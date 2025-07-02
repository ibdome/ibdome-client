box::use(
  shiny[...],
  shiny.semantic[...],
  dplyr[...],
  .. / config[DB, CONFIG, VERSION],
  .. / entities[ENTITIES, VARIABLES],
  ggplot2[...],
  ggpubr[...],
  cowplot[theme_cowplot, background_grid, panel_border, ggsave2],
  tidyr[pivot_longer, drop_na],
  .. / components / page_template[page_template],
  .. / components / sample_selector[sample_selector_ui, get_samples],
  R.devices[suppressGraphics],
  tibble[rownames_to_column],
  .. / load_data[tpm_matrix],
  rlang[ensym],
  readr[write_csv]
  )


#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    div(
      h1("Correlation"),
      div(class = "ui horizontal divider", "select samples"),
      sample_selector_ui(ns),
      div(class = "ui hidden divider"),
      div(class = "ui horizontal divider", "select variables"),
    ),
    div(
      class = "ui basic segment",
      div(
        class = "ui two column very relaxed grid",
        div(
          class = "column",
          div(class = "ui sub header", "data type"),
          dropdown_input(
            ns("select_data_type_1"),
            choices = c("gene expression", "endoscopy", "histopathology", "clinical data","protein abundance"),
            value = c("gene expression"),
            type = "selection fluid"
          ),
          uiOutput(ns("select_variable_ui_1"))
        ),
        div(
          class = "column",
          div(class = "ui sub header", "data type"),
          dropdown_input(
            ns("select_data_type_2"),
            choices = c("gene expression", "endoscopy", "histopathology", "clinical data","protein abundance"),
            value = c("gene expression"),
            type = "selection fluid"
          ),
          uiOutput(ns("select_variable_ui_2"))
        ),
      ),
      div(class = "ui vertical divider", "vs")
    ),
    div(class = "ui hidden divider"),
    div(class = "ui horizontal divider", "results"),
    message_box(
      class = "info",
      header = "Please note",
      content = c(
        "Correlation is indicated as Pearson's R.",
        "Each point represents a patient, except in gene expression correlations where each point corresponds to an individual sample.",
        "The trend-line has been fitted using a linear model.",
        "The inflammation status was assessed by histopathology (score > 0 ~ inflamed).",
        "The shaded area denotes the 95% confidence interval of the slope of the linear model.",
        "Protein abundance data are represented as normalized protein expression (NPX) values, Olink Proteomics arbitrary unit on log2 scale"
      )
    ),
    textOutput(ns("n_samples")),
    uiOutput(ns("plot1_ui")),
    downloadButton(ns("download_plot"), "plot", icon = icon("download")),
    downloadButton(ns("download_data"), "data", icon = icon("download")),
    downloadButton(ns("download_meta"), "meta", icon = icon("download"))
  )
}

#' @export
server <- function(input, output, session) {

  # Update the "select variables panel dynamically based on the selection
  # of the data type
  {
    output$select_variable_ui_1 <- renderUI({
      if (input$select_data_type_1 == "gene expression") {
        div(
          style = "margin-top: 20px",
          div(class = "ui sub header", "genes"),
          selectizeInput(
            session$ns("select_variables_1"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      } else if (input$select_data_type_1=="protein abundance"){
        div(
          style = "margin-top: 20px",
          div(class = "ui sub header", "Olink Assay"),
          selectizeInput(
            session$ns("select_variables_1"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      }
      else {
        div(
          style = "margin-top: 20px",
          div(class = "ui sub header", "variable"),
          dropdown_input(
            session$ns("select_variables_1"),
            choices = VARIABLES[[input$select_data_type_1]],
            value  = VARIABLES[[input$select_data_type_1]][1],
            type = "selection fluid"
          )
        )
      }
    })
    
    output$select_variable_ui_2 <- renderUI({
      if (input$select_data_type_2 == "gene expression") {
        div(
          style = "margin-top: 20px",
          div(class = "ui sub header", "genes"),
          selectizeInput(
            session$ns("select_variables_2"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      } else if (input$select_data_type_2=="protein abundance"){
        div(
          style = "margin-top: 20px",
          div(class = "ui sub header", "Olink Assay"),
          selectizeInput(
            session$ns("select_variables_2"),
            label = NULL,
            choices = NULL,
            multiple = TRUE,
            width = "100%"
          )
        )
      } else {
        div(
          style = "margin-top: 20px",
          div(class = "ui sub header", "variable"),
          dropdown_input(
            session$ns("select_variables_2"),
            choices = VARIABLES[[input$select_data_type_2]],
            value = VARIABLES[[input$select_data_type_2]][1],
            type = "selection fluid"
          )
        )
      }
    })
    
    observeEvent(input$select_data_type_1, {
      switch(input$select_data_type_1,
             "gene expression" = {
               updateSelectizeInput(
                 session,
                 "select_variables_1",
                 choices = ENTITIES$genes,
                 selected = c("IFNG", "IL6"),
                 server = TRUE)
             },
             "protein abundance" = {
               updateSelectizeInput(
                 session,
                 "select_variables_1",
                 choices = ENTITIES$assay,
                 selected = "AXIN1",
                 server = TRUE)
               }
      )
      })
    observeEvent(input$select_data_type_2, {
      switch(input$select_data_type_2,
             "gene expression" = {
               updateSelectizeInput(
                session,
                "select_variables_2",
                choices = ENTITIES$genes,
                selected = c("OSM", "OSMR"),
                server = TRUE)
               },
             "protein abundance" = {
               updateSelectizeInput(
                 session,
                 "select_variables_2",
                 choices = ENTITIES$assay,
                 selected = "CD8A",
                 server = TRUE)
               }
             )
  })
  }

  # Update the size of the plotting area depending on the number of selected
  # input variables.
  plot_dimensions <- reactive({
    list(
      height = max(length(input$select_variables_2) * 300, 300),
      width = max(length(input$select_variables_1) * 300, 300)
    )
  })
  output$plot1_ui <- renderUI({
    plotOutput(
      session$ns("plot1"),
      height = plot_dimensions()$height, width = plot_dimensions()$width
    )
  })
  
  # return a data frame in the format subject_id, variable, value,
  # where variable is the variable name and value is the associated value.
  
  make_correlation_data <- function(data_type, variables) {
    
    req(data_type)
    req(variables)
    
    available_variables <- VARIABLES[[data_type]]
    
    # For gene expression and protein abundance, variables are selected via selectizeInput
    if (data_type %in% c("gene expression", "protein abundance")) {
      req(all(variables %in% ENTITIES[[ifelse(data_type == "gene expression", "genes", "assay")]]), cancelOutput = TRUE)
    } else {
      req(variables %in% available_variables, cancelOutput = TRUE)
    }
    
    query <- get_samples(input)
    
    message("VAR: ", variables)
    
    query <- switch(data_type,
                    "gene expression" = {
                      tpm_matrix |>
                        select(all_of(variables)) |>
                        filter(row.names(tpm_matrix) %in% (query |> pull(sample_id))) |>
                        rownames_to_column(var = "sample_id") |>
                        pivot_longer(cols = all_of(variables), values_to = "log10(TPM+1)", names_to = 'hgnc_symbol')  |>
                        inner_join(query |> collect(), by = 'sample_id') |>
                        select(all_of(c(input$select_variable,"log10(TPM+1)",'hgnc_symbol','sample_id','subject_id'))) |>
                        rename(value = `log10(TPM+1)`) |>
                        rename(variable = 'hgnc_symbol')
                    },
                    "clinical data" = {
                      subjects_summarized_timepoints <- tbl(DB, "subject_timepoints2") |> 
                        group_by(subject_id) |>
                        summarise(
                          median_bmi = median(bmi, na.rm = TRUE),
                          median_pms = median(pms_score_sum, na.rm = TRUE),
                          median_hbi = median(hbi_score_sum, na.rm = TRUE)
                        )
                      
                     query |>
                        inner_join(subjects_summarized_timepoints, by = "subject_id") |>
                        select(subject_id, value = !!variables) |>
                        mutate(variable = !!variables) |>
                        collect()
                    },
                    "endoscopy" = {
                      query |>
                        inner_join(tbl(DB, "data_endoscopy"), by = "sample_id") |>
                        group_by(subject_id) |>
                        summarise(value = median(!!ensym(variables), na.rm = TRUE)) |>
                        mutate(variable = !!variables) |>
                        collect()
                    },
                    "histopathology" = {
                      query |>
                        inner_join(tbl(DB, "data_histopathology2"), by = "sample_id") |>
                        group_by(subject_id) |>
                        summarise(value = median(!!ensym(variables), na.rm = TRUE)) |>
                        mutate(variable = !!variables)|>
                        collect()
                    },
                    "protein abundance" = {
                      query |>
                        inner_join(tbl(DB, "data_olink"), by = "sample_id") |>
                        filter(Assay %in% variables) |>
                        group_by(subject_id, Assay) |>
                        summarise(value = median(NPX, na.rm = TRUE)) |>
                        rename(variable = Assay) |>
                        collect()
                    }
    )
  }
  
  # a data frame with variables and values from both selectors.
  correlation_data <- reactive({

    req(input$select_data_type_1)
    req(input$select_data_type_2) 
    req(input$select_variables_1, length(input$select_variables_1) > 0)
    req(input$select_variables_2, length(input$select_variables_2) > 0)
    
    data1 <- make_correlation_data(input$select_data_type_1, input$select_variables_1) |>
          rename(value1 = value, variable1 = variable)
    data2 <- make_correlation_data(input$select_data_type_2, input$select_variables_2) |>
          rename(value2 = value, variable2 = variable) 
    
    if (input$select_data_type_1=='gene expression' && input$select_data_type_2 == 'gene expression'){
      inner_join(data1, data2, by="sample_id")
    }
    else {
      inner_join(data1, data2, by="subject_id") 
    }
  })
 
  output$n_samples <- try(renderText({
    req(input$select_data_type_1, input$select_data_type_2)
    
    id_col <- if (input$select_data_type_1 == "gene expression" &&
                  input$select_data_type_2 == "gene expression") {
      "sample_id"
    } else {
      "subject_id"
    }
    
    n_samples <- correlation_data() |>
      drop_na() |>
      pull(id_col) |>
      unique() |>
      length()
    
    sprintf("%i samples selected for which the selected data are available.", n_samples)
  }), silent = TRUE)
  
  
  axis_label <- function(data_type) {
    labels <- c(
      "gene expression" = "gene expression [log10(TPM+1)]",
      "protein abundance" = "protein abundance [NPX]",
      "endoscopy" = "endoscopy",
      "histopathology" = "histopathology",
      "clinical data" = "clinical"
    )
    labels[[data_type]] %||% data_type
  }
  
  plot1 <- reactive({
    req(correlation_data())
    tmp_data <- unique(correlation_data())
    
    tmp_data |>
      ggplot(aes(x = value1, y = value2)) +
      geom_point(alpha = .3) +
      facet_grid(rows = vars(variable2), cols = vars(variable1)) +
      stat_cor() +
      geom_smooth(method = "lm", color = "#0571b0", fullrange = FALSE) +
      theme_cowplot() +
      background_grid() +
      panel_border(color = "black") +
      xlab(axis_label(input$select_data_type_1)) +
      ylab(axis_label(input$select_data_type_2)) +
      coord_cartesian(ylim = c(min(tmp_data$value2), max(tmp_data$value2) + 0.5))
  })
  
  selected_parameters <- reactive({
    list(
      selected_diseases = input$ss_select_disease,  
      selected_sex = input$ss_select_sex,
      selected_tissue = input$ss_select_tissue,
      selected_disease_course =  input$ss_disease_course,
      selected_localization_uc = input$ss_localization_uc,
      selected_localization_cd =  input$ss_localization_cd,
      selected_data_type1 = input$select_data_type_1, 
      selected_data_type2 = input$select_data_type_2,
      selected_variable1 = input$select_variables_1,
      selected_variable2 = input$select_variables_2
    )
  })
  
  format_metadata <- function(params) {
    paste(
      "\nSample Selection Parameters:\n",
      "Selected Tissue = ", paste(params$selected_tissue, collapse = ", "), "\n",
      "Selected Sex = ", paste(params$selected_sex, collapse = ", "), "\n",
      "Selected Localization UC = ", paste(params$selected_localization_uc, collapse = ", "), "\n",
      "Selected Localization CD = ", paste(params$selected_localization_ud, collapse = ", "), "\n",
      "Selected Diseases = ", paste(params$selected_diseases, collapse = ", "), "\n",
      "Selected Diseases Course = ", paste(params$selected_disease_course, collapse = ", "), "\n",
      "\nSelected Plotting Parameter:\n",
      "Selected Datatype 1 = ", paste(params$selected_data_type1 , collapse = ", "), "\n",
      "Selected Variable 1 = ", paste(params$selected_variable1, collapse = ", "), "\n",
      "Selected Datatype 2 = ", paste(params$selected_data_type2, collapse = ", "), "\n",
      "Selected Variable 2 = ", paste(params$selected_variable2, collapse = ", "), "\n",
      sep = ""
    )
  }
  
  output$download_plot <- downloadHandler(
    filename = function() {
      sprintf("correlation_%s.pdf", strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
    },
    content = function(file) {
      suppressGraphics(
        ggsave2(file,
                plot = plot1(), device = "pdf",
                width = plot_dimensions()$width / 3,
                height = plot_dimensions()$height / 3,
                units = "mm", dpi = 300
        )
      )
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function() {
      sprintf("Correlation_%s.csv", strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
    },
    content = function(file) {
      data <- correlation_data()
      write_csv(data, file)
    }
  )
  output$download_meta <- downloadHandler(
    filename = function() {
      sprintf("IBDome_MetaData_%s.txt", strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
    },
    content = function(file) {
      params <- selected_parameters()
      metadata_content <- paste(
        sprintf("DB-Version: %s", CONFIG$db_version),
        sprintf("IBDome-App-Version: %s", VERSION),
        format_metadata(params),
        sep = "\n"
      )
      writeLines(metadata_content, file)
    }
  )
  
  try(output$plot1 <- renderPlot(plot1()),silent = T)
}
