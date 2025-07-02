box::use(
  shiny[...],
  shiny.semantic[...],
  dplyr[...],
  .. / config[DB, CONFIG, VERSION],
  .. / entities[COLORS, ENTITIES, VARIABLES],
  ggplot2[...],
  ggpubr[...],
  cowplot[theme_cowplot, background_grid, ggsave2],
  tidyr[pivot_longer],
  ggbeeswarm[geom_quasirandom],
  .. / components / sample_selector[sample_selector_ui, get_samples],
  .. / components / page_template[page_template],
  R.devices[suppressGraphics],
  tibble[rownames_to_column],
  .. / load_data[tpm_matrix],
  rlang[...],
  readr[write_csv]
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    div(
      h1("Gene expression"),
      div(class = "ui horizontal divider", "select samples"),
      sample_selector_ui(ns),
      div(class = "ui hidden divider"),
      div(class = "ui horizontal divider", "select variables"),
      div(
        class = "ui grid",
        div(
          class = "eight wide column",
          div(class = "ui sub header", "Select genes"),
          selectizeInput(
            ns("select_genes"),
            label = NULL,
            choices = NULL,
            selected = c("IFNG", "IL6"), ### add default genes
            multiple = TRUE,
            width = "100%"
          ),
        ),
        div(
          class = "eight wide column",
          div(class = "ui sub header", "plot by this categorical variable"),
          dropdown_input(
            ns("select_variable"),
            choices = c("disease", "tissue", "inflammation_status", "sampling_procedure"),
            value = "disease",
            type = "selection fluid"
          ),
        )
      ),
      div(class = "ui hidden divider"),
      div(class = "ui horizontal divider", "results"),
      message_box(
        class = "info",
        header = "Statistics",
        content = c(
          "Errorbars denote the 95% confidence interval of the mean",
          "The p-values have been calculated using a simple Wilcoxon-Mann-Whitney test which does not
      take into account potential confounding factors.",
          "P-values are not adjusted for multiple testing. "
        )
      ),
      textOutput(ns("n_samples")),
      uiOutput(ns("plot1_ui")),
      downloadButton(ns("download_plot"), "plot", icon = icon("download")),
      downloadButton(ns("download_data"), "data", icon = icon("download")),
      downloadButton(ns("download_meta"), "meta", icon = icon("download"))
    )
  )
}

#' @export
server <- function(input, output, session) {
  
  updateSelectizeInput(
    session,
    "select_genes",
    choices = ENTITIES$genes,
    selected = c("IFNG", "IL6"),
    server = TRUE
  )
  

  process_samples_and_data <- function(input) {
    req(input$select_genes) ## "pause" reactive until a value exists

    samples <- get_samples(input)  %>%
      collect()
    
    data <- tpm_matrix %>%
      select(all_of(input$select_genes)) %>%
      filter(rownames(.) %in% samples$sample_id) %>%
      rownames_to_column(var = "sample_id") %>%
      pivot_longer(cols = all_of(input$select_genes), values_to = "log10(TPM+1)", names_to = 'hgnc_symbol')  %>%
      inner_join(samples, by = 'sample_id') %>%
      select(tissue = tissue_coarse, `log10(TPM+1)`, hgnc_symbol, sample_id, disease, inflammation_status, sampling_procedure) 
    
    return(data)
  }
  
  
  rnaseq_data <- reactive({
    data <- process_samples_and_data(input)
  })

  plot1 <- reactive({
    
     # Ensure rnaseq_data is called as a function when used
      data <- rnaseq_data()
      
      if (input$select_variable == "disease") {
        custom_levels <- if ('non-IBD' %in% input$ss_select_disease) {
          c(setdiff(unique(data[[input$select_variable]]), 'non-IBD'), 'non-IBD')
        } else {
          unique(data[[input$select_variable]])
        }
      } else {
        custom_levels <- unique(data[[input$select_variable]])
      }
      
      p <- data |>
        mutate(!!input$select_variable := factor(!!sym(input$select_variable), levels = custom_levels)) |>
        ggplot(
          aes_string(
            x = input$select_variable,
            y = "`log10(TPM+1)`",
            color = input$select_variable
          )
        ) +
        geom_quasirandom() +
        facet_wrap(~hgnc_symbol, ncol = 2) +
        theme_cowplot() +
        background_grid() +
        scale_color_manual(values = COLORS[[input$select_variable]]) +
        geom_pwc(aes_string(group = input$select_variable), tip.length = 0,
                 method = "t_test", label = "p.format")
      
      add_summary(
        p,
        "mean_ci",
        color = "black",
        error.plot = "errorbar",
        width = .2,
        size = 1.2
      )
    }
   )

  plot_dimensions <- reactive({
    list(
      height = max(ceiling(length(input$select_genes) / 2) * 300, 300),
      width = ifelse(length(input$select_genes) <= 1, 600, 1000)
    )
  })

  output$n_samples <- renderText({
    n_samples <- rnaseq_data()$sample_id |>
      unique() |>
      length()
    sprintf("%i samples selected", n_samples)
  })

  output$plot1_ui <- renderUI({
    plotOutput(
      session$ns("plot1"),
      height = plot_dimensions()$height, width = plot_dimensions()$width
    )
  })
  
  selected_parameters <- reactive({
    list(
      selected_diseases = input$ss_select_disease,
      selected_sex = input$ss_select_sex,
      selected_tissue = input$ss_select_tissue,
      selected_disease_course =  input$ss_disease_course,
      selected_localization_uc = input$ss_localization_uc,
      selected_localization_cd =  input$ss_localization_cd,
      selected_genes = input$select_genes
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
      "Selected Genes = ", paste(params$selected_genes, collapse = ", "), "\n",
      sep = ""
    )
  }

  output$download_plot <- downloadHandler(
    filename = function() {
      sprintf("gene_expression_%s.pdf", strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
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
      sprintf("Gene_Expression_%s.csv", strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S"))
    },
    content = function(file) {
      data <- rnaseq_data()
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

  output$plot1 <- renderPlot(plot1())
}
