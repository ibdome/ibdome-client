box::use(
  shiny[...],
  shiny.semantic[...],
  dplyr[...],
  tidyr[pivot_wider],
  .. / config[DB, VERSION, DOWNLOADS],
  .. / components / page_template[page_template],
  readr[write_csv]
)

# Helper to get metadata
get_metadata <- function(sample_type_filter, extra_cols = character()) {
  tbl(DB, "samples_subjects_tissues_with_inflammation") |>
    select(
      sample_id, subject_id, date, sample_type, sampling_procedure, dataset, disease, sex, birth_year,
      first_diagnosis, symptom_onset, preexisting_diabetes_mellitus, preexisting_arterial_hypertension,
      preexisting_coronary_heart_disease, preexisting_osteoporosis, preexisting_depression,
      preexisting_autoimmune_disease, preexisting_malignant_tumor, riskfactor_pedigree, riskfactor_nicotine,
      riskfactor_birth_control_pill, steroids_disease_onset,
      any_of(extra_cols)
    ) |>
    filter(sample_type == sample_type_filter) |>
    collect()
}

# UI
#' @export
ui <- function(id) {
  ns <- NS(id)
  page_template(
    id,
    div(
      h1("IBDome Data Downloads"),
      p("Download the latest data related to the IBDome project, transcriptomic and proteomic data with the accompanying metadata"),
      h1("IBDome", VERSION),
      tableOutput(ns("download_table"))
    ),
    div(class = "ui hidden divider"),
    div(
      h1("Archive"),
      p("Download the legacy DB archives from former IBDome app versions"),
      tableOutput(ns("download_table_1"))
    )
  )
}

#' @export
server <- function(input, output, session) {
  ns <- session$ns  
  
  addResourcePath("static", DOWNLOADS)
  
  # Preload matrices and metadata
  matrix_olink <- tbl(DB, "data_olink") |>
    select(sample_id, Assay, NPX) |>
    pivot_wider(names_from = Assay, values_from = NPX) |>
    collect()
  
  olink_meta <- get_metadata("Olink")
  transcriptomics_meta <- get_metadata("RNA-seq", extra_cols = c("batch", "tissue"))
  
  # Download table definitions
  downloads <- list(
    list(label = "Olink Proteomics Data", desc = "Proteomic data generated using the Olink technology.", id = "download_proteomics"),
    list(label = "Transcriptomics Data (TPM)", desc = "Transcriptomic data in TPM format.", link = paste0("IBDome_transcriptomics_tpm_", VERSION, ".csv.zip")),
    list(label = "Transcriptomics Data (raw counts)", desc = "Raw transcriptomic counts data.", link = paste0("IBDome_transcriptomics_rawcounts_", VERSION, ".csv.zip")),
    list(label = "Olink Proteomics Metadata", desc = "Metadata associated with the Olink proteomics dataset.", id = "download_proteomics_meta"),
    list(label = "Transcriptomics Metadata", desc = "Metadata for the transcriptomic dataset.", id = "download_transcriptomics_meta"),
    list(label = "H&E images", desc = "Link to the BioImage Archive repository of IBDome", link = "https://www.ebi.ac.uk/biostudies/BioImages/studies/S-BIAD1753"),
    list(label = "IBDome Logo", desc = "Zip folder with the logo for the IBDome project in different formats.", link = "IBDome_logo.zip"),
    list(label = "IBDome Database", desc = "Zipped IBDome SQLite database.", link = "ibdome_v1.0.3.zip")
  )
  
  output$download_table <- renderTable({
    data.frame(
      File = sapply(downloads, `[[`, "label"),
      Description = sapply(downloads, `[[`, "desc"),
      Download = sapply(downloads, function(x) {
        if (!is.null(x$id)) {
          as.character(shiny::downloadButton(ns(x$id), "Download"))
        } else if (grepl("^https?://", x$link)) {
          # External link
          sprintf('<a href="%s" target="_blank">Download</a>', x$link)
        } else {
          # Local file
          sprintf('<a href="%s" target="_blank" download>Download</a>', file.path("static", x$link))
        }
      }),
      stringsAsFactors = FALSE
    )
  }, sanitize.text.function = identity)
  
  output$download_table_1 <- renderTable({
    data.frame(
      File = "IBDome version 0.5.0",
      Description = "IBDome database for app version 0.5.0",
      Download = sprintf('<a href="%s" target="_blank" download>Download</a>', file.path("static", "IBDome_v.0.5.0.zip")),
      stringsAsFactors = FALSE
    )
  }, sanitize.text.function = identity)
  
  # Helper to register download handlers
  register_download <- function(id, filename, content, content_type = "text/csv") {
    output[[id]] <- downloadHandler(
      filename = function() filename,
      content = content,
      contentType = content_type
    )
  }
  
  # Register download handlers
  register_download("download_proteomics", paste0("IBDome_olink_proteomics_", VERSION, ".csv"),
                    function(file) write_csv(matrix_olink, file))
  
  register_download("download_proteomics_meta", paste0("IBDome_olink_proteomics_metadata_", VERSION, ".csv"),
                    function(file) write_csv(olink_meta, file))
  
  register_download("download_transcriptomics_meta", paste0("IBDome_transcriptomics_metadata_", VERSION, ".csv"),
                    function(file) write_csv(transcriptomics_meta, file))
}

