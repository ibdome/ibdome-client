box::use(
  data.table[fread],
  tibble[column_to_rownames],
  ./config[TPM_VERSION],
)

#Load data tables with the start up of the app
#' @export
tpm_matrix = fread(TPM_VERSION, nThread=4) |>
  column_to_rownames('sample_id')
