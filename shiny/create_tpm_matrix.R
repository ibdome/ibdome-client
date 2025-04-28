library(dplyr)
library(DBI)
library(tidyr)
library(jsonlite)
library(readr)
library(yaml)

CONFIG <- modifyList(read_yaml("config.yml"), read_yaml("config_external.yml"))

DB <- DBI::dbConnect(
  RSQLite::SQLite(),
  file.path(CONFIG$db_path, CONFIG$db_version),
  flags = RSQLite::SQLITE_RO
)

data_RNAseq <- tbl(DB, "data_rnaseq") |>
  inner_join(tbl(DB, "genes"), by = c('gene_id'= 'ensg')) |>
  collect()

tpm_matrix_download <- data_RNAseq |>
  pivot_wider(id_cols = gene_id, names_from=sample_id, values_from = tpm)

count_matrix_download <- data_RNAseq |>
  pivot_wider(id_cols = gene_id, names_from=sample_id, values_from = raw_counts)


matrix_RNAseq <- data_RNAseq |>
  filter(!is.na(hgnc_symbol)) |>
  group_by(hgnc_symbol,sample_id) |>
  summarise(mean_tpm = mean(tpm)) |>
  mutate(mean_tpm = log10(mean_tpm + 1)) |>
  pivot_wider(names_from = hgnc_symbol, values_from = mean_tpm)

### Write outputs
db_dir <- "../../db"
dir.create(file.path(db_dir), showWarnings = FALSE)

write_csv(matrix_RNAseq, file = gzfile(file.path(db_dir, paste0('tpm_matrix_rnaseq_', format(Sys.Date(), "%Y-%m-%d") ,'.csv.gz'))))

downloads <- "../../downloads"
dir.create(file.path(downloads), showWarnings = FALSE)

tpm_matrix_download_file = file.path(downloads, paste0('IBDome_transcriptomics_tpm_', CONFIG$version, '.csv'))
write_csv(tpm_matrix_download, tpm_matrix_download_file)
utils::zip(zipfile = paste0(tpm_matrix_download_file, '.zip'), files = tpm_matrix_download_file, extras = '-j')
file.remove(tpm_matrix_download_file)


count_matrix_download_file = file.path(downloads, paste0('IBDome_transcriptomics_rawcounts_', CONFIG$version, '.csv'))
write_csv(count_matrix_download, count_matrix_download_file)
utils::zip(zipfile = paste0(count_matrix_download_file, '.zip'), files = count_matrix_download_file, extras = '-j')
file.remove(count_matrix_download_file)
