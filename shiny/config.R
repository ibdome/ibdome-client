box::use(yaml[read_yaml], utils[modifyList])

#' @export
CONFIG <- modifyList(read_yaml("config.yml"), read_yaml("config_external.yml"))

#' @export
VERSION <- CONFIG$version

#' @export
DB <- DBI::dbConnect(
  RSQLite::SQLite(),
  file.path(CONFIG$db_path, CONFIG$db_version),
  flags = RSQLite::SQLITE_RO
)

#' @export
TPM_VERSION <- file.path(CONFIG$db_path, CONFIG$tpm_version)

#' @export
DOWNLOADS <- CONFIG$downloads
