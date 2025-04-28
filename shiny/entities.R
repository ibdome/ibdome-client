box::use(
  . / config[DB],
  dplyr[...]
)


#' @export
MISSING_VALUE <- "[NA]"

#
# Get static entries from db (only load once)
#
get_unique_entries <- function(table, column) {
  unique_entries <- table |>
    select(!!column) |>
    distinct() |>
    collect() |>
    pull(!!column)

  unique_entries[is.na(unique_entries)] <- MISSING_VALUE

  sort(unique_entries)
}


get_unique_entries_1 <- function(table, column) {
  unique_entries <- table |>
    select(!!column) |>
    distinct() |>
    collect() |>
    pull(!!column)
  
  sort(unique_entries)
}

#' @export
#'
#' unique values for different variables (used, e.g. for select menus)
ENTITIES <- list(
  genes = get_unique_entries(tbl(DB, "genes"), "hgnc_symbol"),
  diseases = get_unique_entries_1(tbl(DB, "subjects"), "disease"),
  tissues = get_unique_entries(tbl(DB, "samples_subjects_tissues_with_inflammation"), "tissue_coarse") %>%
    # Filter the tissues to exclude those that match unwanted patterns
    .[!grepl(paste(c('serum','whole blood','other','[NA]'), collapse = "|"), .)],
  localizations_uc = get_unique_entries(tbl(DB, "subjects"), "localization_uc"),
  localizations_cd = get_unique_entries(tbl(DB, "subjects"), "localization_cd"),
  disease_courses = get_unique_entries(tbl(DB, "subjects"), "disease_course"),
  assay = get_unique_entries(tbl(DB, "data_olink"), "Assay"),
  sex = get_unique_entries(tbl(DB, "subjects"), "sex"),
  inflammation_status = get_unique_entries(tbl(DB,"samples_subjects_tissues_with_inflammation"), "inflammation_status"),
  sampling_procedure = get_unique_entries(tbl(DB, "samples_subjects_tissues_with_inflammation"), "sampling_procedure")
)


na_color <- "#999999"

#' @export
COLORS <- list(
  disease = c(
    "Crohn's disease" = "#1b9e77",
    "Ulcerative colitis" = "#d95f02",
    "Indeterminate colitis" = "#7570b3",
    "non-IBD" = "#e7298a",
    "NA" = na_color
  ),
  
  tissue = c(
    "colon" = "#E69F00",
    "ileum" = "#56B4E9",
    "small intestine" = "#009E73",
    "caecum" = "#F0E442",
    "ileocecal valve" = "#0072B2",
    "rectum" = "#D55E00",
    "whole blood" = "#CC79A7",
    "anastomosis" = "#B3DE68",
    "serum" = "#ffcfdb",
    "pouch" = "#94714f",
    "NA" = na_color
  ),
  
  sex = c(
    "male" = "#8da0cb",
    "female" = "#fc8d62",
    "NA" = na_color
  ),
  
  
  inflammation_status = c(
    "inflamed" = "#BF5B17", 
    "non_inflamed" = "#386CB0",
    "NA" = na_color
    
  ),
  sampling_procedure = c(
    "biopsy" = "coral4",
    "resection"= "cornsilk3",
    "NA" = na_color
  ),
  datasets = c(
    "ibdome_berlin" = "#ffb26699",
    "ibdome_erlangen" = "#ff666699",
    "NA" = na_color
  )
  
)

#' @export
VARIABLES <- list(
  "clinical data" = c("median_bmi", "median_pms", "median_hbi"),
  "endoscopy" = c("SES_CD", "UCEIS"),
  "histopathology" = c("normalized_naini_cortina_score", "normalized_riley_score")
)
# VARIABLE_INFO <- list(
#   "modified_naini_cortina_score" = "The score for Crohn's disease has been developed in cooperation
#                     with Prof. Loddenkemper and is based on the Score by Naini and Cortina (2012).
#                     The score ranges from 0-10 (biopsy) or 0-14 (resection) in Ileum,
#                     and from 0-16 (biopsy) or 0-20 (resection) in Colon, respectively."
# )
