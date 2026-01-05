#' Extract Diet Quality Score (DQS) Data from an eNutri Export
#'
#' @param enuti_source_file an absolute file path of a `.xlsx` workbook
#' @return a `tbl_df` of eNutri diet quality score data
#'
#' @export

extract_dqs_scores <- function(enutri_source_file)
{
  if (tools::file_ext(enutri_source_file) != 'xlsx') {
    stop('eNutri source file must be .xlsx')
  }

  sheet_names <- readxl::excel_sheets(enutri_source_file)

  if (!'DQS' %in% sheet_names) {
    stop('No `DQS` sheet found')
  }


  dqs_scores <- readxl::read_xlsx(enutri_source_file, sheet = 'DQS') %>%
    dplyr::rename(pid = `Participant Code`, ffqdid = 'FFQid')


  return(dqs_scores)

}
