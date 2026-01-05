#' Extract Food Frequency Data from an eNutri Export
#'
#' Extract, clean and compile all frequency related data from a eNutri workbook export. This functions combines frequency data that is exported as three different sheets
#' within a workbook and collates data into a single `tbl_df` with the correct variable types
#'
#' @param enuti_source_file an absolute file path of a `.xlsx` workbook
#' @return a `tbl_df` of all eNutri frequency data
#'
#' @export
#'
#' @examples
#' \dontrun{
#' enutri_export <- 'eNutri-export-ffq-01012026.xlsx'
#'
#' enutri_freq_data <- extract_food_frequencies(enutri_export)
#'
#' }


extract_food_frequencies <- function(enutri_source_file)
{

  if(tools::file_ext(enutri_source_file) != 'xlsx'){
  stop('eNutri source file must be .xlsx')
  }

  sheet_names <- readxl::excel_sheets(enutri_source_file)

  if(!'Frequency Freqday' %in% sheet_names){
    stop('No `Frequency Freqday` sheet found')
  }


  freq_main <- readxl::read_xlsx(enutri_source_file, sheet = 'Frequency Freqday')

  ffqid_names <- freq_main %>%
    dplyr::slice(1) %>%
    unlist(., use.names=FALSE) %>%
    na.omit() %>%
    as.vector()


  names(freq_main)[4:ncol(freq_main)] <- ffqid_names

  enutri_ffq <- freq_main[-1, ]

  eNutri_ffq_clean <-
    enutri_ffq %>%
    dplyr::mutate(dplyr::across(Pizza:`Pre-mixed alcoholic drinks`, as.numeric)) %>%
    dplyr::mutate(dplyr::across(Pizza:`Pre-mixed alcoholic drinks`, ~ {
      tidyr::replace_na(.x, 0)
    })) %>%
    dplyr::rename(pid = 'Participant Code')




  if(!'FreqCheckbox Freqday' %in% sheet_names){
    stop('No `FreqCheckbox Freqday` sheet found')
  }


  freq_checkbox <- readxl::read_xlsx(enutri_source_file, sheet = 'FreqCheckbox Freqday')

  ffqid_names2 <- freq_checkbox %>%
    dplyr::slice(1) %>%
    unlist(., use.names=FALSE) %>%
    na.omit() %>%
    as.vector()


  names(freq_checkbox)[4:ncol(freq_checkbox)] <- ffqid_names2

  enutri_checkbox <- freq_checkbox[-1, ]

  enutri_checkbox_clean <-
    enutri_checkbox %>%
    dplyr::mutate(dplyr::across(`Porridge & overnight oats/bircher`:`Beer, lager, cider, stout & ale`, as.numeric)) %>%
    dplyr::mutate(dplyr::across(`Porridge & overnight oats/bircher`:`Beer, lager, cider, stout & ale`, ~ {
      tidyr::replace_na(.x, 0)
    })) %>%
    dplyr::rename(pid = 'Participant Code')




  if(!'FreqCheckboxAdd Freqday' %in% sheet_names){
    stop('No `FreqCheckboxAdd Freqday` sheet found')
  }

  freq_checkbox_add <- readxl::read_xlsx(enutri_source_file, sheet = 'FreqCheckboxAdd Freqday')


  ffqid_names3 <- freq_checkbox_add %>%
    dplyr::slice(1) %>%
    unlist(., use.names=FALSE) %>%
    na.omit() %>%
    as.vector()



  names(freq_checkbox_add)[4:ncol(freq_checkbox_add)] <- ffqid_names3

  enutri_checkbox_add <- freq_checkbox_add[-1, ]

  enutri_checkbox_add_clean <-
    enutri_checkbox_add %>%
    dplyr::mutate(dplyr::across(`Muesli & granola`:`Hot chocolate, malt drinks (e.g. Ovaltine/Horlicks) & instant cappuccino/latte/mocha`, as.numeric)) %>%
    dplyr::mutate(dplyr::across(`Muesli & granola`:`Hot chocolate, malt drinks (e.g. Ovaltine/Horlicks) & instant cappuccino/latte/mocha`, ~ {
      tidyr::replace_na(.x, 0)
    })) %>%
    dplyr::rename(pid = 'Participant Code')



  compiled_ffq <- eNutri_ffq_clean %>% dplyr::left_join(., enutri_checkbox_clean, by = c('userid', 'pid', 'ffqid')) %>%
    dplyr::left_join(., enutri_checkbox_add_clean, by = c('userid', 'pid', 'ffqid'))




  return(compiled_ffq)






}
