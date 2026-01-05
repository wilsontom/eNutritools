#' Extract Nutrient Data from an eNutri Export
#'
#'
#'
#' @param enuti_source_file an absolute file path of a `.xlsx` workbook
#'
#' @param output a character of one of the following three options;
#'  * `abs`; returns all available nutrient data as absolute values (ie, `g`, `mg`, `ug` etc...)
#'  * `te`; returns all nutrient data that is available as a percentage of total energy intake
#'  * `extra`; returns all additional data that is not a specific nutrient, ie `Greenhouse gas emissions (kg CO2 eqv)` and `Acidification (g SO2 emissions)`
#' @return a `tbl_df` of eNutri nutrient data
#'
#' @export


extract_nutrients <- function(enutri_source_file, output)
  {


  if(tools::file_ext(enutri_source_file) != 'xlsx'){
    stop('eNutri source file must be .xlsx')
  }


  if(!output %in% c('abs', 'te', 'extra')){
    stop('`output` must be one of; `abs`, `te` or `extra`')
  }



  sheet_names <- readxl::excel_sheets(enutri_source_file)

  if(!'Nutrients WITH Supplements' %in% sheet_names){
    stop('No Nutrients sheet found')
  }


  nutrient_names <- names(readxl::read_xlsx(enutri_source_file, sheet = 'Nutrients WITH Supplements', range = 'A1:CN1'))

  nutrients <- suppressMessages(readxl::read_xlsx(enutri_source_file, sheet = 'Nutrients WITH Supplements')) %>%
    dplyr::select(!!nutrient_names) %>%
    dplyr::rename(ffqid = 'FFQid', pid = `Participant Code`) %>%
    dplyr::select(-question_number_int, -gDay)


  te_nut_idx <- names(nutrients)[which(stringr::str_detect(names(nutrients), '%TE') == 'TRUE')]

  nutrient_as_te <- nutrients %>% dplyr::select(userid, pid, ffqid, !!te_nut_idx)



  nutrient_extra_names <- c('Acidification (g SO2 emissions)', 'Blue water (L-check units)','Cost _Budget items (£)',
                       'Cost_Branded items (£)', 'Cost_Supermarket premium & organic items (£)',
                       'Cost_Supermarket standard items (£)', 'Cost_average (£)',
                       'Eutrophication (g N emissions)', 'Green water (L-check units)',
                       'Greenhouse gas emissions (kg CO2 eqv)', 'Grey water (L-check units)',
                       'Land use (m2)', 'Total water (L-check units)')




  nutrient_extras <- nutrients %>% dplyr::select(userid, pid, ffqid, !!nutrient_extra_names)


  nutrients_absolute <- nutrients %>% dplyr::select(-!!te_nut_idx, -!!nutrient_extra_names)



  if(output == 'abs'){
    return(nutrients_absolute)
  }

  if(output == 'te'){
    return(nutrient_as_te)
  }

  if(output == 'extra'){
    return(nutrient_extras)
  }

  }
