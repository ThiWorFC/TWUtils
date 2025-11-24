#' Imports and Decode Fizz Format Excel Sheets
#'
#' This function imports and decodes Fizz files to be directly used in R or in Shiny Apps.
#'
#' @param file_path The path to the file to import
#'
#' @returns The file restructured and decoded
#' @export
#'
#' @examples NULL
Fizz_import <- function(file_path){

  data <- readxl::excel_sheets(file_path) %>%
    purrr::map(function(tab){
      data <- readxl::read_xlsx(file_path, sheet=tab, skip=1) %>%
        dplyr::select(Judge=NO, Session=NS, Replica=NR, 8:ncol(.)) %>%
        dplyr::select(-where(is.character)) %>%
        tidyr::pivot_longer(-c(1:3), names_to="Product", values_to="Score") %>%
        dplyr::mutate(Attribute = tab, Sequence = NA, Code=NA) %>%
        dplyr::select(Judge, Product, Session, Replica, Sequence, Code, Attribute, Score)
    }) %>%
    purrr::reduce(dplyr::bind_rows) %>%
    dplyr::mutate(Judge = as.character(Judge), Attribute = forcats::fct_inorder(Attribute)) %>%
    tidyr::pivot_wider(names_from=Attribute, values_from=Score)

  return(data)

}
