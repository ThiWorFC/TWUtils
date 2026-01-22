utils::globalVariables(c("Display", "Count", "Attribute", "Min"))

#' Imports and Decode EyeQuestion Format Excel Sheets
#'
#' This function imports and decodes EQ files to be directly used in R or in Shiny Apps.
#'
#' @param file_path The path to the file to import
#' @param product Boolean: should the Products be decoded?
#' @param prod_order NULL by default: provide the products' order in the factor
#' @param attribute Boolean: should the Attributes be decoded?
#' @param assessor Boolean: should the Assessors be decoded?
#' @param convert2num Boolean: converts variables to numerical (when Min/Max is not empty in the Attributes tab)
#'
#' @returns The re-coded dataset
#' @export
#'
#' @examples NULL
EQ_import <- function(file_path, product=TRUE, prod_order=NULL, attribute=TRUE, assessor=FALSE, convert2num=TRUE){

  # Import the dataset
  dataset <- readxl::read_excel(file_path, sheet="Data", n_max=100000) %>%
    as.data.frame()

  # Import the product names
  if (product){
    product <- readxl::read_excel(file_path, sheet="Products") %>%
      as.data.frame()
    if (is.null(prod_order)){
      prod_order <- as.character(product[,1])
      prod_lab <- as.character(product$Name)
    } else {
      prod_lab <- product %>%
        tibble::column_to_rownames(var="Product")
      prod_lab <- as.character(prod_lab[prod_order, "Name"])
    }

    dataset[,2] <- factor(dataset[,2], levels=prod_order, labels=prod_lab)
  } else {
    dataset[,2] <- factor(dataset[,2])
  }

  # Import the assessor names
  if (assessor){
    assessor <- readxl::read_excel(file_path, sheet="Assessors") %>%
      as.data.frame()
    dataset[,1] <- factor(dataset[,1], levels=as.character(assessor[,1]), labels=as.character(assessor$Name))
  } else {
    dataset[,1] <- factor(dataset[,1])
  }

  # Import the attribute names
  if (attribute){
    attribute <- readxl::read_excel(file_path, sheet="Attributes") %>%
      dplyr::group_by(Display) %>%
      dplyr::mutate(Count = dplyr::row_number()) %>%
      dplyr::ungroup() %>%
      dplyr::relocate(Count, .after=Display)
    attr_dup <- attribute %>%
      dplyr::filter(Count > 1) %>%
      dplyr::pull(Display) %>%
      unique
    attribute <- attribute %>%
      dplyr::mutate(Display = ifelse(Display %in% attr_dup, stringr::str_c(Display, Count), Display)) %>%
      dplyr::select(-Count) %>%
      dplyr::mutate(Display = stringr::str_replace_all(Display, "[\\/]", " "))

    attr_name <- tibble::tibble(Attribute = colnames(dataset)) %>%
      dplyr::left_join(attribute %>% dplyr::select(Attribute, Display), by="Attribute") %>%
      dplyr::mutate(Display = ifelse(is.na(Display), Attribute, Display)) %>%
      dplyr::pull(Display)

    colnames(dataset) <- attr_name

    # Convert to Numeric
    if (convert2num){
      attr_num <- attribute %>%
        dplyr::filter(!is.na(Min)) %>%
        dplyr::pull(Display)
      dataset <- dataset %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(attr_num), as.numeric))
    }
  }

  return(tibble::as_tibble(dataset))
}
