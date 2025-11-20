EQ_import <- function(file_path, product=TRUE, prod_order=NULL, attribute=TRUE, assessor=FALSE){

  require(tidyverse)
  require(readxl)

  # Import the dataset
  dataset <- readxl::read_excel(file_path, sheet="Data") %>%
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
        column_to_rownames(var="Product")
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
      group_by(Display) %>%
      mutate(Count = row_number()) %>%
      ungroup() %>%
      relocate(Count, .after=Display)
    attr_dup <- attribute %>%
      filter(Count > 1) %>%
      pull(Display) %>%
      unique
    attribute <- attribute %>%
      mutate(Display = ifelse(Display %in% attr_dup, str_c(Display, Count), Display)) %>%
      dplyr::select(-Count) %>%
      mutate(Display = str_replace_all(Display, "[\\/]", " "))

    attr_name <- tibble(Attribute = colnames(dataset)) %>%
      left_join(attribute %>% dplyr::select(Attribute, Display), by="Attribute") %>%
      mutate(Display = ifelse(is.na(Display), Attribute, Display)) %>%
      pull(Display)

    colnames(dataset) <- attr_name
  }

  return(as_tibble(dataset))
}
