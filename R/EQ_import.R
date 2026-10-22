utils::globalVariables(c("Display", "Count", "Attribute", "Min", "Datatype"))

# =============================================================================
# EyeQuestion (EQ) import: three functions, one transformation
#
#   EQ_decode()         the decoding itself, working on data frames
#   EQ_import()         reads an EQ-format .xlsx, then calls EQ_decode()
#   EQ_import_tables()  takes the tables straight from the EQ API, then calls the SAME EQ_decode()
#
# Both entry points end in EQ_decode(), so the file route and the API route cannot drift apart.
# =============================================================================


#' Decode EyeQuestion Tables
#'
#' Turns the coded EyeQuestion (EQ) tables into a dataset that is ready to use in R or in Shiny apps:
#' product and assessor codes become factors (optionally labelled with their names), and the question
#' codes (`Q1`, `Q2`, ...) are renamed to the attribute names. This is the transformation behind
#' [EQ_import()] (tables read from an `.xlsx` file) and [EQ_import_tables()] (tables from the API);
#' use it directly when the tables are already data frames in memory.
#'
#' @details
#' The `dataset` must follow the EQ layout: **column 1 = assessor (Judge)**, **column 2 = product**, the
#' other columns are the questions. What is done:
#'
#' * **Products**: column 2 becomes a factor. If `products` is given, the levels are the product codes
#'   (in the order of `prod_order`, or of the table) and the labels are the product names (`Name`).
#' * **Assessors**: column 1 becomes a factor; if `assessors` is given, it is labelled with the assessor
#'   names (`Name`).
#' * **Attributes**: the question codes in the column names are replaced by the `Display` names of the
#'   `attributes` table. Display names that occur more than once get their occurrence number appended
#'   (`Salt1`, `Salt2`), `/` and `\` are replaced by a space, and a column without an entry in the table
#'   keeps its code.
#' * **Numbers**: with `convert2num = TRUE`, the attributes whose `Datatype` is `scale`, `ordinal`,
#'   `interval` or `binary` are converted to numeric.
#'
#' @param dataset The Data table (data frame or tibble). Column 1 = assessor, column 2 = product.
#' @param products The Products table (columns `Product` and `Name`), or `NULL` to leave the product codes
#'   undecoded (column 2 is still turned into a factor).
#' @param assessors The Assessors table (first column the assessor code, plus `Name`), or `NULL` to leave
#'   the assessor codes undecoded (column 1 is still turned into a factor).
#' @param attributes The Attributes table (columns `Attribute`, `Display` and `Datatype`), or `NULL` to
#'   keep the question codes as column names and skip the numeric conversion.
#' @param prod_order `NULL` by default (order of the Products table); otherwise a vector of product codes
#'   giving the order of the factor levels.
#' @param convert2num Boolean: convert the attributes to numeric according to their `Datatype`
#'   (see Details). Ignored when `attributes` is `NULL`.
#'
#' @returns The decoded dataset, as a tibble.
#' @export
#'
#' @seealso [EQ_import()] to read an EQ `.xlsx` file, [EQ_import_tables()] for the tables of the EQ API.
#'
#' @examples
#' dataset <- data.frame(Judge = c("J01", "J01", "J02", "J02"),
#'                       Product = c("P01", "P02", "P01", "P02"),
#'                       Q3 = c("5", "7", "6", "8"))
#' products <- data.frame(Product = c("P01", "P02"), Name = c("Cheese A", "Cheese B"))
#' attributes <- data.frame(Attribute = "Q3", Display = "Firmness", Datatype = "ordinal")
#'
#' EQ_decode(dataset, products = products, attributes = attributes)
#'
#' # Keep the product codes, change the order of the levels
#' EQ_decode(dataset, attributes = attributes, prod_order = c("P02", "P01"))
EQ_decode <- function(dataset, products=NULL, assessors=NULL, attributes=NULL,
                      prod_order=NULL, convert2num=TRUE){

  dataset <- as.data.frame(dataset)

  # Decode the product names
  if (!is.null(products)){
    products <- as.data.frame(products)
    if (is.null(prod_order)){
      prod_order <- as.character(products[,1])
      prod_lab <- as.character(products$Name)
    } else {
      prod_lab <- products %>%
        tibble::column_to_rownames(var="Product")
      prod_lab <- as.character(prod_lab[prod_order, "Name"])
    }

    dataset[,2] <- factor(dataset[,2], levels=prod_order, labels=prod_lab)
  } else {
    dataset[,2] <- factor(dataset[,2])
  }

  # Decode the assessor names
  if (!is.null(assessors)){
    assessors <- as.data.frame(assessors)
    dataset[,1] <- factor(dataset[,1], levels=as.character(assessors[,1]), labels=as.character(assessors$Name))
  } else {
    dataset[,1] <- factor(dataset[,1])
  }

  # Decode the attribute names
  if (!is.null(attributes)){
    attribute <- attributes %>%
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
        # dplyr::filter(!is.na(Min)) %>%
        dplyr::filter(Datatype %in% c("scale","ordinal","interval","binary")) %>%
        dplyr::pull(Display)
      dataset <- dataset %>%
        dplyr::mutate(dplyr::across(tidyselect::all_of(attr_num), as.numeric))
    }
  }

  return(tibble::as_tibble(dataset))
}


#' Import and Decode EyeQuestion Format Excel Sheets
#'
#' Imports an EyeQuestion (EQ) export (`.xlsx`) and decodes it, so that it can be used directly in R or in
#' Shiny apps. The sheets `Data`, `Products`, `Assessors` and `Attributes` are read as needed and passed to
#' [EQ_decode()], which describes the transformation in detail.
#'
#' @details
#' Only the sheets that are decoded are read: `Products` when `product = TRUE`, `Assessors` when
#' `assessor = TRUE` and `Attributes` when `attribute = TRUE`. The `Data` sheet is always read (at most
#' 100,000 rows). Column 1 of `Data` must be the assessor and column 2 the product.
#'
#' @param file_path The path to the `.xlsx` file to import.
#' @param product Boolean: should the Products be decoded (product codes replaced by product names)?
#' @param prod_order `NULL` by default (order of the Products sheet); otherwise a vector of product codes
#'   giving the order of the factor levels.
#' @param attribute Boolean: should the Attributes be decoded (question codes replaced by attribute names)?
#' @param assessor Boolean: should the Assessors be decoded (assessor codes replaced by assessor names)?
#' @param convert2num Boolean: convert the attributes to numeric according to their `Datatype` in the
#'   Attributes sheet (`scale`, `ordinal`, `interval` and `binary`). Only used when `attribute = TRUE`.
#'
#' @returns The decoded dataset, as a tibble.
#' @export
#'
#' @seealso [EQ_decode()] for the transformation, [EQ_import_tables()] when the tables come from the EQ API
#'   instead of a file.
#'
#' @examples
#' if (requireNamespace("writexl", quietly = TRUE)) {
#'   sheets <- list(
#'     Data = data.frame(Judge = c("J01", "J01", "J02", "J02"),
#'                       Product = c("P01", "P02", "P01", "P02"),
#'                       Q3 = c(5, 7, 6, 8)),
#'     Products = data.frame(Product = c("P01", "P02"), Name = c("Cheese A", "Cheese B")),
#'     Attributes = data.frame(Attribute = "Q3", Display = "Firmness", Datatype = "ordinal"))
#'   file <- tempfile(fileext = ".xlsx")
#'   writexl::write_xlsx(sheets, file)
#'
#'   EQ_import(file)
#'   EQ_import(file, product = FALSE, attribute = FALSE)  # keep the codes
#' }
EQ_import <- function(file_path, product=TRUE, prod_order=NULL, attribute=TRUE, assessor=FALSE, convert2num=TRUE){

  # Import the dataset
  dataset <- readxl::read_excel(file_path, sheet="Data", n_max=100000) %>%
    as.data.frame()

  # Import the product / assessor / attribute sheets (only the ones that are decoded)
  products <- if (product) readxl::read_excel(file_path, sheet="Products") %>% as.data.frame()
  assessors <- if (assessor) readxl::read_excel(file_path, sheet="Assessors") %>% as.data.frame()
  attributes <- if (attribute) readxl::read_excel(file_path, sheet="Attributes")

  EQ_decode(dataset, products=products, assessors=assessors, attributes=attributes,
            prod_order=prod_order, convert2num=convert2num)
}


# Makes an in-memory table look exactly like the same table read back from an .xlsx with
# readxl::read_excel(): empty strings become NA, surrounding whitespace is trimmed, a column that is
# entirely empty becomes logical NA, and duplicate / blank column names are repaired. Numeric columns
# are untouched (read_excel returns them as numeric too).
#' @noRd
.eq_as_sheet <- function(df){
  df <- as.data.frame(df, stringsAsFactors=FALSE)
  names(df) <- trimws(names(df))
  df[] <- lapply(df, function(x){
    if (is.factor(x)) x <- as.character(x)
    if (is.character(x)){
      x <- trimws(x)
      x[!nzchar(x)] <- NA_character_
      if (all(is.na(x))) x <- as.logical(x)
    }
    x
  })
  as.data.frame(tibble::as_tibble(df, .name_repair="unique_quiet"))
}


#' Import and Decode EyeQuestion Data Straight from the API Tables
#'
#' Same transformation as [EQ_import()], but the input is the list of tables that an EyeQuestion (EQ) API
#' client produces, instead of an `.xlsx` file. The result is identical to writing those tables to `.xlsx`
#' and reading them back with [EQ_import()]: the tables are first made to look like freshly read sheets
#' (empty strings become `NA`, whitespace is trimmed, empty columns become logical `NA`, column names are
#' repaired), then decoded with [EQ_decode()].
#'
#' @param tables Named list of data frames with the sheet `Data`, plus `Products`, `Assessors` and
#'   `Attributes` when those are decoded. A list that holds these tables under `$tables` (as returned by
#'   some API clients) is accepted as well.
#' @inheritParams EQ_import
#' @param n_max Maximum number of `Data` rows used (the file route is capped at 100,000 rows too).
#'
#' @returns The decoded dataset, as a tibble.
#' @export
#'
#' @seealso [EQ_import()] for `.xlsx` files, [EQ_decode()] for the transformation.
#'
#' @examples
#' tables <- list(
#'   Data = data.frame(Judge = c("J01", "J01", "J02", "J02"),
#'                     Product = c("P01", "P02", "P01", "P02"),
#'                     Q3 = c(5, 7, 6, 8)),
#'   Products = data.frame(Product = c("P01", "P02"), Name = c("Cheese A", "Cheese B")),
#'   Attributes = data.frame(Attribute = "Q3", Display = "Firmness", Datatype = "ordinal"))
#'
#' EQ_import_tables(tables)
#' EQ_import_tables(tables, product = FALSE, attribute = FALSE)  # keep the codes
EQ_import_tables <- function(tables, product=TRUE, prod_order=NULL, attribute=TRUE, assessor=FALSE,
                             convert2num=TRUE, n_max=100000){

  if (!"Data" %in% names(tables) && is.list(tables$tables)) tables <- tables$tables

  need <- c("Data", if (product) "Products", if (assessor) "Assessors", if (attribute) "Attributes")
  missing_sheets <- setdiff(need, names(tables))
  if (length(missing_sheets))
    stop("Table(s) missing from the input: ", paste(missing_sheets, collapse=", "), call.=FALSE)
  if (is.null(tables$Data) || ncol(tables$Data) < 2 || nrow(tables$Data) == 0)
    stop("The Data table is empty -- the EQ project returned no response rows.", call.=FALSE)

  dataset    <- utils::head(.eq_as_sheet(tables$Data), n_max)
  products   <- if (product)   .eq_as_sheet(tables$Products)
  assessors  <- if (assessor)  .eq_as_sheet(tables$Assessors)
  attributes <- if (attribute) tibble::as_tibble(.eq_as_sheet(tables$Attributes))

  EQ_decode(dataset, products=products, assessors=assessors, attributes=attributes,
            prod_order=prod_order, convert2num=convert2num)
}
