#' Recreate original data file(s) exactly as uploaded
#'
#' @param file_inputs Single file input or list of file inputs from Shiny
#' @param output_dir Directory where to save the recreated files
#'
#' @return Character vector of created file paths
#' @export
#'
#' @examples NULL
recreate_data_files <- function(file_inputs, output_dir = tempdir()) {

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  created_files <- c()

  # Handle single file or list of files
  files_list <- if (is.list(file_inputs) && !is.null(file_inputs$datapath)) {
    # Single file upload object
    list(file_inputs)
  } else if (is.list(file_inputs)) {
    # List of file upload objects
    file_inputs
  } else {
    # Unknown format
    return(NULL)
  }

  for (i in seq_along(files_list)) {
    file_obj <- files_list[[i]]

    # Skip NULL or missing files
    if (is.null(file_obj) || is.null(file_obj$datapath)) next
    if (!file.exists(file_obj$datapath)) next

    # Get original filename
    original_name <- if (!is.null(file_obj$name)) {
      file_obj$name
    } else {
      basename(file_obj$datapath)
    }

    # Determine file type and handle accordingly
    file_ext <- tolower(tools::file_ext(original_name))

    if (file_ext %in% c("xlsx", "xls", "xlsm")) {
      # Excel file - recreate with all sheets
      output_path <- file.path(output_dir, original_name)

      tryCatch({
        sheets <- readxl::excel_sheets(file_obj$datapath)
        excel_data <- list()

        for (sheet in sheets) {
          excel_data[[sheet]] <- readxl::read_xlsx(file_obj$datapath, sheet = sheet)
        }

        writexl::write_xlsx(excel_data, output_path)
        created_files <- c(created_files, output_path)

      }, error = function(e) {
        warning(sprintf("Could not recreate Excel file %s: %s", original_name, e$message))
      })

    } else {
      # Other file types - just copy as-is
      output_path <- file.path(output_dir, original_name)
      file.copy(file_obj$datapath, output_path, overwrite = TRUE)
      created_files <- c(created_files, output_path)
    }
  }

  return(created_files)
}
