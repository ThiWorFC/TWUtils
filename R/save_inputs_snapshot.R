#' Save input snapshot to Excel
#'
#' @param inputs_tibble Tibble from capture_shiny_inputs()
#' @param filepath Path where to save the Excel file
#' @param add_metadata Logical, whether to add a metadata sheet
#' @param app_name Name of the application (for metadata)
#'
#' @returns Something
#' @export
#'
#' @examples NULL
save_inputs_snapshot <- function(inputs_tibble,
                                 filepath,
                                 add_metadata = TRUE,
                                 app_name = "Shiny App") {

  output_list <- list(Inputs = inputs_tibble)

  if (add_metadata) {
    output_list$Metadata <- tibble::tibble(
      Item = c("App Name", "Snapshot Time", "R Version",
               "User", "Platform", "Number of Inputs"),
      Value = c(
        app_name,
        as.character(inputs_tibble$timestamp[1]),
        paste(R.version$major, R.version$minor, sep = "."),
        Sys.info()["user"],
        Sys.info()["sysname"],
        as.character(nrow(inputs_tibble))
      )
    )
  }

  writexl::write_xlsx(output_list, filepath)
}
