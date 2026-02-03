#' @importFrom shiny moduleServer renderText downloadHandler req tagList htmlOutput downloadButton NS br
NULL


#' Capture all Shiny input values automatically
#'
#' This function extracts all input names and values from a Shiny session
#' and returns them in a tidy tibble format. Useful for bug reports,
#' reproducible research, and audit trails.
#'
#' @param session Shiny session object
#' @param exclude Character vector of input names to exclude
#'
#' @return A tibble with columns: input_name, input_value, value_class, timestamp
#'
#' @export
#' @examples NULL
capture_shiny_inputs <- function(session, exclude = NULL) {

  # Get all inputs as a list
  all_inputs <- shiny::reactiveValuesToList(session$input)

  # Remove excluded inputs
  if (!is.null(exclude)) {
    all_inputs <- all_inputs[!names(all_inputs) %in% exclude]
  }

  # Convert to tibble
  inputs_df <- tibble::tibble(
    input_name = names(all_inputs),
    input_value = vapply(all_inputs, function(x) {
      if (is.null(x)) {
        return(NA_character_)
      } else if (length(x) == 0) {
        return(NA_character_)
      } else if (length(x) > 1) {
        return(paste(x, collapse = "; "))
      } else {
        return(as.character(x))
      }
    }, character(1)),
    value_class = vapply(all_inputs, function(x) {
      class(x)[1]
    }, character(1)),
    timestamp = Sys.time()
  )

  return(inputs_df)
}
