#' Create dark-mode styled DT table
#'
#' This function is a wrapper to \code{DT} and datatable() to control for different parameters such as white text in cell (for dark mode), colour coding, superscript, etc.
#'
#' @param data Data frame to display
#' @param color_target Integer vector. Column indices to color
#' @param color_source Integer vector. Column indices containing color codes (same length as color_target)
#' @param color_values Vector of values that trigger colors (e.g., c(1,2,3))
#' @param color_palette Vector of colors (e.g., c("white","#FF0000","#F4777C"))
#' @param hide_cols Integer vector. Column indices to hide (optional, if different from color_source)
#' @param round_cols Integer vector of column indices to round
#' @param round_digits Integer vector of decimal places (same length as round_cols)
#' @param escape Logical. Set to FALSE if data contains HTML (e.g., superscripts)
#' @param auto_width Logical. If TRUE, columns adjust to content
#' @param scroll_x Logical. Enable horizontal scrolling
#' @param page_length Integer. Rows per page
#' @param ... Additional arguments passed to datatable()
#'
#' @return A datatable object
#'
#' @export
#' @examples NULL
dt_dark <- function(data,
                    color_target = NULL,
                    color_source = NULL,
                    color_values = NULL,
                    color_palette = NULL,
                    hide_cols = NULL,
                    round_cols = NULL,
                    round_digits = NULL,
                    escape = TRUE,
                    auto_width = TRUE,
                    scroll_x = FALSE,
                    page_length = 100,
                    ...) {

  n_cols <- ncol(data)

  # Handle position and names for columns
  col_to_index <- function(cols, data) {
    if (is.null(cols)) return(NULL)
    if (is.character(cols)) match(cols, colnames(data))
    else cols
  }

  color_target <- col_to_index(color_target, data)
  color_source <- col_to_index(color_source, data)
  hide_cols    <- col_to_index(hide_cols, data)
  round_cols   <- col_to_index(round_cols, data)

  # Validation checks
  if (!is.null(color_target) && !is.null(color_source)) {
    if (length(color_target) != length(color_source)) {
      stop("color_target and color_source must have the same length")
    }
  }

  if (!is.null(color_values) && !is.null(color_palette)) {
    if (length(color_values) != length(color_palette)) {
      stop("color_values and color_palette must have the same length")
    }
  }

  # If hide_cols not specified but color_source is, hide the color_source columns
  if (is.null(hide_cols) && !is.null(color_source)) {
    hide_cols <- color_source
  }

  # Build columnDefs for hiding
  col_defs <- list()
  if (!is.null(hide_cols)) {
    # Convert to 0-indexed for DataTables
    col_defs <- list(list(targets = hide_cols - 1, visible = FALSE))
  }

  # Add auto-width settings if needed
  if (auto_width) {
    col_defs <- c(col_defs, list(list(targets = "_all", width = NULL)))
  }

  # Create base datatable
  dt <- DT::datatable(
    data,
    rownames = FALSE,
    selection = "none",
    escape = escape,
    options = list(
      columnDefs = col_defs,
      pageLength = page_length,
      sDom = '<"top">t<"bottom">',
      autoWidth = auto_width,
      scrollX = scroll_x
    ),
    class = 'cell-border stripe',
    callback = DT::JS("
      // Style the header
      $('thead th').css({
        'background-color': '#0094D9',
        'color': '#ffffff',
        'font-weight': 'bold'
      });

      // Style the body cells for more breathing room
      $('tbody td').css({
        'padding': '12px 25px'
      });
    "),
    ...
  )

  # Determine which columns get white text (all except color_target)
  if (!is.null(color_target)) {
    white_cols <- dplyr::setdiff(1:n_cols, c(color_target, hide_cols))
  } else {
    white_cols <- dplyr::setdiff(1:n_cols, hide_cols)
  }

  # Apply white text to non-colored columns
  if (length(white_cols) > 0) {
    dt <- dt %>%
      DT::formatStyle(
        columns = white_cols,
        color = '#ffffff',
        backgroundColor = 'transparent'
      )
  }

  # Apply conditional coloring

  if (!is.null(color_target) && !is.null(color_source) &&
      !is.null(color_values) && !is.null(color_palette)) {

    for (i in seq_along(color_target)) {
      dt <- dt %>%
        DT::formatStyle(
          columns = color_target[i],
          valueColumns = color_source[i],
          color = DT::styleEqual(color_values, color_palette)
        )
    }
  }

  # Apply rounding
  if (!is.null(round_cols) && !is.null(round_digits)) {
    if (length(round_digits) == 1) {
      # Single value applies to all round_cols
      dt <- dt %>%
        DT::formatRound(round_cols, round_digits)
    } else {
      # Apply different decimals to different columns
      for (i in seq_along(round_cols)) {
        dt <- dt %>%
          DT::formatRound(round_cols[i], round_digits[i])
      }
    }
  }

  return(dt)
}
