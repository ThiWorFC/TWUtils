# R/render_release_notes.R

#' Render Release Notes from YAML
#'
#' @param news_file Path to NEWS.yaml file. Defaults to "NEWS.yaml" in package root
#' @param max_height Maximum height of scrollable container (CSS units)
#' @param color_scheme Named list mapping change types to bs4Dash colors
#'
#' @return HTML div with formatted release notes
#' @export
#'
#' @examples NULL
render_release_notes <- function(
    news_file = "NEWS.yaml",
    max_height = "400px",
    color_scheme = NULL
) {

  # Default color scheme
  if (is.null(color_scheme)) {
    color_scheme <- list(
      added = "success",
      updated = "info",
      fixed = "warning",
      removed = "danger",
      security = "danger"
    )
  }

  # Check file exists
  if (!file.exists(news_file)) {
    return(tags$div(
      class = "alert alert-warning",
      "No release notes found."
    ))
  }

  news <- yaml::read_yaml(news_file)

  # Build HTML
  html_content <- lapply(news$releases, function(release) {
    tagList(
      tags$h5(
        class = "text-primary font-weight-bold mb-2",
        glue::glue("Version {release$version}"),
        tags$small(class = "text-muted ml-2", release$date)
      ),
      lapply(names(release$changes), function(type) {
        tags$div(
          class = "mb-3",
          tags$span(
            class = glue::glue("badge badge-{color_scheme[[type]]} mr-2"),
            toupper(type)
          ),
          tags$ul(
            class = "mb-0 ml-3",
            lapply(release$changes[[type]], function(item) {
              tags$li(item)
            })
          )
        )
      }),
      tags$hr(class = "my-3")
    )
  })

  # Return scrollable container
  tags$div(
    style = glue::glue("max-height: {max_height}; overflow-y: auto;"),
    class = "release-notes p-3",
    html_content
  )
}

#' Custom scrollbar CSS for release notes
#'
#' @export
release_notes_css <- function() {
  tags$head(tags$style(HTML("
    .release-notes::-webkit-scrollbar {
      width: 8px;
    }
    .release-notes::-webkit-scrollbar-track {
      background: #f1f1f1;
      border-radius: 4px;
    }
    .release-notes::-webkit-scrollbar-thumb {
      background: #888;
      border-radius: 4px;
    }
    .release-notes::-webkit-scrollbar-thumb:hover {
      background: #555;
    }
  ")))
}
