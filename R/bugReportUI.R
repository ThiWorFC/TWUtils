#' Bug Report Module UI
#'
#' @param id Module namespace ID
#' @param email Email address for bug reports
#' @param app_name Name of the app/module for display
#' @param button_label Label for the download button
#' @param button_class CSS class for the download button
#' @param button_width Width of the download button
#'
#' @returns Something
#' @export
#'
#' @examples NULL
bugReportUI <- function(id,
                        email = "thierry.worch@frieslandcampina.com",
                        app_name = "App",
                        button_label = "Download Bug Report",
                        button_class = "xls_dl",
                        button_width = "100%") {
  ns <- NS(id)

  tagList(
    htmlOutput(ns("bug_text")),
    br(),
    downloadButton(
      ns("bug_download"),
      label = button_label,
      class = button_class,
      width = button_width
    )
  )
}
