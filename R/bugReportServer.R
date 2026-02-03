#' Bug Report Module Server
#'
#' @param id Module namespace ID
#' @param data_files Reactive expression returning file input(s)
#' @param session Parent module's session object
#' @param app_name Name for the output files and metadata
#' @param email Email address for bug reports
#' @param exclude_inputs Character vector of input names to exclude from capture
#' @param inputs_format Format for inputs file: "excel" or "txt"
#'
#' @return Something
#' @export
#'
#' @examples NULL
bugReportServer <- function(id,
                            data_files,
                            session,
                            app_name = "App",
                            email = "thierry.worch@frieslandcampina.com",
                            exclude_inputs = NULL,
                            inputs_format = c("excel", "txt")) {

  inputs_format <- match.arg(inputs_format)

  moduleServer(id, function(input, output, module_session) {

    # Render bug report instructions
    output$bug_text <- renderText({
      paste0(
        "<br>",
        "<b>Encountered an issue?</b><br><br>",
        sprintf(
          "Download the file below and attach it to an <a href='mailto:%s?subject=%s - Bug Report'>email here</a>. ",
          email, app_name
        ),
        "Please include a brief description of the issue in your email.",
        "<br><br>"
      )
    })

    # Download handler
    output$bug_download <- downloadHandler(
      filename = function() {
        timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
        sprintf("%s_BugReport_%s.zip",
                gsub(" ", "_", app_name),
                timestamp)
      },
      content = function(file) {

        # Create temporary directory for bug report contents
        temp_dir <- tempfile()
        dir.create(temp_dir, recursive = TRUE)

        files <- data_files()
        req(files)

        # 1. Recreate original data file(s)
        data_dir <- file.path(temp_dir, "data_files")
        dir.create(data_dir)

        recreated_files <- recreate_data_files(files, data_dir)

        # 2. Capture inputs
        inputs_snapshot <- capture_shiny_inputs(
          session = session,
          exclude = exclude_inputs
        )

        # 3. Save inputs in requested format
        if (inputs_format == "excel") {
          inputs_file <- file.path(temp_dir, "session_inputs.xlsx")
          save_inputs_snapshot(
            inputs_tibble = inputs_snapshot,
            filepath = inputs_file,
            add_metadata = TRUE,
            app_name = app_name
          )
        } else {
          # Text format
          inputs_file <- file.path(temp_dir, "session_inputs.txt")

          # Create formatted text output
          txt_content <- c(
            sprintf("=== %s - Session Inputs ===", app_name),
            sprintf("Timestamp: %s", Sys.time()),
            sprintf("User: %s", Sys.info()["user"]),
            sprintf("R Version: %s.%s", R.version$major, R.version$minor),
            "",
            "=== Input Values ===",
            ""
          )

          for (i in seq_len(nrow(inputs_snapshot))) {
            txt_content <- c(
              txt_content,
              sprintf("[%s] (%s)",
                      inputs_snapshot$input_name[i],
                      inputs_snapshot$value_class[i]),
              sprintf("  Value: %s", inputs_snapshot$input_value[i]),
              ""
            )
          }

          writeLines(txt_content, inputs_file)
        }

        # 4. Create README
        readme_file <- file.path(temp_dir, "README.txt")
        readme_content <- c(
          sprintf("Bug Report for: %s", app_name),
          sprintf("Generated: %s", Sys.time()),
          "",
          "Contents:",
          "- data_files/: Original data file(s) exactly as uploaded",
          sprintf("- session_inputs.%s: All input values at time of bug report",
                  ifelse(inputs_format == "excel", "xlsx", "txt")),
          "- This README file",
          "",
          "Instructions:",
          sprintf("Please email this entire zip file to %s with a description of the issue.",
                  email),
          "",
          "System Information:",
          sprintf("R Version: %s.%s", R.version$major, R.version$minor),
          sprintf("Platform: %s", Sys.info()["sysname"]),
          sprintf("User: %s", Sys.info()["user"])
        )
        writeLines(readme_content, readme_file)

        # 5. Create zip file
        all_files <- c(
          list.files(data_dir, full.names = TRUE, recursive = TRUE),
          inputs_file,
          readme_file
        )

        # Get relative paths for zip
        old_wd <- getwd()
        setwd(temp_dir)

        rel_paths <- c(
          file.path("data_files", list.files(data_dir)),
          basename(inputs_file),
          basename(readme_file)
        )

        zip::zip(zipfile = file, files = rel_paths)

        setwd(old_wd)

        # Cleanup
        unlink(temp_dir, recursive = TRUE)
      }
    )
  })
}
