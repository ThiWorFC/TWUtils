# logging_utils.R
# Usage tracking utilities for Shiny applications
# This file provides functions to log user sessions and module access to a CSV file

#' Initialize the log file
#' Creates the logs directory and CSV file with headers if they don't exist
#'
#' @param log_path Path to the log file (default: "logs/usage_log.csv")
#'
#' @export
init_log_file <- function(log_path = "logs/usage_log.csv") {
  # Create logs directory if it doesn't exist
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)

  # Create file with headers if it doesn't exist
  if (!file.exists(log_path)) {
    headers <- data.frame(
      timestamp = character(),
      event_type = character(),
      session_id = character(),
      module_name = character(),
      ip_address = character(),
      user_agent = character(),
      stringsAsFactors = FALSE
    )
    write.csv(headers, log_path, row.names = FALSE)
  }
}

#' Log an event to the CSV file
#' Internal function that writes a log entry to the CSV
#'
#' @param event_type Type of event: "session_start", "session_end", or "module_access"
#' @param session Shiny session object
#' @param module_name Name of the module being accessed (NA for session events)
#' @param log_path Path to the log file
#'
#' @export
log_event <- function(event_type, session, module_name = NA,
                      log_path = "logs/usage_log.csv") {

  # Create log entry
  log_entry <- data.frame(
    timestamp = as.character(Sys.time()),
    event_type = event_type,
    session_id = session$token,
    module_name = as.character(module_name),
    ip_address = session$request$REMOTE_ADDR %||% "unknown",
    user_agent = session$request$HTTP_USER_AGENT %||% "unknown",
    stringsAsFactors = FALSE
  )

  # Append to CSV
  write.table(log_entry,
              file = log_path,
              sep = ",",
              append = TRUE,
              quote = TRUE,
              col.names = FALSE,
              row.names = FALSE)
}

#' Log session start
#' Call this when a user connects to the app
#'
#' @param session Shiny session object
#' @param log_path Path to the log file
#'
#' @export
log_session_start <- function(session, log_path = "logs/usage_log.csv") {
  log_event("session_start", session, module_name = NA, log_path = log_path)
}

#' Log session end
#' Call this when a user disconnects from the app
#'
#' @param session Shiny session object
#' @param log_path Path to the log file
#'
#' @export
log_session_end <- function(session, log_path = "logs/usage_log.csv") {
  log_event("session_end", session, module_name = NA, log_path = log_path)
}

#' Log module access
#' Call this when a user accesses a specific module
#'
#' @param session Shiny session object
#' @param module_name Name/ID of the module being accessed
#' @param log_path Path to the log file
#'
#' @export
log_module_access <- function(session, module_name, log_path = "logs/usage_log.csv") {
  log_event("module_access", session, module_name = module_name, log_path = log_path)
}
