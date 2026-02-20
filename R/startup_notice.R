#' Internal package state (not exported)
#' @keywords internal
.ukbrapr_state <- new.env(parent = emptyenv())

#' Package notice and version banner
#'
#' Prints a short banner every time, and a feedback notice once per session.
#' 
#' @return Invisible NULL.
#' 
#' @keywords internal
.ukbrapr_startup_notice <- function() {
  # Suppress banner for nested calls within the same top-level entry
  if (isTRUE(.ukbrapr_state$banner_active)) {
    return(invisible(NULL))
  }

  .ukbrapr_state$banner_active <- TRUE
  on.exit({ .ukbrapr_state$banner_active <- FALSE }, add = TRUE)

  
  # get package version
  pkg_version <- utils::packageVersion("ukbrapR")

  # Identify what function the user called from the call stack
  calls <- sys.calls()
  caller_name <- "<unknown>"

  if (length(calls) > 0L) {
    call_names <- vapply(
      calls,
      FUN = function(x) {
        head_expr <- x[[1L]]
        deparse(head_expr, nlines = 1L)[[1L]]
      },
      FUN.VALUE = character(1L)
    )

    # Remove any namespace qualifiers (e.g. ukbrapR::export_tables -> export_tables)
    call_names <- sub("^.*:::", "", call_names)
    call_names <- sub("^.*::", "", call_names)

    is_candidate <- !startsWith(call_names, ".") &
      !startsWith(call_names, "sys.") &
      nzchar(call_names)

    candidate_names <- call_names[is_candidate]
    caller_name <- if (length(candidate_names) > 0L) candidate_names[[1L]] else call_names[[1L]]
  }


  # Show package version & calling function
  cli::cli_alert_info("{.pkg ukbrapR}::{caller_name}() v{pkg_version}")

  # Show feedback notice once per session
  if (!isTRUE(getOption("ukbrapR.feedback_notice_shown", FALSE))) {
    options(ukbrapR.feedback_notice_shown = TRUE)

    cli::cli_ul(c(
        "Thanks for using {.pkg ukbrapR}. I am keen to understand how widely it is being used.",
        "Please star the repo {cli::symbol$star} and consider sharing feedback {.url https://github.com/lcpilling/ukbrapR}"
    ))
  }

  invisible(NULL)
}
