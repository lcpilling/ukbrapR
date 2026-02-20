#' Package notice and version banner
#'
#' Prints a short banner every time, and a feedback notice once per session.
#' 
#' @return Invisible NULL.
#' 
#' @keywords internal
.ukbrapr_startup_notice <- function() {
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
  cli::cli_alert_info("{.pkg ukbrapR} v{pkg_version}: {caller_name}()")

  # Show feedback notice once per session
  if (!isTRUE(getOption("ukbrapR.feedback_notice_shown", FALSE))) {
    options(ukbrapR.feedback_notice_shown = TRUE)

    cli::cli_inform(c(
      "i" = "Thanks for using {.pkg ukbrapR} — I hope it is useful.",
      " " = "I am keen to understand how it is being used.",
      " " = "Consider starring the repo ({.url https://github.com/lcpilling/ukbrapR}) and sharing feedback."
    ))
  }

  invisible(NULL)
}
