#' Package notice
#'
#' Prints a feedback notice once per session.
#'
#' @return Invisible NULL.
#' @keywords internal
.ukbrapr_startup_notice <- function() {
  # Feedback notice once per session
  if (!isTRUE(getOption("ukbrapR.feedback_notice_shown", FALSE))) {
    options(ukbrapR.feedback_notice_shown = TRUE)

    cli::cli_ul(c(
      "Thanks for using {.pkg ukbrapR}. I am keen to understand how widely it is being used.",
      "Please star the repo {cli::symbol$star} and consider sharing feedback {.url https://github.com/lcpilling/ukbrapR}"
    ))
  }

  invisible(NULL)
}
