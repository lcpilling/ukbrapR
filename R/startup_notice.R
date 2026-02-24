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

    cli::cli_inform(c(
      "",
      "i" = "Hi! Once-per-session message from Luke. I am keen to understand how widely {.pkg ukbrapR} is used. Please consider {.emph starring} the repo ({cli::symbol$star} {.url https://github.com/lcpilling/ukbrapR}), {.strong citing it in papers}, and sharing feedback. Thanks for supporting {.pkg ukbrapR}!",
      ""
    ))
  }

  invisible(NULL)
}
