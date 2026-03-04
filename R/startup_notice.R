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
      "i" = "Hi, thanks for using {.pkg ukbrapR}! Please consider {.emph starring} the repo ({cli::symbol$star} {.url https://github.com/lcpilling/ukbrapR}), {.strong citing it in papers} ({.url https://doi.org/10.5281/zenodo.18836738}), and sharing feedback.",
      ""
    ))
  }

  invisible(NULL)
}
