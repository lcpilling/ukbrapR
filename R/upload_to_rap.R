#' Use R to upload a file to the UK Biobank RAP
#'
#' @description Use R to upload a file to the UK Biobank RAP (really just a wrapper for `dx upload`)
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name upload_to_rap
#'
#' @param file A string. Filename of the file to be uploaded (character)
#' @param dir A string. Target directory in the RAP space. If blank, the current working directory (character)
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' readr::write_tsv(data.frame(x=1:10,y=11:20), "ukbrap.dummy.20231114.txt.gz")
#'
#' # upload file to RAP storage
#' upload_to_rap(file="ukbrap.dummy.20231114.txt.gz", dir="extracts/")
#'
#' @export
#'
upload_to_rap <- function(
	file,
	dir = "FALSE",
	verbose = FALSE
)  {

	# start up messages
  pkg_version <- utils::packageVersion("ukbrapR")
  cli::cli_alert_info("{.pkg ukbrapR} v{pkg_version}")
  .ukbrapr_startup_notice()

	if (verbose) cat("Check input & options\n")
	if (! is.character(file) ) stop("`file` needs to be a string of the filename to upload")
	if (! file.exists(file) ) stop("`file` not found")

	if (dir == "FALSE")  {
		if (verbose) cat("Uploading to current working directory\n")
		system(paste0("dx upload ", file))
	}  else  {
		if (verbose) cat(paste0("Uploading to `dir`: ", dir, "\n"))
		if (substr(dir, nchar(dir), nchar(dir)) != "/")  dir <- paste0(dir, "/")
		system(paste0("dx mkdir -p ", dir))
		system(paste0("dx upload ", file, " --path ", dir))
	}

}


#' Use R to download a file to the UK Biobank RAP
#'
#' @description Use R to download a file to the UK Biobank RAP (really just a wrapper for `dx download`)
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name download_from_rap
#'
#' @param file A string. Filename of the file to be downloaded (character)
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' readr::write_tsv(data.frame(x=1:10,y=11:20), "ukbrap.dummy.20231114.txt.gz")
#'
#' # download file to RAP storage
#' download_from_rap(file="ukbrap.dummy.20231114.txt.gz", dir="extracts/")
#'
#' @export
#'
download_from_rap <- function(
	file,
	verbose = FALSE
)  {

	# start up messages
  pkg_version <- utils::packageVersion("ukbrapR")
  cli::cli_alert_info("{.pkg ukbrapR} v{pkg_version}")
  .ukbrapr_startup_notice()

	if (verbose) cat("Check input & options\n")
	if (! is.character(file) ) stop("`file` needs to be a string of the filename to download")
	if (! file.exists(file) ) stop("`file` not found")

	if (verbose) cat("downloading to current working directory\n")
	system(paste0("dx download ", file))

}


