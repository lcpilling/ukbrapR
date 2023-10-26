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
#' # upload file to RAP storage
#' ukbrapR::upload_to_rap(file="ukb14631.data_output.20231026.txt.gz", dir="extracts/")
#'
#' @export
#'
upload_to_rap <- function(file,
                          dir="FALSE",
                          verbose=FALSE)  {

	if (verbose) cat("Check input & options\n")
	if (! is.character(file) ) stop("`file` needs to be a string of the filename to upload")
	if (! file.exists(file) ) stop("`file` not found")

	if (dir == "FALSE")  {
		if (verbose) cat("Uploading to current working directory\n")
		system(paste0("dx upload ", file))
	}  else  {
		if (verbose) cat(paste0("Uploading to `dir`: ", dir, "\n"))
		if (substr(dir, nchar(dir), nchar(dir)) != "/")  dir <- paste0(dir, "/")
		system(paste0("dx upload ", file, " --path ", dir))
	}

}

