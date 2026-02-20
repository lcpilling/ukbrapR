
#' Load BED file into memory
#'
#' @description Use Plink to convert BED to RAW then easily load it
#'
#' @return A data frame
#'
#' @author Luke Pilling
#'
#' @name load_bed
#'
#' @param in_bed A string. BED prefix
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' liver_variants <- load_bed(in_bed="liver_cirrhosis.imputed.variants")
#'
#' @export
#'
load_bed <- function(
	in_bed,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	# start up messages
	.ukbrapr_startup_notice()
		
	# if it's a character string, assume user has provided a file path
	if (class(in_bed)[1] == "character")  {
		
		if (length(in_bed)>1)  cli::cli_abort("Input file path needs to be length 1")
		
		# does input file exist?
		if (! file.exists(stringr::str_c(in_bed, ".bed")))  cli::cli_abort("Input file not found")
		
	}
	
	#
	# get plink 1.9
	ukbrapR:::prep_tools(get_plink=TRUE, verbose=verbose, very_verbose=very_verbose)
	
	# use Plink to convert
	if (verbose) cli::cli_alert("Use Plink to convert BED to RAW text file")
	c1 <- paste0("~/_ukbrapr_tools/plink --bfile ", in_bed, " --recode A --out _ukbrapr_tmp")
	if (very_verbose)  {
		system(c1)
	} else {
		system(stringr::str_c(c1, " >/dev/null"))
	}
	
	# load data to format / merge
	if (verbose) cli::cli_alert("Read into memory and format")
	bed <- readr::read_delim("_ukbrapr_tmp.raw", delim=" ", progress=FALSE, show_col_types=FALSE)
	bed <- bed |>
		dplyr::rename(eid=FID) |>
		dplyr::select(-IID, -PAT, -MAT, -SEX, -PHENOTYPE)
	
	# remove tmp files
	system("rm _ukbrapr_tmp*")
	
	# return
	return(bed)
	
}
