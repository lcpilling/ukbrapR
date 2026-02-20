#' Extract variants from bulk data and load to memory
#'
#' @description Use user-provided list of genetic variants to extract from imputed BGEN files (field 22828) or WGS DRAGEN BGEN files (field 24309) data and load as data.frame
#'
#' If selecting the DRAGEN data as the source, this assumes your project has access to the WGS BGEN files released April 2025. If not, run `ukbrapR:::make_dragen_bed_from_pvcfs()` to use [tabix] and [plink] to subset the [DRAGEN WGS pVCF files].
#'
#' @return A data frame
#'
#' @author Luke Pilling
#'
#' @name extract_variants
#'
#' @param in_file A data frame or file path. Contains rsid, chr, and pos. For imputed genos pos is build 37. For DRAGEN pos is build 38. Other columns are ignored.
#' @param out_bed A string. Prefix for output files (optional)
#'        \code{default="tmp"}
#' @param source A string. Either "imputed" or "dragen" - indicating whether the variants should be from "UKB imputation from genotype" (field 22828) or "DRAGEN population level WGS variants, PLINK format [500k release]" (field 24308)
#'        \code{default="imputed"}
#' @param use_imp_pos Logical. If source imputed, use position instead of rsID to extract variants?,
#'        \code{default=FALSE}
#' @param overwrite Logical. Overwrite output BED files? (If output prefix is left as 'tmp' overwrite is set to TRUE),
#'        \code{default=FALSE}
#' @param progress Logical. Show progress through each individual file,
#'        \code{default=FALSE}
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' liver_variants <- extract_variants(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_bed="liver_cirrhosis.imputed.variants")
#'
#' @export
#'
extract_variants <- function(
	in_file,
	out_bed="tmp",
	source="imputed",
	use_imp_pos=FALSE,
	overwrite=FALSE,
	progress=FALSE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	# start up messages
	.ukbrapr_startup_notice()
	
	start_time <- Sys.time()
	
	#
	#
	# check inputs
	if (very_verbose)  verbose <- TRUE
	if (verbose) cli::cli_alert("Checking inputs")
	
	# imputed or dragen?
	if (! source %in% c("imputed","dragen")) cli::cli_abort("{.var source} must be either \"imputed\" or \"dragen\"")
	
	# load user-provided varlist file (only first two TSV cols are used: must be chr, bp)
	varlist <- NULL
	
	# if it's a character string, assume user has provided a file path
	if (class(in_file)[1] == "character")  {
		
		if (length(in_file)>1)  cli::cli_abort("Input file path needs to be length 1")
		# does input file exist?
		if (! file.exists(in_file))  cli::cli_abort("Input file not found")
		varlist <- readr::read_tsv(in_file, progress=FALSE, show_col_types=FALSE)
		
	} else if (! any(class(in_file) %in% c("data.frame","tbl","tbl_df")))  {
		
		cli::cli_abort(c(
			"{.var in_file} must be a data.frame (or tibble), or a character string",
			"x" = "You've supplied a {.cls {class(in_file)}}."
		))
		
	} else {
		varlist <- in_file   # user has passed a data frame
	}
	
	# check varlist formatting
	need_pos <- FALSE 
	if (source == "dragen" | use_imp_pos)  need_pos <- TRUE
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, need_pos=need_pos, verbose=verbose)
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	if (out_bed=="tmp")  overwrite <- TRUE
	if (file.exists(paste0(out_bed,".bed")) & !overwrite)  cli::cli_abort("Output bed already exists. To overwrite, set option `overwrite=TRUE`")
	
	#
	#
	# make bed 
	if (source == "imputed")  ukbrapR::make_imputed_bed(in_file=varlist, out_bed=out_bed, use_pos=use_imp_pos, progress=progress, verbose=verbose, very_verbose=very_verbose)
	if (source == "dragen")   ukbrapR::make_dragen_bed(in_file=varlist, out_bed=out_bed, progress=progress, verbose=verbose, very_verbose=very_verbose)
	
	# did it work?
	if (! file.exists(stringr::str_c(out_bed, ".bed")))  cli::cli_abort("Failed to make the BED. Try with `very_verbose=TRUE` to see terminal output.")
	
	# load bed
	bed <- ukbrapR::load_bed(in_bed=out_bed, verbose=verbose, very_verbose=very_verbose)
	
	#
	#
	# finished
	cli::cli_alert_success(c("Loaded data from {ncol(bed)-1} variants."))
	if (verbose) cli::cli_alert_info(c("Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	return(bed)
	
}
