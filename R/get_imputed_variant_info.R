#' Get variant info from UK Biobank imputed genotype MFI files
#'
#' @description For a given set of genomic coordinates (position in build 37) get the UK Biobank imputed genotype variant IDs from the MFI files.
#'
#' @return A data frame of variants with added "ukb_rsid" column (also returns alleles, MAF and INFO)
#'
#' @author Luke Pilling
#'
#' @name get_imputed_variant_info
#'
#' @param varlist A data frame. Contains at least two columns: `chr` and `pos` (b37). Other columns are ignored.
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # Example where RSID is only known for some variants
#' varlist1 <- data.frame(rsid=c("","rs564086017",""), chr=c(1,2,22), pos=c(10616, 10180, 16050435))
#' varlist2 <- get_imputed_variant_info(varlist1)
#'
#' @export
#' 
get_imputed_variant_info <- function(
	varlist,
	verbose=FALSE
)  {
	
	# start
	start_time <- Sys.time()
	
	#
	#
	# check inputs
	if (verbose) cli::cli_alert("Checking inputs")
	
	# check varlist formatting
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, need_pos=TRUE, verbose=verbose)
	varlist$ukb_minor_allele <- varlist$ukb_a2 <- varlist$ukb_a1 <- varlist$ukb_rsid <- varlist$ukb_variant_id <- ""
	varlist$ukb_info <- varlist$ukb_maf <- NA_real_
	
	#
	#
	# for each CHR 
	chrs <- unique(varlist$chr)
	n_chrs <- length(chrs)
	
	# show progress
	cli::cli_alert("Getting variant IDs for {nrow(varlist)} variant{?s} from {n_chrs} imputed BGEN file{?s}")
	
	# loop over files...
	for (ii in 1:n_chrs)  {
		
		# this CHR
		chr <- chrs[ii]
		if (verbose) cli::cli_alert("Processing CHR {chr} ({ii} of {n_chrs})")		
		
		# path to MFI
		mfi_path <- stringr::str_c("/mnt/project/Bulk/Imputation/UKB\\ imputation\\ from\\ genotype/ukb22828_c", chr, "_b0_v3.mfi.txt")
		
		# build grep query for this CHR
		# grep whole word matches only 
		varlist_sub <- varlist |> dplyr::filter(chr==!!chr)
		search_string <- paste0("grep -E -w ", sprintf('"%s"', stringr::str_flatten(varlist_sub$pos, collapse = "|")), " ", sprintf('%s', mfi_path))
		
		# use search string to only read lines that matched a code
		mfi_tbl <- suppressWarnings(readr::read_tsv(pipe(search_string), col_names=c("ukb_variant_id","ukb_rsid","pos","ukb_a1","ukb_a2","ukb_maf","ukb_minor_allele","ukb_info"), show_col_types=FALSE, progress=FALSE))

		# make sure no rogue matches occurred
		mfi_tbl <- mfi_tbl |> dplyr::filter(pos %in% varlist_sub$pos)

		# for each variant in varlist_sub, get the variant_id from mfi_tbl
		for (jj in 1:nrow(varlist_sub))  {	
			r <- mfi_tbl |> dplyr::filter(pos==!!varlist_sub$pos[jj])
			# find any for this pos?
			if (nrow(r)>0)  {
				varlist$ukb_variant_id[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_variant_id[1]
				varlist$ukb_rsid[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_rsid[1]
				varlist$ukb_a1[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_a1[1]
				varlist$ukb_a2[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_a2[1]
				varlist$ukb_minor_allele[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_minor_allele[1]
				varlist$ukb_maf[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_maf[1]
				varlist$ukb_info[ varlist$chr==chr & varlist$pos==varlist_sub$pos[jj] ] <- r$ukb_info[1]
				if (nrow(r)>1)  {
					# Multiple matches found for variant - add new row for each duplicated position
					for (kk in 2:nrow(r))  {
						new_row <- varlist_sub[jj, ]
						new_row$ukb_variant_id <- r$ukb_variant_id[kk]
						new_row$ukb_rsid <- r$ukb_rsid[kk]
						new_row$ukb_a1 <- r$ukb_a1[kk]
						new_row$ukb_a2 <- r$ukb_a2[kk]
						new_row$ukb_minor_allele <- r$ukb_minor_allele[kk]
						new_row$ukb_maf <- r$ukb_maf[kk]
						new_row$ukb_info <- r$ukb_info[kk]
						varlist <- dplyr::bind_rows(varlist, new_row)
					}
				}
			}
		}

	}

	if (verbose) cli::cli_alert_success(stringr::str_c("Got variant IDs in ", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units="secs")))))

	# return
	return(varlist)

}
