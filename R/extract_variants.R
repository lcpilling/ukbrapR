#' Extract variants from bulk data and load to memory
#'
#' @description Use user-provided list of genetic variants to extract from imputed or WGS (DRAGEN) data and load as data.frame
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
#' @param source A string. Either "imputed" or "dragen" - indicating whether the variants should be from "UKB imputation from genotype" (field 22828) or "DRAGEN population level WGS variants, pVCF format [500k release]" (field 24310)
#'        \code{default="imputed"}
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
	overwrite=FALSE,
	progress=FALSE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	v <- packageVersion("ukbrapR")
	cli::cli_alert_info("ukbrapR v{v}")
	
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
	if (source == "dragen")  need_pos <- TRUE
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, need_pos=need_pos, verbose=verbose)
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	if (out_bed=="tmp")  overwrite <- TRUE
	if (file.exists(paste0(out_bed,".bed")) & !overwrite)  cli::cli_abort("Output bed already exists. To overwrite, set option `overwrite=TRUE`")
	
	#
	#
	# make bed 
	if (source == "imputed")  ukbrapR::make_imputed_bed(in_file=varlist, out_bed=out_bed, progress=progress, verbose=verbose, very_verbose=very_verbose)
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




#' Create a polygenic score
#'
#' @description Use user-provided list of genetic variants with weights for a trait to create a polygenic score
#'
#' @return A data frame
#'
#' @author Luke Pilling
#'
#' @name create_pgs
#'
#' @param in_file A data frame or file path. Must contain rsid, chr, pos, effect_allele, other_allele, beta. For imputed genos pos is build 37. For DRAGEN pos is build 38. Other columns are ignored.
#' @param out_file A string. Prefix for output files (optional)
#'        \code{default="tmp"}
#' @param pgs_name A string. Variable name for created PGS (optional)
#'        \code{default="pgs"}
#' @param source A string. Either "imputed" or "dragen" - indicating whether the variants should be from "UKB imputation from genotype" (field 22828) or "DRAGEN population level WGS variants, pVCF format [500k release]" (field 24310)
#'        \code{default="imputed"}
#' @param overwrite Logical. Overwrite output BED files? (If out_file is left as 'tmp' overwrite is set to TRUE),
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
#' liver_pgs <- create_pgs(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_file="liver_cirrhosis.imputed.pgs", pgs_name="liver_cirrhosis_pgs")
#'
#' @export
#'
create_pgs <- function(
	in_file,
	out_file="tmp",
	pgs_name="pgs",
	source="imputed",
	overwrite=FALSE,
	progress=FALSE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	v <- packageVersion("ukbrapR")
	cli::cli_alert_info("ukbrapR v{v}")
	
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
		
	} else {  # user has passed a data frame
		varlist <- in_file
	}
	
	# check varlist formatting and save
	readr::write_tsv(varlist, stringr::str_c(out_file, ".input.txt"))
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=TRUE, verbose=verbose)
	out_file_varlist <- stringr::str_c(out_file, ".varlist.txt")
	
	# check input is right for the source:
	if (source == "dragen")  {
		varlist <- varlist |>
			dplyr::mutate(filename="")
	}
	
	# check output format 
	if (! class(out_file)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_file)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	if (out_file=="tmp")  overwrite <- TRUE
	if (file.exists(paste0(out_file,".bed")) & !overwrite)  cli::cli_abort("Output bed already exists. To overwrite, set option `overwrite=TRUE`")
	
	#
	#
	# make bed 
	if (source == "imputed")  ukbrapR::make_imputed_bed(in_file=varlist, out_bed=out_file, progress=progress, verbose=verbose, very_verbose=very_verbose)
	if (source == "dragen")   ukbrapR::make_dragen_bed(in_file=varlist, out_bed=out_file, progress=progress, verbose=verbose, very_verbose=very_verbose)
	
	# did it work?
	if (! file.exists(stringr::str_c(out_file, ".bed")))  cli::cli_abort("Failed to make the BED. Try with `very_verbose=TRUE` to see terminal output.")
	
	#
	#
	# create PGS
	
	# if using DRAGEN we need to replace `rsid` with the `chr:pos:a1:a2` in the WGS bim file 
	if (source == "dragen")  {
		
		# load the bim file
		bim <- readr::read_tsv(stringr::str_c(out_file, ".bim"), col_names=c("chr","id","null","pos","a1","a2"))
		
		# create ID for each row of the varlist - make sure alleles match the bim file
		varlist$rsid_old <- varlist$rsid
		varlist$rsid <- ""
		for (ii in 1:nrow(varlist))  {
			
			# keep bim rows where CHR and POS match
			r <- bim[ bim$chr==varlist$chr[ii] & bim$pos==varlist$pos[ii] , ]
			
			# any matched?
			if (nrow(r)>0)  {
				
				# keep rows where alleles both genotyped
				r <- r[ r$a1 %in% c(varlist$effect_allele[ii],varlist$other_allele[ii]) & r$a2 %in% c(varlist$effect_allele[ii],varlist$other_allele[ii]) , ]
				
				# any matched?
				if (nrow(r)>0)  {
				
					# add `id` to varlist
					# if multiple (shouldn't be!) just use first
					varlist$rsid[ii] <- r$id[1]
				
				}
				
			}
			
		}
		
	}
	
	# save the varlist for plink
	readr::write_tsv(varlist, out_file_varlist)
	
	# Plink
	if (verbose) cli::cli_alert("Make PGS")
	c1 <- paste0("~/_ukbrapr_tools/plink --bfile ", out_file, " --score ", out_file_varlist, " 1 4 6 header --out ", out_file)
	if (very_verbose)  {
		system(c1)
	} else {
		system(stringr::str_c(c1, " >/dev/null"))
	}  
	
	# did it work?
	if (! file.exists(stringr::str_c(out_file, ".profile")))  cli::cli_abort("Plink failed to make the allele score. Try with `very_verbose=TRUE` to see terminal output.")
	
	# just extract EID and SCORE to a .tsv file -- remove participants with invalid EIDs < 0
	system(stringr::str_c("echo \"eid\t", pgs_name, "\" > ", out_file, ".tsv"))
	system(stringr::str_c("awk 'NR > 1 && $1 > 0 { print $1\"\t\"$6 }' ", out_file, ".profile >> ", out_file, ".tsv"))
	
	# load
	pgs <- readr::read_tsv(stringr::str_c(out_file, ".tsv"), progress=FALSE, show_col_types=FALSE)
	
	#
	#
	# finished
	cli::cli_alert_success(stringr::str_c("PGS created! See file {.file ", out_file, ".tsv}"))
	if (verbose) cli::cli_alert_info(c("Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	return(pgs)
	
}




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


#' Extract variants from DRAGEN PGEN file(s) into single BED file 
#'
#' @description For a given set of genomic coordinates extract the UK Biobank WGS DRAGEN variant calls (from the PLINK format PGEN files) into a single BED file.
#'
#' Plink2 requires up to 10Gb of reserved RAM (depending on CHR), and takes up to 40 seconds per CHR.
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_dragen_bed
#'
#' @param in_file A data frame or file path. Contains at least two columns: `chr` and `pos` (in build 38). Other columns are ignored.
#' @param out_bed A string. 
#' @param memory Integer. The memory for Plink2 to reserve (in MiB),
#'        \code{default=10000}
#' @param progress Logical. Show progress through each individual file,
#'        \code{default=TRUE}
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' make_dragen_bed(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_bed="liver_cirrhosis.dragen.variants")
#'
#' @export
#'
make_dragen_bed <- function(
	in_file,
	out_bed,
	memory=10000,
	progress=TRUE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	# start
	start_time <- Sys.time()
	
	#
	#
	# check inputs
	if (very_verbose)  verbose <- TRUE
	if (verbose) cli::cli_alert("Checking inputs")
	
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
			"x" = "You've supplied a {.cls {class(in_file)}} vector."
		))
		
	} else {
		varlist <- in_file   # user has passed a data frame
	}
	
	# check varlist formatting
	varlist$rsid <- ""
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, verbose=verbose)
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	
	#
	#
	# get plink 2
	ukbrapR:::prep_tools(get_plink=TRUE, get_plink2=TRUE, verbose=verbose, very_verbose=very_verbose)
	
	#
	#
	# for each CHR 
	chrs <- unique(varlist$chr)
	n_chrs <- length(chrs)
	
	# show progress
	cli::cli_alert("Extracting {nrow(varlist)} variant{?s} from {n_chrs} PGEN file{?s} (ETA {prettyunits::pretty_sec(n_chrs*30)})")
	
	# loop over files...
	for (ii in 1:n_chrs)  {
		
		# this CHR
		chr <- chrs[ii]
		chr_time <- Sys.time()
		
		# get variants list for this file
		varlist_sub <- varlist |> dplyr::filter(chr==!!chr) |> dplyr::mutate(pos2=pos)
		readr::write_tsv(dplyr::select(varlist_sub, chr, pos, pos2), "_ukbrapr_tmp_range.txt", col_names = FALSE, progress = FALSE)
		
		# path to PGEN
		pgen_path <- stringr::str_c("/mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ PLINK\\ format\\ \\[500k\\ release\\]/ukb24308_c", chr, "_b0_v1")
		
		# use plink2 to extract subset of PGEN
		if (verbose) cli::cli_alert(stringr::str_c("Using plink2 to extract the positions from chr", chr))
		c1 <- stringr::str_c("~/_ukbrapr_tools/plink2 --pfile ", pgen_path, " --extract range _ukbrapr_tmp_range.txt --no-pheno --memory ", memory, " --make-bed --out _ukbrapr_tmp")
		if (very_verbose)  {
			system(c1)
		} else {
			system(stringr::str_c(c1, " &> /dev/null"))
		}
		
		# did it work?
		bed_available <- TRUE
		if (! file.exists("_ukbrapr_tmp.bed"))  {
			bed_available <- FALSE
			
			# did it "fail" or were there just no variants? Check the log 
			if (file.exists("_ukbrapr_tmp.log"))  {
				vars_not_found <- system("grep \"No variants remaining after main filters\" _ukbrapr_tmp.log >/dev/null")
				if (vars_not_found == 1)  {
					cli::cli_abort("Plink2 failed to extract from the UKB PGEN. Try with `very_verbose=TRUE` to see terminal output.")
				}
			}
		}
		
		# if no variants in the BGEN (nrow of list file <=3) then skip this CHR
		if (bed_available)  {
			
			# if this is the first one, simply rename
			if (ii==1)  {
				system(stringr::str_c("mv _ukbrapr_tmp.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp.fam ", out_bed, ".fam"))
			}
			
			# if not the first one, use plink2 to merge beds
			if (ii>1)  {
				if (verbose) cli::cli_alert("Merge BEDs")
				c1 <- stringr::str_c("~/_ukbrapr_tools/plink --bfile ", out_bed, " --bmerge _ukbrapr_tmp --make-bed --out _ukbrapr_tmp2")
				if (very_verbose)  {
					system(c1)
				} else {
					system(stringr::str_c(c1, " &> /dev/null"))
				}  
				system(stringr::str_c("mv _ukbrapr_tmp2.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp2.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp2.fam ", out_bed, ".fam"))
			}
		
		} else {
			cli::cli_warn(stringr::str_c("Variants on CHR ", chr, " are in the input varlist but are missing from imputed PGEN"))
		}
		
		# remove tmp files
		system("rm _ukbrapr_tmp*")
		
		# give update
		if (progress)  cli::cli_alert_info(stringr::str_c("Extracted from PGEN chr", chr, " (", ii, " of ", n_chrs, ") [", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), chr_time, units="secs"))), "]"))
		
	}
	
	# finished
	if (n_chrs>1)  {
		cli::cli_progress_done()
		options(cli.progress_show_after = 2)
	}
	
	# finished
	cli::cli_alert_success(stringr::str_c("DRAGEN BED made in ", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units="secs")))))
	
}




#' Extract variants from imputed genotype file(s) into single BED file 
#'
#' @description For a given set of genomic coordinates extract from the UK Biobank imputed genotypes (v3) into a single BED file.
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_imputed_bed
#'
#' @param in_file A data frame or file path. Contains at least two columns: `rsID` and `CHR`. Other columns are ignored.
#' @param out_bed A string. 
#' @param progress Logical. Show progress through each individual file,
#'        \code{default=TRUE}
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' make_imputed_bed(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_bed="liver_cirrhosis.imputed.variants")
#'
#' @export
#'
make_imputed_bed <- function(
	in_file,
	out_bed,
	progress=TRUE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	# start
	start_time <- Sys.time()
	
	#
	#
	# check inputs
	if (very_verbose)  verbose <- TRUE
	if (verbose) cli::cli_alert("Checking inputs")
	
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
			"x" = "You've supplied a {.cls {class(in_file)}} vector."
		))
		
	} else {
		varlist <- in_file   # user has passed a data frame
	}
	
	# check varlist formatting
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, need_pos=FALSE, verbose=verbose)
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")

	#
	# get bgen, plink 1.9 and plink 2
	ukbrapR:::prep_tools(get_plink=TRUE, get_plink2=TRUE, get_bgen=TRUE, verbose=verbose, very_verbose=very_verbose)
	
	#
	#
	# for each CHR 
	chrs <- unique(varlist$chr)
	n_chrs <- length(chrs)
	
	# show progress
	cli::cli_alert("Extracting {nrow(varlist)} variant{?s} from {n_chrs} imputed bgen file{?s} (ETA {prettyunits::pretty_sec(n_chrs*8)})")
	
	# loop over files...
	for (ii in 1:n_chrs)  {
		
		# this CHR
		chr <- chrs[ii]
		chr_time <- Sys.time()
		
		# get variants list for this file
		varlist_sub <- varlist |> dplyr::filter(chr==!!chr)
		readr::write_tsv(dplyr::select(varlist_sub, rsid), "_ukbrapr_tmp_rsids.txt", col_names = FALSE, progress = FALSE)
		
		# path to BGEN
		bgen_path <- stringr::str_c("/mnt/project/Bulk/Imputation/UKB\\ imputation\\ from\\ genotype/ukb22828_c", chr, "_b0_v3.bgen")
		
		# use bgenix to extract subset of BGEN
		if (verbose) cli::cli_alert("Use bgenix to extract the positions")
		c1 <- stringr::str_c("~/_ukbrapr_tools/bgenix -g ", bgen_path, " -incl-rsids _ukbrapr_tmp_rsids.txt > _ukbrapr_tmp.bgen")
		if (very_verbose)  {
			system(c1)
		} else {
			system(stringr::str_c(c1, " 2>/dev/null"))
		}
		
		# did it work?
		if (! file.exists("_ukbrapr_tmp.bgen"))  cli::cli_abort("BGENIX failed to extract from the UKB BGEN. Try with `very_verbose=TRUE` to see terminal output.")
		
		# does the BGEN actually contain variants? -- create index and check file length 
		c1 <- stringr::str_c("~/_ukbrapr_tools/bgenix -g _ukbrapr_tmp.bgen -index")
		if ( very_verbose)  system(c1)
		if (!very_verbose)  system(stringr::str_c(c1, " 2>/dev/null"))
		c1 <- stringr::str_c("~/_ukbrapr_tools/bgenix -g _ukbrapr_tmp.bgen -list > _ukbrapr_tmp.bgen.list")
		if ( very_verbose)  system(c1)
		if (!very_verbose)  system(stringr::str_c(c1, " 2>/dev/null"))
		n_rows <- as.integer(system("wc -l < _ukbrapr_tmp.bgen.list", intern = TRUE)) 
		
		# if no variants in the BGEN (nrow of list file <=3) then skip this CHR
		if (n_rows > 3)  {
			
			# use Plink to convert to BED
			if (verbose) cli::cli_alert("Use plink2 to convert BGEN to BED")
			c1 <- stringr::str_c("~/_ukbrapr_tools/plink2 --bgen _ukbrapr_tmp.bgen ref-first --sample /mnt/project/Bulk/Imputation/UKB\\ imputation\\ from\\ genotype/ukb22828_c", chr, "_b0_v3.sample --make-bed --out _ukbrapr_tmp")
			if (very_verbose)  {
				system(c1)
			} else {
				system(stringr::str_c(c1, " >/dev/null"))
			}
			
			# did it work?
			if (! file.exists("_ukbrapr_tmp.bed"))  cli::cli_abort("plink2 failed to convert the BGEN to BED. Try with `very_verbose=TRUE` to see terminal output.")
			
			# if this is the first one, simply rename
			if (ii==1)  {
				system(stringr::str_c("mv _ukbrapr_tmp.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp.fam ", out_bed, ".fam"))
			}
			
			# if not the first one, use plink to merge beds
			if (ii>1)  {
				if (verbose) cli::cli_alert("Merge BEDs")
				c1 <- stringr::str_c("~/_ukbrapr_tools/plink --bfile ", out_bed, " --bmerge _ukbrapr_tmp --make-bed --out _ukbrapr_tmp2")
				if (very_verbose)  {
					system(c1)
				} else {
					system(stringr::str_c(c1, " >/dev/null"))
				}  
				system(stringr::str_c("mv _ukbrapr_tmp2.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp2.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp2.fam ", out_bed, ".fam"))
			}
		
		} else {
			cli::cli_warn(stringr::str_c("Variants on CHR ", chr, " are in the input varlist but are missing from imputed BGEN"))
		}
		
		# remove tmp files
		system("rm _ukbrapr_tmp*")
		
		# give update
		if (progress)  cli::cli_alert_info(stringr::str_c("Extracted from BGEN ", ii, " of ", n_chrs, " [", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), chr_time, units="secs"))), "]"))
		
	}
	
	# finished
	if (n_chrs>1)  {
		cli::cli_progress_done()
		options(cli.progress_show_after = 2)
	}
	cli::cli_alert_success(stringr::str_c("Imputed BED made in ", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units="secs")))))
	
}



#' Get required genomic tools ready for use
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name prep_tools
#'
#' @noRd
prep_tools <- function(
	get_plink=FALSE,
	get_plink2=FALSE,
	get_bgen=FALSE,
	get_tabix=FALSE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	# check tools directory exists
	if (! dir.exists("~/_ukbrapr_tools/"))  system("mkdir ~/_ukbrapr_tools/")
	
	#
	# get Plink 1.9 (if not already available)
	if (get_plink)  {
		if (verbose) cli::cli_alert("Checking plink available")
		if (! file.exists("~/_ukbrapr_tools/plink"))  {
			if (verbose) cli::cli_alert("Unpacking plink")
			c1 <- "curl https://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20241022.zip > ~/_ukbrapr_tools/plink.zip"
			c2 <- "unzip ~/_ukbrapr_tools/plink.zip -d ~/_ukbrapr_tools/"
			if (very_verbose)  {
				system(c1)
				system(c2)
			} else {
				system(stringr::str_c(c1, " 2>/dev/null"))
				system(stringr::str_c(c2, " >/dev/null"))
			}
		}
	}

	#
	# get Plink 2 (if not already available)
	if (get_plink2)  {
		if (verbose) cli::cli_alert("Checking plink2 available")
		if (! file.exists("~/_ukbrapr_tools/plink2"))  {
			if (verbose) cli::cli_alert("Unpacking plink2")
			c1 <- "curl https://s3.amazonaws.com/plink2-assets/alpha6/plink2_linux_x86_64_20250122.zip > ~/_ukbrapr_tools/plink2.zip"
			c2 <- "unzip ~/_ukbrapr_tools/plink2.zip -d ~/_ukbrapr_tools/"
			if (very_verbose)  {
				system(c1)
				system(c2)
			} else {
				system(stringr::str_c(c1, " 2>/dev/null"))
				system(stringr::str_c(c2, " >/dev/null"))
			}
		}
	}
	
	#
	# get bgen (if not already available)
	if (get_bgen)  {
		if (verbose) cli::cli_alert("Checking bgenix available")
		if (! file.exists("~/_ukbrapr_tools/bgenix"))  {
			if (verbose) cli::cli_alert("Unpacking bgenix")
			c1 <- "curl https://www.chg.ox.ac.uk/~gav/resources/bgen_v1.1.4-Ubuntu16.04-x86_64.tgz > ~/_ukbrapr_tools/bgen.tgz"
			c2 <- "tar -xzf ~/_ukbrapr_tools/bgen.tgz --strip-components=1 -C ~/_ukbrapr_tools/"
			if (very_verbose)  {
				system(c1)
				system(stringr::str_replace(c2, "xzf", "xzvf"))
			} else {
				system(stringr::str_c(c1, " 2>/dev/null"))
				system(stringr::str_c(c2, " 2>/dev/null"))
			}
		}
	}
	
	#
	# install tabix (if not already installed)
	if (get_tabix)  {
		if (verbose) cli::cli_alert("Checking tabix installed")
		if ( ! suppressWarnings(system2("command", args = c("-v", "tabix"), stdout = FALSE)) == 0 )  {
			if (verbose) cli::cli_alert("Installing tabix")
			c1 <- "sudo apt-get update"
			c2 <- "sudo apt-get -y install tabix"
			if (very_verbose)  {
				system(c1)
				system(c2)
			} else {
				system(stringr::str_c(c1, " >/dev/null"))
				system(stringr::str_c(c2, " >/dev/null"))
			}
		}
	}
	
}


#' Format varlist. Check col names and rename to internal format
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name prep_varlist
#'
#' @noRd
prep_varlist <- function(
	varlist,
	doing_pgs=FALSE,
	need_pos=TRUE,
	verbose=FALSE
)  {
	
	if (verbose) cli::cli_alert("Checking variants list")
	
	# rename if not matching
	if (! "rsid" %in% colnames(varlist))  {
		if ("rsID" %in% colnames(varlist))  { # PGS CATALOG input
			varlist <- varlist |> dplyr::mutate(rsid=rsID)
		}  else if ("rs_id" %in% colnames(varlist))  { # GWAS CATALOG input
			varlist <- varlist |> dplyr::mutate(rsid=rs_id)
		}  else if ("ID" %in% colnames(varlist))  { # REGENIE input
			varlist <- varlist |> dplyr::mutate(rsid=ID)
		}  else if ("SNPID" %in% colnames(varlist))  { # SAIGE input
			varlist <- varlist |> dplyr::mutate(rsid=SNPID)
		}  else if ("MarkerID" %in% colnames(varlist))  { # SAIGE input
			varlist <- varlist |> dplyr::mutate(rsid=MarkerID)
		}  else if ("SNP" %in% colnames(varlist))  { # BOLT-LMM input
			varlist <- varlist |> dplyr::mutate(rsid=SNP)
		}  else  {
			cli::cli_abort("Input variants list file needs to contain `rsid` column")
		}
	}
	
	if (! "chr" %in% colnames(varlist))  {
		if ("chr_name" %in% colnames(varlist))  { # PGS CATALOG input
			varlist <- varlist |> dplyr::mutate(chr=chr_name)
		}  else if ("chromosome" %in% colnames(varlist))  { # GWAS CATALOG input
			varlist <- varlist |> dplyr::mutate(chr=chromosome)
		}  else if ("CHROM" %in% colnames(varlist))  { # REGENIE input
			varlist <- varlist |> dplyr::mutate(chr=CHROM)
		}  else if ("CHR" %in% colnames(varlist))  { # SAIGE/BOLT-LMM input
			varlist <- varlist |> dplyr::mutate(chr=CHR)
		}  else  {
			cli::cli_abort("Input variants list file needs to contain `chr` column")
		}
	}
	
	# don't need pos for imputed data so give an option
	if (need_pos | doing_pgs)  {
		if (! "pos" %in% colnames(varlist))  {
			if ("base_pair_location" %in% colnames(varlist))  { # GWAS CATALOG input
				varlist <- varlist |> dplyr::mutate(pos=base_pair_location)
			}  else if ("GENPOS" %in% colnames(varlist))  { # REGENIE input
				varlist <- varlist |> dplyr::mutate(pos=GENPOS)
			}  else if ("POS" %in% colnames(varlist))  { # SAIGE/BOLT-LMM input
				varlist <- varlist |> dplyr::mutate(pos=POS)
			}  else  {
				cli::cli_abort("Input variants list file needs to contain `pos` column")
			}
		}
	}  else  {
		varlist <- varlist |> dplyr::mutate(pos=1)
	}
	
	# if doing a PGS also need alleles and beta
	if (doing_pgs)  {
		
		if (! "effect_allele" %in% colnames(varlist))  {
			if ("ALLELE1" %in% colnames(varlist))  { # REGENIE/BOLT-LMM input
				varlist <- varlist |> dplyr::mutate(effect_allele=ALLELE1)
			}  else if ("Allele2" %in% colnames(varlist))  { # SAIGE input
				varlist <- varlist |> dplyr::mutate(effect_allele=Allele2)
			}  else  {
				cli::cli_abort("Input variants list file needs to contain `effect_allele` column")
			}
		}
		
		if (! "other_allele" %in% colnames(varlist))  {
			if ("ALLELE0" %in% colnames(varlist))  { # REGENIE/BOLT-LMM input
				varlist <- varlist |> dplyr::mutate(other_allele=ALLELE0)
			}  else if ("Allele1" %in% colnames(varlist))  { # SAIGE input
				varlist <- varlist |> dplyr::mutate(other_allele=Allele1)
			}  else  {
				cli::cli_abort("Input variants list file needs to contain `other_allele` column")
			}
		}
		
		if (! "beta" %in% colnames(varlist))  {
			if ("effect_weight" %in% colnames(varlist))  { # PGS catalog input
				varlist <- varlist |> dplyr::mutate(beta=effect_weight)
			}  else if ("BETA" %in% colnames(varlist))  { 
				varlist <- varlist |> dplyr::mutate(beta=BETA)
			}  else  {
				cli::cli_abort("Input variants list file needs to contain `beta` column")
			}
		}
		
		# if beta is negative, swap effect and other allele 
		varlist <- varlist |> 
			dplyr::rename(
				effect_allele_tmp=effect_allele,
				other_allele_tmp=other_allele
			) |> 
			dplyr::mutate(
				effect_allele=dplyr::if_else(beta<0, other_allele_tmp, effect_allele_tmp),
				other_allele=dplyr::if_else(beta<0, effect_allele_tmp, other_allele_tmp),
				beta=abs(beta),
			)
		
		varlist <- varlist |> dplyr::select(rsid, chr, pos, effect_allele, other_allele, beta)
		
	}  else  {
		
		varlist <- varlist |> dplyr::select(rsid, chr, pos)
		
	}
	
	# order by CHR and make sure unique rows
	varlist <- varlist |>
		dplyr::arrange(chr, pos) |>
		dplyr::distinct()
	
	# return
	return(varlist)
	
}





#' Extract variants from DRAGEN pVCF file(s) into single BED file 
#'
#' @description For a given set of genomic coordinates extract the UK Biobank WGS DRAGEN variant calls (spread across many pVCFs) into a single BED file.
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_dragen_bed_from_pvcfs
#'
#' @param in_file A data frame or file path. Contains at least two columns: `chr` and `pos` (in build 38). Other columns are ignored.
#' @param out_bed A string. 
#' @param progress Logical. Show progress through each individual file,
#'        \code{default=TRUE}
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' make_dragen_bed_from_pvcfs(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_bed="liver_cirrhosis.dragen.variants")
#'
make_dragen_bed_from_pvcfs <- function(
	in_file,
	out_bed,
	progress=TRUE,
	verbose=FALSE,
	very_verbose=FALSE
)  {
	
	# required files
	file_dragen <- system.file("files", "dragen_pvcf_coordinates.csv.gz", package="ukbrapR")
	
	# start
	start_time <- Sys.time()
	
	#
	#
	# check inputs
	if (very_verbose)  verbose <- TRUE
	if (verbose) cli::cli_alert("Checking inputs")
	
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
			"x" = "You've supplied a {.cls {class(in_file)}} vector."
		))
		
	} else {
		varlist <- in_file   # user has passed a data frame
	}
	
	# check varlist formatting
	varlist$rsid <- ""
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, verbose=verbose)
	varlist <- varlist |> dplyr::mutate(filename="")
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	
	
	#
	#
	# load DRAGEN coordinate file
	if (verbose) cli::cli_alert("Load DRAGEN coordinates file")
	dragen <- readr::read_csv(file_dragen, progress=FALSE, show_col_types=FALSE)
	dragen <- dragen |> 
		dplyr::mutate(chromosome = stringr::str_remove_all(chromosome, "chr")) |>
		dplyr::filter(chromosome %in% unique(varlist$chr)) |>
		dplyr::arrange(chromosome, starting_position)
	
	# for each variant get file name
	if (verbose) cli::cli_alert("For each variant identify the corresponding DRAGEN pVCF file")
	for (ii in 1:nrow(varlist))  {
		dragen_sub <- dragen |> 
			dplyr::filter(chromosome == varlist$chr[ii] & starting_position < varlist$pos[ii]) |>
			tail(n=1)
		varlist$filename[ii] <- dragen_sub$filename[1]
	}
	
	#
	# get tabix and plink 1.9
	ukbrapR:::prep_tools(get_plink=TRUE, get_tabix=TRUE, verbose=verbose, very_verbose=very_verbose)
	
	#
	#
	# for each VCF file
	fls <- unique(varlist$filename)
	n_files <- length(fls)
	
	# show progress
	cli::cli_alert("Extracting {nrow(varlist)} variant{?s} from {n_files} DRAGEN pVCF file{?s} (ETA {prettyunits::pretty_sec(n_files*60)})")
	
	# loop over files...
	for (ii in 1:n_files)  {
		
		# this file name
		fl <- fls[ii]
		fl_time <- Sys.time()
		
		# get variants list for that file
		varlist_sub <- varlist |> 
			dplyr::filter(filename==fl)
		
		# get CHR
		chr <- varlist_sub$chr[1]
		
		# path to VCF
		vcf_path <- stringr::str_c("/mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ pVCF\\ format\\ \\[500k\\ release\\]/chr", chr, "/", fl, " ")
		#if (verbose) cli::cli_alert(stringr::str_c("Path to pVCF: ", vcf_path))
		
		# create empty VCF file to fill
		system("echo '##fileformat=VCFv4.2' > _ukbrapr_tmp.vcf")
		system(stringr::str_c("zgrep -m 1 '#CHROM' ", vcf_path, " >> _ukbrapr_tmp.vcf 2>/dev/null"))
		
		# use tabix to extract the positions
		if (verbose) cli::cli_alert("Use tabix to extract the positions")
		system(stringr::str_c(
			"tabix ",
			vcf_path,
			stringr::str_c("chr", chr, ":", varlist_sub$pos-1, "-", varlist_sub$pos, collapse=" "),
			" >> _ukbrapr_tmp.vcf"
		))
		
		# subset to just those in the input file - sometimes the grep catches extras due to the tabix requirement of POS-1:POS!
		system("head -n2 _ukbrapr_tmp.vcf > _ukbrapr_tmp2.vcf")
		system(stringr::str_c(
			"awk -v list=\"", 
			stringr::str_c(varlist_sub$pos, collapse=","),
			"\" 'BEGIN { split(list, nums, \",\"); for (i in nums) integers[nums[i]] } $2 in integers' _ukbrapr_tmp.vcf >> _ukbrapr_tmp2.vcf"
		))
		system("mv _ukbrapr_tmp2.vcf _ukbrapr_tmp.vcf")
		
		# does the VCF actually contain variants? -- check file length 
		n_rows <- as.integer(system("wc -l < _ukbrapr_tmp.vcf", intern = TRUE))
		
		# if no variants in the VCF (nrow of list file <=2) then skip this FILE
		if (n_rows > 2)  {
			
			# use Plink to convert
			if (verbose) cli::cli_alert("Use plink to convert pVCF to BED")
			c1 <-"~/_ukbrapr_tools/plink --vcf _ukbrapr_tmp.vcf --set-missing-var-ids @:#:\\$1:\\$2 --make-bed --out _ukbrapr_tmp"
			if (very_verbose)  {
				system(c1)
			} else {
				system(stringr::str_c(c1, " >/dev/null"))
			}
			
			# did it work?
			if (! file.exists("_ukbrapr_tmp.bed"))  cli::cli_abort("Plink failed to convert the VCF to BED. Try with `very_verbose=TRUE` to see terminal output.")
			
			# if this is the first one, simply rename
			if (ii==1)  {
				system(stringr::str_c("mv _ukbrapr_tmp.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp.fam ", out_bed, ".fam"))
			}
			
			# if not the first one, use plink to merge beds
			if (ii>1)  {
				if (verbose) cli::cli_alert("Merge BEDs")
				c1 <- stringr::str_c("~/_ukbrapr_tools/plink --bfile ", out_bed, " --bmerge _ukbrapr_tmp --make-bed --out _ukbrapr_tmp2")
				if (very_verbose)  {
					system(c1)
				} else {
					system(stringr::str_c(c1, " >/dev/null"))
				}  
				system(stringr::str_c("mv _ukbrapr_tmp2.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp2.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp2.fam ", out_bed, ".fam"))
			}
		
		} else {
			cli::cli_warn(stringr::str_c("Variants in chr", chr, "/", fl, " are in the input varlist but are missing from the pVCF"))
		}

		# remove tmp files
		system("rm _ukbrapr_tmp*")
		
		# give update
		if (progress)  cli::cli_alert_info(stringr::str_c("Extracted from pVCF ", ii, " of ", n_files, " [", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), fl_time, units="secs"))), "]"))
		
	}
	
	# finished
	cli::cli_alert_success(stringr::str_c("DRAGEN BED made in ", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units="secs")))))
	
}





