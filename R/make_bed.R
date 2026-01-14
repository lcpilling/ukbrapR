#' Extract variants from DRAGEN BGEN file(s) into single BED file 
#'
#' @description For a given set of genomic coordinates extract the UK Biobank WGS DRAGEN variant calls (from the BGEN format, field 24309) into a single BED file.
#'
#' This assumes your project has access to the WGS BGEN files released April 2025. If not, run `ukbrapR:::make_dragen_bed_from_pvcfs()` to use [tabix] and [plink] to subset the [DRAGEN WGS pVCF files].
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_dragen_bed
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
#' make_dragen_bed(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_bed="liver_cirrhosis.dragen.variants")
#'
#' @export
#'
make_dragen_bed <- function(
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
	varlist$rsid <- ""
	varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, verbose=verbose)
	
	# check output format 
	if (! class(out_bed)=="character")  cli::cli_abort("Output file prefix needs to be a character string")
	if (length(out_bed)>1)  cli::cli_abort("Output file prefix needs to be length 1")
	
	#
	#
	# get bgen, plink 1.9 and plink 2
	ukbrapR:::prep_tools(get_plink=TRUE, get_plink2=TRUE, get_bgen=TRUE, verbose=verbose, very_verbose=very_verbose)
	
	#
	#
	# for each CHR 
	chrs <- unique(varlist$chr)
	n_chrs <- length(chrs)
	n_snps_per_chr <- nrow(varlist) / n_chrs
	n_secs <- (n_chrs * 10) * (0.1 * n_snps_per_chr)
	
	# show progress
	cli::cli_alert("Extracting {nrow(varlist)} variant{?s} from {n_chrs} DRAGEN BGEN file{?s} (ETA ~{prettyunits::pretty_sec(n_secs)})")
	
	# loop over files...
	for (ii in 1:n_chrs)  {
		
		# this CHR
		chr <- chrs[ii]
		chr_time <- Sys.time()
		
		# get variants list for this file
		varlist_sub <- varlist |> dplyr::filter(chr==!!chr) |> dplyr::mutate(bed_range=stringr::str_c(chr, ":", pos, "-", pos))
		readr::write_tsv(dplyr::select(varlist_sub, bed_range), "_ukbrapr_tmp_range.txt", col_names = FALSE, progress = FALSE)
		
		# path to BGEN
		bgen_path <- stringr::str_c("/mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ BGEN\\ format\\ \\[500k\\ release\\]/ukb24309_c", chr, "_b0_v1.bgen")
		
		# check it exists - exit if not 
		if (! file.exists(stringr::str_replace_all(bgen_path, stringr::fixed("\\"), "")) )  {
			cli::cli_abort(c(
				stringr::str_c("DRAGEN BGEN file not found: ukb24309_c", chr, "_b0_v1.bgen"), 
				"Has your Project been updated since April 2025? If not, you probably don't have the new BGENs.",
				"Consider using `ukbrapR:::make_dragen_bed_from_pvcfs()`"
			))
		}
		
		# use bgenix to extract subset of BGEN
		if (verbose) cli::cli_alert(stringr::str_c("Using bgenix to extract the positions from chr", chr))
		c1 <- stringr::str_c("~/_ukbrapr_tools/bgenix -g ", bgen_path, " -incl-range _ukbrapr_tmp_range.txt > _ukbrapr_tmp.bgen")
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
			c1 <- stringr::str_c("~/_ukbrapr_tools/plink2 --bgen _ukbrapr_tmp.bgen ref-first --sample /mnt/project/Bulk/DRAGEN\\ WGS/DRAGEN\\ population\\ level\\ WGS\\ variants\\,\\ BGEN\\ format\\ \\[500k\\ release\\]/ukb24309_c", chr, "_b0_v1.sample --make-bed --out _ukbrapr_tmp")
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
			
			# if not the first one, use plink2 to merge beds
			if (ii>1)  {
				if (verbose) cli::cli_alert("Merge BEDs")
				c1 <- stringr::str_c("~/_ukbrapr_tools/plink --bfile ", out_bed, " --bmerge _ukbrapr_tmp --make-bed --out _ukbrapr_tmp2")
				if (very_verbose)  {
					system(c1)
				} else {
					system(stringr::str_c(c1, " >/dev/null 2>/dev/null"))
				}  
				system(stringr::str_c("mv _ukbrapr_tmp2.bed ", out_bed, ".bed"))
				system(stringr::str_c("mv _ukbrapr_tmp2.bim ", out_bed, ".bim"))
				system(stringr::str_c("mv _ukbrapr_tmp2.fam ", out_bed, ".fam"))
			}
		
		} else {
			cli::cli_warn(stringr::str_c("Variants on CHR ", chr, " are in the input varlist but are missing from DRAGEN BGEN"))
		}
		
		# remove tmp files
		system("rm _ukbrapr_tmp*")
		
		# give update
		if (progress)  cli::cli_alert_info(stringr::str_c("Extracted from DRAGEN BGEN chr", chr, " (", ii, " of ", n_chrs, ") [", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), chr_time, units="secs"))), "]"))
		
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
#' @description For a given set of identifiers (RSID or "build37 position") extract from the UK Biobank imputed genotypes (BGEN files, field 22828) into a single BED file.
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_imputed_bed
#'
#' @param in_file A data frame or file path. Contains at least two columns: (`rsID` and `CHR`) OR (`CHR` and `POS`). Other columns are ignored.
#' @param out_bed A string. Output BED file prefix.
#' @param progress Logical. Show progress through each individual file,
#'        \code{default=TRUE}
#' @param use_pos Logical. Use genomic position (CHR and POS) instead of RSID,
#'        \code{default=FALSE}
#' @param verbose Logical. Be verbose (show individual steps),
#'        \code{default=FALSE}
#' @param very_verbose Logical. Be very verbose (show individual steps & show terminal output from Plink etc),
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # Example list of variants
#' #    only RSID and CHR are provided
#' varlist <- data.frame(rsid=c("rs1800562","rs429358"), chr=c(6,19))
#' 
#' make_imputed_bed(in_file=varlist, out_bed="example_variants_by_rsid")
#' 
#' # Above example works fine if the imputed BGEN contains the provided rsid
#' 
#' # However if you have a list of variants defined by genomic position (CHR and POS)
#' # then use the `use_pos=TRUE` argument
#' 
#' # Example list of variants using position (build 37) not rsid
#' varlist_pos <- data.frame(chr=c(6,19), pos=c(26093141,45411941))
#' 
#' # if you provide an rsid it is ignored when `use_pos=TRUE`
#' make_imputed_bed(in_file=varlist_pos, out_bed="example_variants_by_pos", use_pos=TRUE)
#'
#' @export
#'
make_imputed_bed <- function(
    in_file,
    out_bed,
    use_pos=FALSE,
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
  if (use_pos)  {
    varlist$rsid <- ""
    varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, need_pos=TRUE, verbose=verbose)
  } else {
    varlist <- ukbrapR:::prep_varlist(varlist, doing_pgs=FALSE, need_pos=FALSE, verbose=verbose)
  }
  
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
  n_snps_per_chr <- nrow(varlist) / n_chrs
  n_secs <- (n_chrs * 8) * (0.1 * n_snps_per_chr)
  
  # show progress
  cli::cli_alert("Extracting {nrow(varlist)} variant{?s} from {n_chrs} imputed BGEN file{?s} (ETA {prettyunits::pretty_sec(n_secs)})")
  
  # loop over files...
  for (ii in 1:n_chrs)  {
    
    # this CHR
    chr <- chrs[ii]
    chr_time <- Sys.time()
    
    # get variants list for this file
    if (use_pos)  {
      this_chr <- chr
      if (chr < 10)  this_chr <- stringr::str_c("0", chr)   # imputed BGENs have 0 prefix to chrs <10
      varlist_sub <- varlist |> dplyr::filter(chr==!!chr) |> dplyr::mutate(bed_range=stringr::str_c(this_chr, ":", pos, "-", pos))
      readr::write_tsv(dplyr::select(varlist_sub, bed_range), "_ukbrapr_tmp_range.txt", col_names = FALSE, progress = FALSE)
    } else {
      varlist_sub <- varlist |> dplyr::filter(chr==!!chr)
      readr::write_tsv(dplyr::select(varlist_sub, rsid), "_ukbrapr_tmp_rsids.txt", col_names = FALSE, progress = FALSE)
    }
    
    # path to BGEN
    bgen_path <- stringr::str_c("/mnt/project/Bulk/Imputation/UKB\\ imputation\\ from\\ genotype/ukb22828_c", chr, "_b0_v3.bgen")
    
    # use bgenix to extract subset of BGEN
    if (verbose) cli::cli_alert("Use bgenix to extract the positions")
    if (use_pos)  {
      c1 <- stringr::str_c("~/_ukbrapr_tools/bgenix -g ", bgen_path, " -incl-range _ukbrapr_tmp_range.txt > _ukbrapr_tmp.bgen")
    } else {
      c1 <- stringr::str_c("~/_ukbrapr_tools/bgenix -g ", bgen_path, " -incl-rsids _ukbrapr_tmp_rsids.txt > _ukbrapr_tmp.bgen")
    }
    if (very_verbose)  {
      system(c1)
    } else {
      system(stringr::str_c(c1, " 2>/dev/null"))
    }
    
    # did it work?
    if (! file.exists("_ukbrapr_tmp.bgen"))  cli::cli_abort("BGENIX failed to extract from the UKB imputed BGEN. Try with `very_verbose=TRUE` to see terminal output.")
    
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
      if (! file.exists("_ukbrapr_tmp.bed"))  cli::cli_abort("plink2 failed to convert the imputed BGEN to BED. Try with `very_verbose=TRUE` to see terminal output.")
      
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
    if (progress)  cli::cli_alert_info(stringr::str_c("Extracted from imputed BGEN ", ii, " of ", n_chrs, " [", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), chr_time, units="secs"))), "]"))
    
  }
  
  # finished
  if (n_chrs>1)  {
    cli::cli_progress_done()
    options(cli.progress_show_after = 2)
  }
  cli::cli_alert_success(stringr::str_c("Imputed BED made in ", prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units="secs")))))
  
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
#' @noRd
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



#' Extract variants from DRAGEN PGEN file(s) into single BED file 
#'
#' @description For a given set of genomic coordinates extract the UK Biobank WGS DRAGEN variant calls (from the PLINK format PGEN files, field 24308) into a single BED file.
#'
#' This assumes your project has access to the WGS PGEN files released April 2025. By running `ukbrapR:::make_dragen_bed_from_pvcfs()` this will use [tabix](https://www.htslib.org/doc/tabix.html) and [plink](https://www.cog-genomics.org/plink/) to subset the [DRAGEN WGS pVCF files](https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=24310). This requires "pos" in the input data frame (build 38).
#'
#' Plink2 requires up to 10Gb of reserved RAM (depending on CHR), and takes up to 40 seconds per CHR.
#'
#' @return A single merged BED file (and BIM and FAM files)
#'
#' @author Luke Pilling
#'
#' @name make_dragen_bed_from_pgen
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
#' make_dragen_bed_from_pgen(in_file=system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"), out_bed="liver_cirrhosis.dragen.variants")
#'
#' @noRd
#'
make_dragen_bed_from_pgen <- function(
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
			system(stringr::str_c(c1, " >/dev/null 2>/dev/null"))
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
					system(stringr::str_c(c1, " >/dev/null 2>/dev/null"))
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
