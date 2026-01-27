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
	
	# if includes column "ukb_rsid" then use this over user-provided rsid
	if ("ukb_rsid" %in% colnames(varlist))  {
		varlist <- varlist |> dplyr::mutate(rsid=ukb_rsid)
		cli::cli_alert_info("Using `ukb_rsid` column for RSIDs over user-provided `rsid` column")
	}

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
