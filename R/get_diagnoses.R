#' Get UK Biobank participant diagnosis data 
#'
#' @description For a list of diagnostic codes get the HES, GP, cancer registry, operations, and self-reported illness data, matching the provided codes.
#'
#' Valid code vocabularies are:
#'
#'  - ICD10 (for `hesin`, `death_cause` and `cancer_registry` searches) - fuzzy matching
#'
#'  - ICD9 (for `hesin` searches) - fuzzy matching
#'
#'  - Read2 / CTV3 (for `gp_clinical`) - exact matches on first 5 characters
#'
#'  - OPCS3 / OPCS4 (for `hesin_oper`) - fuzzy matching
#'
#'  - ukb_cancer / ukb_noncancer (for self-reported illness at UK Biobank assessments - all available will be searched) - exact matches
#'
#' This function relies on exported raw data files and thus does not need to be run in a Spark cluster. If the files are not in the default locations for the package you will need to specify the  `file_paths` to exported tables. Recommend to run `export_tables()` once in your project to export the tables to the default paths for the package.
#'
#' @return Returns a list of data frames (the participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, `hesin_oper`, `gp_clinical`, `cancer_registry` and `selfrep_illness`. Also includes the original codes list)
#'
#' @author Luke Pilling
#'
#' @name get_diagnoses
#'
#' @param codes_df A data frame. Contains two columns: `code` and `vocab_id` i.e., a list of diagnostic codes, and an indicator of the vocabulary (ICD10, Read2, CTV3, OPCS3, OPCS4, ukb_cancer, and ukb_noncancer are recognised). Other columns are ignored.
#' @param file_paths A data frame. Columns must be `object` and `path` containing paths to required files. Default assumes you have the tables exported in the RAP environment from
#'        ukbrapR::export_tables() 
#'        \code{default=ukbrapR:::ukbrapr_paths}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for CKD from GEMINI multimorbidity project
#' codes_df_ckd <- ukbrapR:::codes_df_ckd
#' head(codes_df_ckd)
#'
#' # Get diagnosis data - returns list of data frames (one per source)
#' # -- Requires exported tables - see `export_tables()` 
#' diagnosis_list <- get_diagnoses(codes_df_ckd)
#'
#' # don't forget to save and upload data to RAP persistent storage!
#' save(diagnosis_list, "ukbrap.CKD.emr.20231114.RDat")
#' upload_to_rap(file="ukbrap.CKD.*", dir="")
#'
#' @export
#'
get_diagnoses <- function(
	codes_df,
	file_paths = NULL,
	verbose = FALSE
)  {
	
	v <- packageVersion("ukbrapR")
	cli::cli_alert_info("ukbrapR v{v}")
	
	start_time <- Sys.time()
	
	vocab_col = "vocab_id"
	codes_col = "code"
	
	# Check input
	if (verbose) cli::cli_alert("Checking inputs (codes, file paths, etc)")
	if (! any(class(codes_df) %in% c("data.frame","tbl","tbl_df")))  {
		cli::cli_abort(c(
			"{.var codes_df} must be a data.frame or tibble",
			"x" = "You've supplied a {.cls {class(codes_df)}} vector."
		))
	}
	codes_df = as.data.frame(codes_df)  # in case a tibble
	
	if (! vocab_col %in% colnames(codes_df))  stop("Codelist data frame needs to include vocabulary column `vocab_id`")
	if (! codes_col %in% colnames(codes_df))  stop("Codelist data frame needs to include codes column `code`")
	
	if (! any(c("ICD10","ICD9","Read2","CTV3","OPCS3","OPCS4","ukb_cancer","ukb_noncancer") %in% codes_df[,vocab_col]))  stop("Vocabularies need to include at least one of ICD10, ICD9, Read2, CTV3, OPCS3, OPCS4, ukb_cancer, or ukb_noncancer")
	
	# Is this one of my systems? If so, get the internal file_paths 
	nodename <- as.character(Sys.info()['nodename'])
	if ( is.null(file_paths)  &  nodename %in% c("SNOW","SHAPTER") )  {
		file_paths = ukbrapR:::snow_paths
		if (verbose)  cli::cli_alert_info("Identified server {nodename} - using predefined paths.")
	}
	if ( is.null(file_paths)  &  nodename == "indy.ex.ac.uk" )  {
		file_paths = ukbrapR:::indy_paths
		if (verbose)  cli::cli_alert_info("Identified server {nodename} - using predefined paths.")
	}
	
	# if file_paths not provided assume default paths
	if (is.null(file_paths))  file_paths = ukbrapR:::ukbrapr_paths
	
	
	#########################################################################################################
	#
	# check codes provided, determine what datasets we are going to search
	
	# Check code lists - only first 5 digits are used by UK Biobank
	cli::cli_alert("Checking provided codes (remember only the first 5 characters are used)")
	get_icd10   <- FALSE
	get_icd9    <- FALSE
	get_canreg  <- FALSE
	get_gp      <- FALSE
	get_oper    <- FALSE
	get_selfrep <- FALSE
	ICD10s      <- ""
	ICD9s       <- ""
	Read2s      <- ""
	CTV3s       <- ""
	OPCS3s      <- ""
	OPCS4s      <- ""
	
	# throw error if any provided code has length 0
	if (any(stringr::str_length(codes_df[,codes_col]) == 0))  {
		cli::cli_abort("Blank code provided. Check your input codes lists to avoid unexpected matches.")
	}
	
	# warn if any provided code has length 1
	if (any(stringr::str_length(codes_df[,codes_col]) == 1))  {
		cli::cli_warn("Some provided code(s) have length 1. Check your input codes lists and matched outputs to avoid unexpected matches.")
	}
	
	# function to check for hyphens and abort if any provided 
	# (suggests they want a range of codes. Safer to abort at ask the user to explicitly provide the codes to search for)
	hyphen_check <- function(codes, vocab)  {
		if (any(stringr::str_detect(codes, "-")))  {
			codes = codes[ stringr::str_detect(codes, "-") ]
			cli::cli_abort(c(
				"{vocab} codes cannot contain hyphens, as this suggests you want a range of codes.",
				"x" = "Remove hyphens and provide the specific {vocab} codes you wish to ascertain (safer)."
			))
		}
	}
	
	# get ICD10s. Remove "." dot characters. First 5 characters only.
	if (any(codes_df[,vocab_col] == "ICD10"))  {
		get_icd10 <- TRUE
		ICD10s    <- codes_df |>
			dplyr::filter(!!rlang::sym(vocab_col) == "ICD10") |>
			dplyr::select(!!rlang::sym(codes_col)) |>
			dplyr::pull() |>
			unique() |>
			stringr::str_remove(stringr::fixed(".")) |> 
			stringr::str_sub(1, 5)
		cat(" - N unique ICD10 codes:", length(ICD10s), "\n")
		hyphen_check(ICD10s, "ICD10")
		if (any(stringr::str_starts(ICD10s, "C")))  get_canreg <- TRUE
	}
	
	# get ICD9s. Remove "." dot characters. First 5 characters only.
	if (any(codes_df[,vocab_col] == "ICD9"))  {
		get_icd9 <- TRUE
		ICD9s    <- codes_df |>
			dplyr::filter(!!rlang::sym(vocab_col) == "ICD9") |>
			dplyr::select(!!rlang::sym(codes_col)) |>
			dplyr::pull() |>
			unique() |>
			stringr::str_remove(stringr::fixed(".")) |> 
			stringr::str_sub(1, 5)
		hyphen_check(ICD9s, "ICD9")
		cat(" - N unique ICD9 codes:", length(ICD9s), "\n")
	}
	
	# get Read2 and CTV3s. First 5 characters only. 
	gp_codes = NULL
	if (any(codes_df[,vocab_col] == "Read2"))  {
		get_gp    <- TRUE
		Read2s    <- codes_df[ codes_df[,vocab_col] == "Read2" , codes_col ]
		Read2s    <- stringr::str_sub(Read2s, 1, 5) |> unique()
		gp_codes  <- Read2s
		cat(" - N unique Read2 codes:", length(Read2s), "\n")
		hyphen_check(Read2s, "Read2")
		# any <5 characters?
		nchar_Read2s = nchar(Read2s)
		nchar_Read2s_n5 = length(nchar_Read2s[ nchar_Read2s < 5 ])
		if (nchar_Read2s_n5 > 0)  {
			cli::cli_abort(c(
				"i" = "Read2 codes must be at least 5 characters in length",
				"x" = "There {?is/are} {nchar_Read2s_n5} Read2 code{?s} < 5 characters."
			))
		}
	}
	if (any(codes_df[,vocab_col] == "CTV3"))  {
		get_gp   <- TRUE
		CTV3s    <- codes_df[ codes_df[,vocab_col] == "CTV3" , codes_col ]
		CTV3s    <- stringr::str_sub(CTV3s, 1, 5) |> unique()
		if (is.null(gp_codes))  {
			gp_codes <- CTV3s
		} else {
			gp_codes <- c(gp_codes, CTV3s)
		}
		cat(" - N unique CTV3 codes:", length(CTV3s), "\n")
		hyphen_check(CTV3s, "CTV3")
		# any <5 characters?
		nchar_CTV3s = nchar(CTV3s)
		nchar_CTV3s_n5 = length(nchar_CTV3s[ nchar_CTV3s < 5 ])
		if (nchar_CTV3s_n5 > 0)  {
			cli::cli_abort(c(
				"i" = "CTV3 codes must be at least 5 characters in length",
				"x" = "There {?is/are} {nchar_CTV3s_n5} CTV3 code{?s} < 5 characters.",
			))
		}
	}
	
	# get OPCS codes? Remove "." dot characters. First 5 characters only.
	oper_codes = NULL
	if (any(codes_df[,vocab_col] == "OPCS3"))  {
		get_oper <- TRUE
		OPCS3s   <- codes_df |>
			dplyr::filter(!!rlang::sym(vocab_col) == "OPCS3") |>
			dplyr::select(!!rlang::sym(codes_col)) |>
			dplyr::pull() |>
			unique() |>
			stringr::str_remove(stringr::fixed(".")) |> 
			stringr::str_sub(1, 5)
		cat(" - N unique OPCS3 codes:", length(OPCS3s), "\n")
		hyphen_check(OPCS3s, "OPCS3")
		oper_codes = OPCS3s
	}
	if (any(codes_df[,vocab_col] == "OPCS4"))  {
		get_oper <- TRUE
		OPCS4s   <- codes_df |>
			dplyr::filter(!!rlang::sym(vocab_col) == "OPCS4") |>
			dplyr::select(!!rlang::sym(codes_col)) |>
			dplyr::pull() |>
			unique() |>
			stringr::str_remove(stringr::fixed(".")) |> 
			stringr::str_sub(1, 5)
		cat(" - N unique OPCS4 codes:", length(OPCS4s), "\n")
		hyphen_check(OPCS4s, "OPCS4")
		oper_codes = c(oper_codes, OPCS4s)
	}
	
	# check for self-reported codes
	n_selfrep = length(unique(codes_df[codes_df[,vocab_col] %in% c("ukb_cancer","ukb_noncancer"),codes_col]))
	if (n_selfrep>0)  {
		get_selfrep <- TRUE
		cat(" - N unique UKB-self-reported codes:", n_selfrep, "\n")
	}
	

	#########################################################################################################
	#
	# check data is available, download if required 
	
	# Check file paths are provided
	if (is.null(file_paths))  cli::cli_abort("Need to provide {.var file_paths}")
	if (! any(class(file_paths) %in% c("data.frame","tbl","tbl_df")))  {
		cli::cli_abort(c(
			"{.var file_paths} must be a data.frame or tibble",
			"x" = "You've supplied a {.cls {class(file_paths)}} vector."
		))
	}
	if (colnames(file_paths)[1] != "object" | colnames(file_paths)[2] != "path")  cli::cli_abort("{.var file_paths} needs two columns: `object` and `path`")
	
	# check all the required files are included
	must_include = "baseline_dates"
	if (get_icd10)    must_include <- c(must_include, c("death","death_cause","hesin","hesin_diag"))
	if (get_icd9)     must_include <- c(must_include, c("hesin","hesin_diag"))
	if (get_canreg)   must_include <- c(must_include, c("cancer_registry"))
	if (get_gp)       must_include <- c(must_include, c("gp_clinical"))
	if (get_oper)     must_include <- c(must_include, c("hesin_oper"))
	if (get_selfrep)  must_include <- c(must_include, c("selfrep_illness"))
	must_include = unique(must_include)
	
	for (file in must_include)  if (! file %in% file_paths$object) cli::cli_abort("{.var file_paths} must contain {.path {file}}")
	
	# if files not already downloaded from RAP then copy to user's home directory
	#   only do this if the file paths are to "ukbrapr_data"
	files = file_paths$path[file_paths$object %in% must_include]
	if (stringr::str_detect(files[1], "ukbrapr_data"))  {
		
		# get path to users home directory - create ukbrapr_data directory on worker (if already exists then nothing happens)
		home_path = as.character(Sys.getenv()["HOME"])
		dir.create(stringr::str_c(home_path, "/ukbrapr_data"), showWarnings = FALSE)
		
		# do any need downloading from RAP? Or already been done?
		#     each file now has two paths: a RAP path and a local path:
		dx_files = NULL
		for (file in stringr::str_c(home_path, "/", files))  if (! file.exists(file))  dx_files = c(dx_files, file)
		
		# if any were missing, download them
		if (!is.null(dx_files))  {
			
			options(cli.progress_show_after = 0)
			#cli::cli_progress_bar("Downloading files from the RAP", total = length(files))
			cli::cli_progress_bar(format = "Downloading {.path {basename(file)}} from the RAP [{cli::pb_current}/{cli::pb_total}] {cli::pb_bar} {cli::pb_percent}", total = length(dx_files))
			
			# move to RAP directory
			dx_pwd = system("dx pwd", intern=TRUE)
			if (! stringr::str_detect(dx_pwd, "ukbrapr_data"))  system("dx cd ukbrapr_data")
			
			# copy file from RAP space to instance
			for (file in dx_files)  {
				#system(stringr::str_c("cp /mnt/project/", file, " ~/ukbrapr_data/"))
				cli::cli_progress_update()
				system(stringr::str_c("dx download \"", basename(file), "\" -o \"", home_path, "/ukbrapr_data\""))
			}
			cli::cli_progress_done()
			options(cli.progress_show_after = 2)
			
			if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
			
		}
		
		# add home directory prefix to file paths
		file_paths$path = stringr::str_c(home_path, "/", file_paths$path)
		
	}
	
	# check files exist
	for (file in file_paths$path[file_paths$object %in% must_include])  if (! file.exists(file))  cli::cli_abort("Could not find file {.path {file}}")
	
	# assume OS in UNIX... check if Windows -- i.e., should we use `grep` or `findstr`
	unix = TRUE
	if (as.character(Sys.info()['sysname'])=="Windows")  {
		unix = FALSE
		# paths need converting from unix to dos with escape characters?
		file_paths = file_paths |> dplyr::mutate(path = stringr::str_replace_all(path, "/", "\\\\"))
	}

	#
	#########################################################################################################
	#
	
	# Get data for each code vocabulary
	if (verbose) cli::cli_alert("Ascertaining codes from long EMR files")
	death_cause_tbl     <- NULL  # ICD10
	hesin_diag_tbl      <- NULL  # ICD10, ICD9
	cancer_registry_tbl <- NULL  # ICD10
	gp_clinical_tbl     <- NULL  # Read2 / CTV3
	hesin_oper_tbl      <- NULL  # OPCS3 / OPCS4
	selfrep_illness_tbl <- NULL  # ukb_cancer / ukb_noncancer
	
	if (get_icd10)  {
		
		#
		# death data ###########################################
		#
		
		cli::cli_alert("Ascertaining cause of death data.")
		
		death_cause_path = file_paths$path[ file_paths$object=="death_cause" ]
		
		# create search string
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(ICD10s, collapse = "|")), " ", sprintf('%s', death_cause_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(ICD10s, collapse = "\" /c:\""), "\" ", sprintf('"%s"', death_cause_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(death_cause_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		if (! "eid" %in% headers)  headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		death_cause_tbl <- suppressWarnings(readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE))
		
		# if any matches returned, make sure eid is formatted nicely (remove file name) and the dates are dates
		if (nrow(death_cause_tbl)>0)  {
			if (!unix)  {
				death_cause_tbl <- death_cause_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(death_cause_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with date of death data
			death_tbl = suppressWarnings(readr::read_tsv(file_paths$path[ file_paths$object=="death" ], show_col_types=FALSE, progress=FALSE))
			if (! "eid" %in% colnames(death_tbl))  colnames(death_tbl)[1] <- "eid"
			death_cause_tbl = dplyr::inner_join(death_tbl, death_cause_tbl, by=c("eid"="eid", "ins_index"="ins_index"))
			
			# format date col if not "Date"
			if (!lubridate::is.Date(death_cause_tbl$date_of_death))  death_cause_tbl <- death_cause_tbl |> dplyr::mutate(date_of_death = lubridate::dmy(date_of_death))
		}
		
		cli::cli_alert_success("Loaded {.var death_cause} with {nrow(death_cause_tbl)} matched rows.")
		
		if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
		
		#
		# cancer registry ####################################
		#
		
		# do any ICD10s start with a C? Skip if not.
		if (get_canreg)  {
			
			cli::cli_alert("Ascertaining cancer registry data.")
			
			# load data 
			cancer_registry_dat <- suppressWarnings(readr::read_tsv(file_paths$path[ file_paths$object=="cancer_registry" ], show_col_types = FALSE, progress = FALSE))
			
			# get cancer registry data for these ICD10s
			cancer_registry_tbl <- ukbrapR:::get_cancer_registry(codes = ICD10s, ukb_dat = cancer_registry_dat, verbose = verbose)
			cli::cli_alert_success("Loaded {.var cancer_registry} with {nrow(cancer_registry_tbl)} matched rows.")
			
			rm(cancer_registry_dat)
			
			if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
			
		}
		
		#
		# HES diagnosis data (ICD10s) ###########################################
		#
		
		cli::cli_alert("Ascertaining HES diagnosis data (ICD10s).")
		
		hesin_diag_path = file_paths$path[ file_paths$object=="hesin_diag" ]
		
		# create search string
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(ICD10s, collapse = "|")), " ", sprintf('%s', hesin_diag_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(ICD10s, collapse = "\" /c:\""), "\" ", sprintf('"%s"', hesin_diag_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(hesin_diag_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		if (! "eid" %in% headers)  headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		hesin_diag_tbl <- suppressWarnings(readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE))
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), and dates are dates
		if (nrow(hesin_diag_tbl)>0)  {
			if (!unix)  {
				hesin_diag_tbl <- hesin_diag_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(hesin_diag_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with HES episode data
			hesin_tbl = suppressWarnings(readr::read_tsv(file_paths$path[ file_paths$object=="hesin" ], show_col_types=FALSE, progress=FALSE))
			if (! "eid" %in% colnames(hesin_tbl))  colnames(hesin_tbl)[1] <- "eid"
			hesin_diag_tbl = dplyr::inner_join(hesin_tbl, hesin_diag_tbl, by=c("eid"="eid", "ins_index"="ins_index"))
			
			# format date cols if not "Date"
			date_cols = c("epistart", "epiend", "elecdate", "admidate", "disdate")
			for (dc in date_cols)  {
				dc = rlang::sym(dc)
				if (!lubridate::is.Date(hesin_diag_tbl |> dplyr::select(!!dc) |> dplyr::pull()))  {
					hesin_diag_tbl <- hesin_diag_tbl |> dplyr::mutate(!!dc := lubridate::dmy(!!dc))
				}
			}
		}
		
		cli::cli_alert_success("Loaded {.var hesin_diag} with {nrow(hesin_diag_tbl)} matched rows.")
		
		if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
		
	}
	
	#
	# ICD9 HES diagnosis data ###########################################
	#
	if (get_icd9)  {
		
		cli::cli_alert("Ascertaining HES diagnosis data (ICD9s).")
		
		hesin_diag_path = file_paths$path[ file_paths$object=="hesin_diag" ]
		
		# create search string
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(ICD9s, collapse = "|")), " ", sprintf('%s', hesin_diag_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(ICD9s, collapse = "\" /c:\""), "\" ", sprintf('"%s"', hesin_diag_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(hesin_diag_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		if (! "eid" %in% headers)  headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		hesin_diag_tbl_icd9 <- suppressWarnings(readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE))
		
		# check we have actually matched any ICD9s
			# exclude missing ICD9s (EIDs may have been matched)
		if (nrow(hesin_diag_tbl_icd9)>0)  
			hesin_diag_tbl_icd9 <- hesin_diag_tbl_icd9 |> dplyr::filter(!is.na(diag_icd9))
			# check codes match START of code string
		ICD9_search = stringr::str_flatten(ICD9s, collapse = "|")
		if (nrow(hesin_diag_tbl_icd9)>0)  
			hesin_diag_tbl_icd9 <- hesin_diag_tbl_icd9 |> dplyr::filter(stringr::str_starts(diag_icd9, !! ICD9_search))
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), and dates are dates
		if (nrow(hesin_diag_tbl_icd9)>0)  {
			if (!unix)  {
				hesin_diag_tbl_icd9 <- hesin_diag_tbl_icd9 |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(hesin_diag_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with HES episode data
			hesin_tbl = suppressWarnings(readr::read_tsv(file_paths$path[ file_paths$object=="hesin" ], show_col_types=FALSE, progress=FALSE))
			if (! "eid" %in% colnames(hesin_tbl))  colnames(hesin_tbl)[1] <- "eid"
			hesin_diag_tbl_icd9 = dplyr::inner_join(hesin_tbl, hesin_diag_tbl_icd9, by=c("eid"="eid", "ins_index"="ins_index"))
			
			# format date cols if not "Date"
			date_cols = c("epistart", "epiend", "elecdate", "admidate", "disdate")
			for (dc in date_cols)  {
				dc = rlang::sym(dc)
				if (!lubridate::is.Date(hesin_diag_tbl_icd9 |> dplyr::select(!!dc) |> dplyr::pull()))  
					hesin_diag_tbl_icd9 <- hesin_diag_tbl_icd9 |> dplyr::mutate(!!dc := lubridate::dmy(!!dc))
			}
		}
		
		cli::cli_alert_success("Loaded {.var hesin_diag} with {nrow(hesin_diag_tbl_icd9)} matched rows.")
		
		# combine with other hesin table?
		if (!is.null(hesin_diag_tbl))  hesin_diag_tbl = rbind(hesin_diag_tbl, hesin_diag_tbl_icd9)
		if (is.null(hesin_diag_tbl))   hesin_diag_tbl = hesin_diag_tbl_icd9
		
		if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
		
	}
	
	#
	# HES operations data ###########################################
	#
	if (get_oper)  {
		
		cli::cli_alert("Ascertaining HES operations data.")
		
		hesin_oper_path = file_paths$path[ file_paths$object=="hesin_oper" ]
		
		# create search string
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(oper_codes, collapse = "|")), " ", sprintf('%s', hesin_oper_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(oper_codes, collapse = "\" /c:\""), "\" ", sprintf('"%s"', hesin_oper_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(hesin_oper_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		if (! "eid" %in% headers)  headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		hesin_oper_tbl <- suppressWarnings(readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE))
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), and dates are dates
		# make sure OPCS3 are exact
		if (nrow(hesin_oper_tbl)>0)  {
			if (!unix)  {
				hesin_oper_tbl <- hesin_oper_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(hesin_oper_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			if (OPCS4s[1] != "" & OPCS3s[1] == "")  hesin_oper_tbl <- hesin_oper_tbl |> dplyr::filter(stringr::str_detect(oper4, stringr::str_flatten(!! OPCS4s, collapse = "|"))) 
			if (OPCS4s[1] == "" & OPCS3s[1] != "")  hesin_oper_tbl <- hesin_oper_tbl |> dplyr::filter(stringr::str_starts(oper3, stringr::str_flatten(!! OPCS3s, collapse = "|"))) 
			if (OPCS4s[1] != "" & OPCS3s[1] != "")  hesin_oper_tbl <- hesin_oper_tbl |> dplyr::filter(stringr::str_starts(oper3, stringr::str_flatten(!! OPCS3s, collapse = "|")) | stringr::str_detect(oper4, stringr::str_flatten(!!OPCS4s, collapse = "|"))) 
			
			if (is.character(hesin_oper_tbl$opdate))  hesin_oper_tbl$opdate <- lubridate::dmy(hesin_oper_tbl$opdate)
		}
		
		cli::cli_alert_success("Loaded {.var hesin_oper} with {nrow(hesin_oper_tbl)} matched rows.")
		
		if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
		
	}

	#
	# Ascertaining GP clinical  ########################################################
	#
	if (get_gp)  {
		
		cli::cli_alert("Ascertaining GP data.")
		
		gp_clinical_path = file_paths$path[ file_paths$object=="gp_clinical" ]
		
		# create search strings
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(gp_codes, collapse = "|")), " ", sprintf('%s', gp_clinical_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(gp_codes, collapse = "\" /c:\""), "\" ", sprintf('"%s"', gp_clinical_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(gp_clinical_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		if (! "eid" %in% headers)  headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		gp_clinical_tbl <- suppressWarnings(readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE))
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), the codes are definite matches, and the dates are dates
		if (nrow(gp_clinical_tbl)>0)  {
			if (!unix)  {
				gp_clinical_tbl <- gp_clinical_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(gp_clinical_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			gp_clinical_tbl <- gp_clinical_tbl |> dplyr::filter(read_2 %in% !!Read2s | read_3 %in% !!CTV3s) 
			
			# format date col if not "Date"
			if (!lubridate::is.Date(gp_clinical_tbl$event_dt))  gp_clinical_tbl <- gp_clinical_tbl |> dplyr::mutate(event_dt = lubridate::dmy(event_dt))
		}
		
		cli::cli_alert_success("Loaded {.var gp_clinical} with {nrow(gp_clinical_tbl)} matched rows.")
		
		if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	}
	
	#
	# Ascertaining self-reported illness  ########################################################
	#
	if (get_selfrep)  {
		
		cli::cli_alert("Ascertaining self-reported illness data.")
		
		# load data 
		selfrep_illness_dat <- suppressWarnings(readr::read_tsv(file_paths$path[ file_paths$object=="selfrep_illness" ], show_col_types = FALSE, progress = FALSE))
		
		# get self-reported illness data - convert to long
		selfrep_illness_tbl <- ukbrapR:::get_selfrep_illness(codes_df = codes_df, ukb_dat = selfrep_illness_dat, verbose = verbose)
		cli::cli_alert_success("Loaded {.var selfrep_illness} with {nrow(selfrep_illness_tbl)} matched rows.")
		
		rm(selfrep_illness_dat)
		
	}

	
	#
	#
	#
	
	cli::cli_alert_success(c("Finished. Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data as list
	output_list <- list(
		gp_clinical=gp_clinical_tbl, 
		hesin_diag=hesin_diag_tbl, 
		death_cause=death_cause_tbl, 
		cancer_registry=cancer_registry_tbl, 
		hesin_oper=hesin_oper_tbl, 
		selfrep_illness=selfrep_illness_tbl,
		codes_df=tibble::as_tibble(codes_df)
	)
	class(output_list) <- "ukbrapr_emr"
	return(output_list)
	
}


#' The old get_emr() function
#'
#' @export
#' @noRd
get_emr <- function(
	codes_df,
	spark_master = "spark://master:41000",
	file_paths = NULL,
	verbose = FALSE
)  {
	lifecycle::deprecate_stop("0.2.0", "get_emr()", "get_diagnoses()")
}

