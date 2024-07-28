#' Get UK Biobank participant Electronic Medical Records (EMR) data 
#'
#' @description Get UK Biobank medical records for specific diagnostic codes list
#'
#' This function relies on exported raw data files and thus does not need to be run in a Spark cluster. If the files are not in the default locations for the package you will need to specify the  `file_paths` to exported tables. Recommend to run `export_tables()` once in your project to export the tables to the default paths for the package.
#'
#' Valid code vocabularies are:
#'  - ICD10 (for `hesin`, `death_cause` and `cancer_registry` searches)
#'  - Read2 / CTV3 (for `gp_clinical`)
#'  - OPCS3 / OPCS4 (for `hesin_oper`)
#'  - ukb_cancer / ukb_noncancer (for self-reported illness at UK Biobank assessments - all available will be searched)
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
	file_paths = ukbrapR:::ukbrapr_paths,
	verbose = FALSE
)  {
	
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
	
	if (! any(c("ICD10","Read2","CTV3") %in% codes_df[,vocab_col]))  stop("Vocabularies need to include ICD10, Read2 or CTV3")
	
	# Is this one of my systems? If so, get the internal file_paths 
	nodename <- as.character(Sys.info()['nodename'])
	if ( nodename %in% c("SNOW","SHAPTER") )  {
		file_paths = ukbrapR:::snow_paths
		if (verbose)  cli::cli_alert_info("Identified server {nodename} - using predefined paths.")
	}
	if ( nodename == "indy.ex.ac.uk" )  {
		file_paths = ukbrapR:::indy_paths
		if (verbose)  cli::cli_alert_info("Identified server {nodename} - using predefined paths.")
	}
	
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
	must_include = c("death","death_cause","hesin","hesin_diag","hesin_oper","gp_clinical","cancer_registry","selfrep_illness")
	for (file in must_include)  if (! file %in% file_paths$object) cli::cli_abort("{.var file_paths} must contain {.path {file}}")
	
	# if files not already downloaded to RAP then download them to user's home directory
	#   only do this if the file paths are to "ukbrapr_data"
	files = file_paths$path
	if (stringr::str_detect(files[1], "ukbrapr_data"))  {
		# if the directory exists already then no need to do again
		if (!file.exists(stringr::str_c("~/", files[1])))  {
			cli::cli_progress_bar("Downloading files from the RAP", total = length(files))
			# create directory
			home_path = as.character(Sys.getenv()["HOME"])
			dir.create(stringr::str_c(home_path, "/ukbrapr_data"), showWarnings = FALSE)
			system("dx cd ukbrapr_data")
			# copy file from RAP space to instance
			for (file in files)  {
				#system(stringr::str_c("cp /mnt/project/", file, " ~/ukbrapr_data/"))
				system(stringr::str_c("dx download \"", basename(file), "\" -o \"", home_path, "/ukbrapr_data\""))
				cli::cli_progress_update()
			}
			cli::cli_progress_done()
			# add home directory prefix to file paths
			file_paths$path = stringr::str_c(home_path, "/", file_paths$path)
			if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
		}
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
	#
	#
	
	# Check code lists - only first 5 digits are used by UK Biobank
	cli::cli_alert("Checking provided codes (remember only the first 5 digits are used by UK Biobank)")
	get_icd10 <- FALSE
	get_gp    <- FALSE
	get_oper  <- FALSE
	ICD10s    <- ""
	Read2s    <- ""
	CTV3s     <- ""
	OPCS3s    <- ""
	OPCS4s    <- ""
	
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
	}
	
	# get Read2 and CTV3s. First 5 characters only. 
	if (any(codes_df[,vocab_col] == "Read2"))  {
		get_gp    <- TRUE
		Read2s    <- codes_df[ codes_df[,vocab_col] == "Read2" , codes_col ]
		Read2s    <- stringr::str_sub(Read2s, 1, 5) |> unique()
		cat(" - N unique Read2 codes:", length(Read2s), "\n")
	}
	if (any(codes_df[,vocab_col] == "CTV3"))  {
		get_gp   <- TRUE
		CTV3s    <- codes_df[ codes_df[,vocab_col] == "CTV3" , codes_col ]
		CTV3s    <- stringr::str_sub(CTV3s, 1, 5) |> unique()
		cat(" - N unique CTV3 codes:", length(CTV3s), "\n")
	}
	gp_codes = c(Read2s, CTV3s)
	
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
		oper_codes = c(oper_codes, OPCS4s)
	}
	
	# check for self-reported codes
	n_selfrep = length(unique(codes_df[codes_df[,vocab_col] %in% c("ukb_cancer","ukb_noncancer"),codes_col]))
	if (n_selfrep>0)  cat(" - N unique UKB-self-reported codes:", n_selfrep, "\n")
	
	#
	#
	#
	
	# Get data for each code vocabulary
	if (verbose) cli::cli_alert("Ascertaining codes from long EMR files")
	death_cause_tbl     <- NULL  # ICD10
	hesin_diag_tbl      <- NULL  # ICD10
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
		death_cause_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		
		# if any matches returned, make sure eid is formatted nicely (remove file name) and the dates are dates
		if (nrow(death_cause_tbl)>0)  {
			if (!unix)  {
				death_cause_tbl <- death_cause_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(death_cause_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with date of death data
			death_tbl = readr::read_tsv(file_paths$path[ file_paths$object=="death" ], show_col_types=FALSE, progress=FALSE)
			if (! "eid" %in% colnames(death_tbl))  colnames(death_tbl)[1] <- "eid"
			death_cause_tbl = dplyr::inner_join(death_tbl, death_cause_tbl, by=c("eid"="eid", "ins_index"="ins_index"))
			
			# format date col if not "Date"
			if (!lubridate::is.Date(death_cause_tbl$date_of_death))  death_cause_tbl <- death_cause_tbl |> dplyr::mutate(date_of_death = lubridate::dmy(date_of_death))
		}
		
		cli::cli_alert_success("Loaded {.var death_cause} with {nrow(death_cause_tbl)} matched rows.")
		
		if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
		
		#
		# HES diagnosis data ###########################################
		#
		
		cli::cli_alert("Ascertaining HES diagnosis data.")
		
		hesin_diag_path = file_paths$path[ file_paths$object=="hesin_diag" ]
		
		# create search string
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(ICD10s, collapse = "|")), " ", sprintf('%s', hesin_diag_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(ICD10s, collapse = "\" /c:\""), "\" ", sprintf('"%s"', hesin_diag_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(hesin_diag_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		if (! "eid" %in% headers)  headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		hesin_diag_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), and dates are dates
		if (nrow(hesin_diag_tbl)>0)  {
			if (!unix)  {
				hesin_diag_tbl <- hesin_diag_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(hesin_diag_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with HES episode data
			hesin_tbl = readr::read_tsv(file_paths$path[ file_paths$object=="hesin" ], show_col_types=FALSE, progress=FALSE)
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
		
		#
		# cancer registry ####################################
		#
		
		# do any ICD10s start with a C? Skip if not.
		if (any(stringr::str_starts(ICD10s, "C")))  {
			
			cli::cli_alert("Ascertaining cancer registry data.")
			
			# load data 
			cancer_registry_dat <- readr::read_tsv(file_paths$path[ file_paths$object=="cancer_registry" ], show_col_types = FALSE, progress = FALSE)
			
			# get cancer registry data for these ICD10s
			cancer_registry_tbl <- ukbrapR::get_cancer_registry(codes = ICD10s, ukb_dat = cancer_registry_dat, verbose = verbose)
			cli::cli_alert_success("Loaded {.var cancer_registry} with {nrow(cancer_registry_tbl)} matched rows.")
			
			rm(cancer_registry_dat)
			
			if (verbose)  cli::cli_alert_info(c("Time taken so far: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
			
		}
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
		hesin_oper_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), and dates are dates
		# make sure OPCS3 are exact
		if (nrow(hesin_oper_tbl)>0)  {
			if (!unix)  {
				hesin_oper_tbl <- hesin_oper_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(hesin_oper_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			hesin_oper_tbl <- hesin_oper_tbl |> dplyr::filter(oper3 %in% !!OPCS3s | stringr::str_detect(oper4, stringr::str_flatten(oper_codes, collapse = "|"))) 
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
		gp_clinical_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		
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
	if (n_selfrep>0)  {
		
		cli::cli_alert("Ascertaining self-reported illness data.")
		
		# load data 
		selfrep_illness_dat <- readr::read_tsv(file_paths$path[ file_paths$object=="selfrep_illness" ], show_col_types = FALSE, progress = FALSE)
		
		# get cancer registry data for these ICD10s
		selfrep_illness_tbl <- ukbrapR::get_selfrep_illness(codes_df = codes_df, ukb_dat = selfrep_illness_dat, verbose = verbose)
		cli::cli_alert_success("Loaded {.var selfrep_illness} with {nrow(selfrep_illness_tbl)} matched rows.")
		
		rm(selfrep_illness_dat)
		
	}

	
	#
	#
	#
	
	cli::cli_alert_success(c("Finished. Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	cli::cli_alert_info("{.emph (Normally safe to disregard warnings regarding parsing issues)}.")
	
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

