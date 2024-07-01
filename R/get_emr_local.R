#' Get UK Biobank participant Electronic Medical Records (EMR) data 
#'
#' @description Get medical records for specific diagnostic codes list using local UK Biobank EMR long files
#'
#' @return Returns a list of data frames (the participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, and `gp_clinical`. Also includes the original codes list)
#'
#' @author Luke Pilling
#'
#' @name get_emr_local
#'
#' @param codes_df A data frame. Contains two columns: `code` and `vocab_id` i.e., a list of diagnostic codes, and an indicator of the vocabulary. Other columns are ignored.
#' @param file_paths A data frame. Columns must be `object` and `path` containing paths to: 
#'        `death`, `death_cause`, `hesin`, `hesin_diag` & `gp_clinical` 
#'        \code{default=NULL}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for CKD from GEMINI multimorbidity project
#' head(codes_df_ckd)
#'
#' # set location of file paths (if using one of my group servers `indy`, `snow` or `shapter` you can ignore this option)
#' paths = data.frame(
#'   object = c("death","death_cause","hesin","hesin_diag","gp_clinical"),
#'   path = c("/path/to/death.tsv","/path/to/death_cause.tsv","/path/to/hesin.tsv","/path/to/hesin_diag.tsv","/path/to/gp_clinical.tsv")
#' )
#'
#' # get EMR data - returns list of data frames (one per source)
#' emr_dat <- get_emr_local(codes_df_ckd, file_paths = paths)
#'
#' # save to data either as an R object, or separate as text files:
#' save(emr_dat, "ukbrap.CKD.emr.20231114.RDat")
#' readr::write_tsv(emr_dat$hesin_diag,  "ukbrap.CKD.hesin_diag.20231114.txt.gz")
#'
#' @export
#'
get_emr_local <- function(
	codes_df,
	file_paths = NULL,
	verbose = FALSE,
	local_paths = lifecycle::deprecated()
)  {
	
	# using old options?
	if (lifecycle::is_present(local_paths))  {
		lifecycle::deprecate_warn("0.1.5", "get_emr_local(local_paths)", "get_emr_local(file_paths)")
		file_paths = local_paths 
	}
	
	start_time <- Sys.time()
	
	vocab_col = "vocab_id"
	codes_col = "code"
	
	# Check input
	if (verbose) cat("Check inputs\n")
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
	
	must_include = c("death","death_cause","hesin","hesin_diag","gp_clinical")
	if (! all(must_include %in% file_paths$object)) cli::cli_abort("{.var file_paths} must contains objects `death`, `death_cause`, `hesin`, `hesin_diag` & `gp_clinical`")
	
	# assume OS in UNIX... check if Windows -- i.e., should we use `grep` or `findstr`
	unix = TRUE
	if (as.character(Sys.info()['sysname'])=="Windows")  {
		unix = FALSE
		# paths need converting from unix to dos with escape characters?
		file_paths = file_paths |> dplyr::mutate(path = stringr::str_replace_all(path, "/", "\\\\"))
	}
	
	# check files exist
	for (file in file_paths$path)  if (! file.exists(file))  cli::cli_abort("could not find file {.path {file}}")
	
	#
	#
	#
	
	# Check code lists - only first 5 digits are used by UK Biobank
	cli::cli_alert("Checking provided codes (remember only the first 5 digits are used by UK Biobank)")
	get_icd10 <- FALSE
	get_gp    <- FALSE
	ICD10s    <- ""
	Read2s    <- ""
	CTV3s     <- ""
	
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
	
	#
	#
	#
	
	# Get data for each code vocabulary
	if (verbose) cat("Ascertaining codes from long EMR files\n")
	death_cause_tbl <- NULL
	hesin_diag_tbl  <- NULL
	gp_clinical_tbl <- NULL
	if (get_icd10)  {
		
		#
		# death data ###########################################
		#
		
		cli::cli_alert("Ascertaining cause of death data.")
		
		death_cause_path = file_paths$path[ file_paths$object=="death_cause" ]
		
		# create search strings
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(ICD10s, collapse = "|")), " ", sprintf('"%s"', death_cause_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(ICD10s, collapse = "\" /c:\""), "\" ", sprintf('"%s"', death_cause_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(death_cause_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		death_cause_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		death_cause_tbl_nrow <- nrow(death_cause_tbl)
		
		# if any matches returned, make sure eid is formatted nicely (remove file name) and the dates are dates
		if (death_cause_tbl_nrow>0)  {
			if (!unix)  {
				death_cause_tbl <- death_cause_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(death_cause_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with date of death data
			death_tbl = readr::read_tsv(file_paths$path[ file_paths$object=="death" ], show_col_types=FALSE, progress=FALSE)
			colnames(death_tbl)[1] = "eid"
			death_cause_tbl = dplyr::inner_join(death_tbl, death_cause_tbl, by=c("eid"="eid", "ins_index"="ins_index"))
			death_cause_tbl <- death_cause_tbl |> dplyr::mutate(date_of_death = lubridate::dmy(date_of_death))
		}
		
		death_cause_tbl_nrow <- nrow(death_cause_tbl)
		cli::cli_alert_success("Loaded {.var death_cause} with {death_cause_tbl_nrow} matched rows.")
		
		if (verbose)  cat("Time taken so far:", Sys.time() - start_time, "\n")
		
		#
		# HES data ###########################################
		#
		cli::cli_alert("Ascertaining HES data.")
		
		hesin_diag_path = file_paths$path[ file_paths$object=="hesin_diag" ]
		
		# create search strings
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(ICD10s, collapse = "|")), " ", sprintf('"%s"', hesin_diag_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(ICD10s, collapse = "\" /c:\""), "\" ", sprintf('"%s"', hesin_diag_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(hesin_diag_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		hesin_diag_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		hesin_diag_tbl_nrow <- nrow(hesin_diag_tbl)
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), and dates are dates
		if (hesin_diag_tbl_nrow>0)  {
			if (!unix)  {
				hesin_diag_tbl <- hesin_diag_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(hesin_diag_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			
			# match with HES episode data
			hesin_tbl = readr::read_tsv(file_paths$path[ file_paths$object=="hesin" ], show_col_types=FALSE, progress=FALSE)
			colnames(hesin_tbl)[1] = "eid"
			hesin_diag_tbl = dplyr::inner_join(hesin_tbl, hesin_diag_tbl, by=c("eid"="eid", "ins_index"="ins_index"))
			hesin_diag_tbl = hesin_diag_tbl |> dplyr::mutate(epistart = lubridate::dmy(epistart), epiend = lubridate::dmy(epiend), elecdate = lubridate::dmy(elecdate), admidate = lubridate::dmy(admidate), disdate = lubridate::dmy(disdate))
		}
		
		hesin_diag_tbl_nrow <- nrow(hesin_diag_tbl)
		cli::cli_alert_success("Loaded {.var hesin_diag} with {hesin_diag_tbl_nrow} matched rows.")
		
		if (verbose)  cat("Time taken so far:", Sys.time() - start_time, "\n")
	}
	if (get_gp)  {
		
		cli::cli_alert("Ascertaining GP data (can take a few minutes).")
		
		gp_clinical_path = file_paths$path[ file_paths$object=="gp_clinical" ]
		
		# create search strings
		search_string <- paste0("grep -E ", sprintf('"%s"', stringr::str_flatten(gp_codes, collapse = "|")), " ", sprintf('"%s"', gp_clinical_path))
		if (!unix)  search_string <- stringr::str_c("findstr /c:\"", stringr::str_flatten(gp_codes, collapse = "\" /c:\""), "\" ", sprintf('"%s"', gp_clinical_path))
		if (verbose)  cat(" -- search string: ", search_string, "\n")
		
		# get file headers
		headers <- colnames(readr::read_tsv(gp_clinical_path, n_max=1, show_col_types=FALSE, progress=FALSE))
		headers[1] <- "eid"
		
		# use search string to only read lines that matched a code
		gp_clinical_tbl <- readr::read_tsv(pipe(search_string), col_names=headers, show_col_types=FALSE, progress=FALSE)
		gp_clinical_tbl_nrow <- nrow(gp_clinical_tbl)
		
		# if any matches returned, make sure eid is formatted nicely (remove file name), the codes are definite matches, and the dates are dates
		if (gp_clinical_tbl_nrow>0)  {
			if (!unix)  {
				gp_clinical_tbl <- gp_clinical_tbl |> 
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(gp_clinical_path))) |>
					dplyr::mutate(eid = stringr::str_remove(eid, stringr::fixed(":"))) |>
					dplyr::mutate(eid = as.numeric(eid))
			}
			gp_clinical_tbl <- gp_clinical_tbl |> dplyr::filter(read_2 %in% !!Read2s | read_3 %in% !!CTV3s) 
			gp_clinical_tbl <- gp_clinical_tbl |> dplyr::mutate(event_dt = lubridate::dmy(event_dt))
		}
		
		gp_clinical_tbl_nrow <- nrow(gp_clinical_tbl)
		cli::cli_alert_success("Loaded {.var gp_clinical} with {gp_clinical_tbl_nrow} matched rows.")
		
		if (verbose)  cat("Time taken so far:", Sys.time() - start_time, "\n")
	}
	
	#
	#
	#
	
	time_taken = as.numeric(difftime(Sys.time(), start_time, units="secs"))
	
	cli::cli_alert_success(c("Finished. Time taken: ", "{prettyunits::pretty_sec(time_taken)}."))
	cli::cli_alert_info("{.emph (Normally safe to disregard warnings regarding parsing issues)}.")
	
	# Return data as list
	output_list <- list(gp_clinical=gp_clinical_tbl, hesin_diag=hesin_diag_tbl, death_cause=death_cause_tbl, codes_df=tibble::as_tibble(codes_df))
	class(output_list) <- "ukb_emr"
	return(output_list)
	
}


