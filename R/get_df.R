#' Get UK Biobank participant Date First (DF) diagnosis 
#'
#' @description For each participant identify the date of first diagnosis from all available electronic medical records & self-reported data.
#'
#' If `use_baseline_dates=TRUE` (the default) then will also produce a binary 0/1 variable, indicating the controls (people without a diagnosis) and setting the date first `_df` field to the date of censoring (currently 30 October 2022).
#'
#' @return Returns a single, "wide" data frame: the participant data for the requested diagnosis codes with "date first" `_df` variables. One for each source of data, and a combined variable.
#'
#' @author Luke Pilling
#'
#' @name get_df
#'
#' @param diagnosis_list A list of data frames. The output of `get_diagnoses()` i.e., the raw diagnosis and self-reported illness data that matched the provided codes list.
#' @param prefix String. Prefix to add to variable names (e.g., if prefix="chd" the output variables would be "chd_gp_df", "chd_hes_df", "chd_df" etc.)
#'        \code{default=NULL}
#' @param group_by String. If the codes list provided to `get_diagnoses()` (i.e., in diagnosis_list$codes_df) contained a grouping/condition variable, indicate the variable name here. 
#'        "Date first" variables will be created for each prefix in the grouping variable. The `prefix` option is ignored, in favour of the names in the grouping variable.
#'        \code{default=NULL}
#' @param include_selfrep_illness logical. Include self-reported diagnosesin the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_death_cause logical. Include the cause of death in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_gp_clinical logical. Include the GP data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_hesin_diag logical. Include the HES diagnosis data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_hesin_oper logical. Include the HES OPCS (operations) data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_cancer_registry logical. Include the cancer registry data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param use_baseline_dates logical. If `baseline_dates` available in file paths, produce a binary 0/1 variable, indicating the controls (people without a diagnosis) and setting the date first `_df` field to the date of censoring (currently see `censoring_date` option).
#'        \code{default=TRUE}
#' @param file_paths A data frame. Columns must be `object` and `path` containing paths to outputted files. If not provided will use those in `ukbrapr_paths`
#'        \code{default=NULL}
#' @param censoring_date A string. If using baseline data to infer control participants, include a censoring date (set to NA if not desired). Use dd-mm-yyyy format. Default is the (current) HES date.
#'        \code{default="30-10-2022"}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' ###############################################
#' # example 1. haemochromatosis
#'
#' # get diagnosis data - returns list of data frames (one per source)
#' diagnosis_list <- get_diagnoses(ukbrapR:::codes_df_hh)
#'
#' # for each participant, get Date First diagnosed with the condition
#' diagnosis_df <- get_df(diagnosis_list, prefix="hh")
#'
#' ###############################################
#' # example 2. get multiple diseases at once
#' #            don't have to all have the same code types/data sources
#'
#' codes = rbind(ukbrapR:::codes_df_hh, ukbrapR:::codes_df_ckd)
#' print(codes)
#'
#' # get diagnosis data - returns list of data frames (one per source)
#' diagnosis_list <- get_diagnoses(codes)
#'
#' # for each participant, get Date First diagnosed with the condition
#' diagnosis_df <- get_df(diagnosis_list, group_by="condition")
#'
#' @export
#'
get_df <- function(
	diagnosis_list,
	prefix = NULL,
	group_by = NULL,
	include_selfrep_illness = TRUE,
	include_death_cause = TRUE,
	include_gp_clinical = TRUE,
	include_hesin_diag = TRUE,
	include_hesin_oper = TRUE,
	include_cancer_registry = TRUE,
	use_baseline_dates = TRUE,
	file_paths = NULL,
	censoring_date = "30-10-2022",
	verbose = FALSE
)  {
	
	v <- packageVersion("ukbrapR")
	cli::cli_alert_info("ukbrapR v{v}")
	
	start_time <- Sys.time()
	
	# use baseline dates?
	if (use_baseline_dates)  {
		
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
		
		# does baseline_dates file exist?
		bl_file_path = file_paths$path[ file_paths$object=="baseline_dates" ]
		if (file.exists(bl_file_path))  {
			
			# read baseline dates
			bl_data = readr::read_tsv(bl_file_path, show_col_types = FALSE, progress = FALSE)
			
			# rename assessment date (p53_i0) for ease later
			bl_data = bl_data |> 
				dplyr::rename(assessment_date_0 = p53_i0) |>
				dplyr::select(eid, assessment_date_0)
			
			# censoring date provided?
			if (!is.na(censoring_date))  censoring_date = lubridate::dmy(censoring_date)
			
		} else {
			use_baseline_dates = FALSE
			cli::cli_alert_warning("Could not find \"baseline dates\" file at path {.file {bl_file_path}} - continued without using it")
		}
	}
	
	# are we using a grouping variable?
	if (is.null(group_by))  {
		
		df_tbl = ukbrapR:::get_df1(
			diagnosis_list=diagnosis_list, 
			include_selfrep_illness=include_selfrep_illness, 
			include_gp_clinical=include_gp_clinical, 
			include_death_cause=include_death_cause, 
			include_hesin_diag=include_hesin_diag, 
			include_cancer_registry=include_cancer_registry,
			include_hesin_oper=include_hesin_oper,
			prefix=prefix, 
			verbose=verbose
		)
		
		# add binary variables (ever, prev) & censoring date (if provided)
		if (use_baseline_dates)  df_tbl = ukbrapR:::get_df1_add_bin(df=df_tbl, bd=bl_data, cd=censoring_date, prefix=prefix, verbose=verbose)
		
	} else {
		
		if (verbose) cli::cli_alert("Grouping variable detected - checking codes")
		
		# check input codes and group variable
		if (class(diagnosis_list) != "ukbrapr_emr")  cli::cli_alert_warning(c("{.var diagnosis_list} should be of class {.cls ukbrapr_emr}", "x" = "You've supplied a {.cls {class(diagnosis_list)}} - behaviour may not be as intended."))
		
		codes = as.data.frame(diagnosis_list[['codes_df']])
		
		if (! group_by %in% colnames(codes))  cli::cli_abort("{.var diagnosis_list} codes data frame needs to contain the group column {group_by}")
		if (class(codes[,group_by]) != "character")  cli::cli_abort(c("Group column {group_by} needs to be a character vector", "x" = "You've supplied a {.cls {class(codes[,group_by])}}."))
		
		# for each grouping variable, subset diagnostic data, run get_df1(), and combine output
		groups = unique(codes[,group_by])
		df_tbl = NULL
		cli::cli_alert("{length(groups)} group{?s} identified - getting date first for each")
		
		for (group in groups)  {
			
			if (verbose) cli::cli_alert("Doing group {group}")
			
			# subset diagnostic codes 
			codes_sub = codes[codes[,group_by]==group,]
			diagnosis_list_sub = diagnosis_list
			
			## gp clinical
			if (!is.null(diagnosis_list_sub$gp_clinical) & any(codes_sub$vocab_id %in% c("Read2","CTV3")))  {  
				Read2s = ""
				CTV3s  = ""
				if (any(codes_sub$vocab_id == "Read2"))  {
					Read2s <- codes_sub$code[codes_sub$vocab_id == "Read2"]
					Read2s <- stringr::str_sub(Read2s, 1, 5) |> unique()
				}
				if (any(codes_sub$vocab_id == "CTV3"))  {
					CTV3s  = codes_sub$code[codes_sub$vocab_id == "CTV3"]
					CTV3s <- stringr::str_sub(CTV3s, 1, 5) |> unique()
				}
				diagnosis_list_sub$gp_clinical = diagnosis_list_sub$gp_clinical |> dplyr::filter(read_2 %in% !!Read2s | read_3 %in% !!CTV3s) 
			}
			
			# create ICD10 search string
			if (any(codes_sub$vocab_id == "ICD10"))  {
				ICD10s <- codes_sub |>
					dplyr::filter(vocab_id == "ICD10") |>
					dplyr::select(code) |>
					dplyr::pull() |>
					unique() |>
					stringr::str_remove(stringr::fixed(".")) |> 
					stringr::str_sub(1, 5)
				ICD10_search = stringr::str_flatten(ICD10s, collapse = "|")
			}
			
			## hesin_diag
			if (!is.null(diagnosis_list_sub$hesin_diag) & any(codes_sub$vocab_id %in% c("ICD10","ICD9")))  {  
				hesin_diag_sub = NULL
				
				if (any(codes_sub$vocab_id == "ICD10"))  {
					colnames(diagnosis_list_sub$hesin_diag) = tolower(colnames(diagnosis_list_sub$hesin_diag))
					hesin_diag_sub = diagnosis_list_sub$hesin_diag |> dplyr::filter(stringr::str_detect(diag_icd10, !! ICD10_search))
				}
				
				if (any(codes_sub$vocab_id == "ICD9"))  {
					ICD9s = ""
					if (any(codes_sub$vocab_id == "ICD9"))  {
						ICD9s <- codes_sub |>
							dplyr::filter(vocab_id == "ICD9") |>
							dplyr::select(code) |>
							dplyr::pull() |>
							unique() |>
							stringr::str_remove(stringr::fixed(".")) |> 
							stringr::str_sub(1, 5)
					}
					ICD9_search = stringr::str_flatten(ICD9s, collapse = "|")
					colnames(diagnosis_list_sub$hesin_diag) = tolower(colnames(diagnosis_list_sub$hesin_diag))
					hesin_diag_sub = rbind(hesin_diag_sub, diagnosis_list_sub$hesin_diag |> dplyr::filter(stringr::str_starts(diag_icd9, !! ICD9_search)))
				}
				
				diagnosis_list_sub$hesin_diag = hesin_diag_sub
			}
			
			## death_cause
			if (!is.null(diagnosis_list_sub$death_cause) & any(codes_sub$vocab_id == "ICD10"))  
				diagnosis_list_sub$death_cause = diagnosis_list_sub$death_cause |> dplyr::filter(stringr::str_detect( cause_icd10, !! ICD10_search))
			
			## cancer_registry
			if (!is.null(diagnosis_list_sub$cancer_registry) & any(codes_sub$vocab_id == "ICD10"))  
				diagnosis_list_sub$cancer_registry = diagnosis_list_sub$cancer_registry |> dplyr::filter(stringr::str_detect( icd10, !! ICD10_search))  
			
			## hesin_oper
			if (!is.null(diagnosis_list_sub$hesin_oper) & any(codes_sub$vocab_id %in% c("OPCS3","OPCS4")))  {  
				OPCS4s = ""
				if (any(codes_sub$vocab_id == "OPCS4"))  {
					OPCS4s <- codes_sub |>
						dplyr::filter(vocab_id == "OPCS4") |>
						dplyr::select(code) |>
						dplyr::pull() |>
						unique() |>
						stringr::str_remove(stringr::fixed(".")) |> 
						stringr::str_sub(1, 5)
				}
				OPCS4_search = stringr::str_flatten(OPCS4s, collapse = "|")
				diagnosis_list_sub$hesin_oper = diagnosis_list_sub$hesin_oper |> dplyr::filter(stringr::str_detect(oper4, !! OPCS4_search))
				
				if (any(codes_sub$vocab_id == "OPCS3"))  {
					OPCS3s <- codes_sub |>
						dplyr::filter(vocab_id == "OPCS3") |>
						dplyr::select(code) |>
						dplyr::pull() |>
						unique() |>
						stringr::str_remove(stringr::fixed(".")) |> 
						stringr::str_sub(1, 5)
				}
				OPCS3_search = stringr::str_flatten(OPCS3s, collapse = "|")
				diagnosis_list_sub$hesin_oper = diagnosis_list_sub$hesin_oper |> dplyr::filter(stringr::str_detect(oper3, !! OPCS3_search))
			}
			
			## self-reported illness 
			if (!is.null(diagnosis_list_sub$selfrep_illness) & any(codes_sub$vocab_id %in% c("ukb_cancer","ukb_noncancer")))  {
				if (any(codes_sub$vocab_id == "ukb_cancer"))  {
					codes_cancer = codes_sub$code[ codes_sub$vocab_id == "ukb_cancer" ]
					diagnosis_list_sub$selfrep_illness = diagnosis_list_sub$selfrep_illness |> dplyr::filter(cancer_code %in% codes_cancer)
				}
				if (any(codes_sub$vocab_id == "ukb_noncancer"))  {
					codes_noncancer = codes_sub$code[ codes_sub$vocab_id == "ukb_noncancer" ]
					diagnosis_list_sub$selfrep_illness = diagnosis_list_sub$selfrep_illness |> dplyr::filter(noncancer_code %in% codes_noncancer)
				}
			}
			if (!any(codes_sub$vocab_id %in% c("ukb_cancer","ukb_noncancer")))  include_selfrep_illness <- FALSE
			
			
			#
			#
			# get DF for this condition
			df_tbl_sub = ukbrapR:::get_df1(
				diagnosis_list=diagnosis_list_sub, 
				include_selfrep_illness=include_selfrep_illness, 
				include_gp_clinical=include_gp_clinical, 
				include_death_cause=include_death_cause, 
				include_hesin_diag=include_hesin_diag, 
				include_cancer_registry=include_cancer_registry,
				include_hesin_oper=include_hesin_oper,
				prefix=group, 
				verbose=verbose
			)
			
			# add binary variables (ever, prev) & censoring date (if provided)
			if (use_baseline_dates)  df_tbl_sub = ukbrapR:::get_df1_add_bin(df=df_tbl_sub, bd=bl_data, cd=censoring_date, prefix=group, verbose=verbose)
			
			# merge with main DF table
			if (is.null(df_tbl))  {
				df_tbl = df_tbl_sub
			} else {
				df_tbl = dplyr::full_join(df_tbl, df_tbl_sub, by="eid")
			}
			
		}
		
		cli::cli_alert_info("Finished getting date first diagnosed for each group/condition.")
		
	}
	
	if (verbose)  cli::cli_alert_success(c("Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# return table
	return(df_tbl)

}


#' Get UK Biobank participant Date First (DF) diagnosis for one condition
#'
#' @description For each participant identify the date of first diagnosis from all available electronic medical records & self-reported data.
#'
#' @return Returns a single, "wide" data frame: the participant data for the requested diagnosis codes with "date first" `_df` variables. One for each source of data, and a combined variable.
#'
#' @author Luke Pilling
#'
#' @name get_df1
#'
#' @noRd
get_df1 <- function(
	diagnosis_list,
	include_selfrep_illness = TRUE,
	include_gp_clinical = TRUE,
	include_death_cause = TRUE,
	include_hesin_diag = TRUE,
	include_cancer_registry = TRUE,
	include_hesin_oper = TRUE,
	prefix = NULL,
	verbose = FALSE
)  {
	
	#start_time <- Sys.time()
	
	# Check input
	if (verbose) cli::cli_alert("Check inputs\n")
	if (class(diagnosis_list) != "ukbrapr_emr")  cli::cli_alert_warning(c("{.var diagnosis_list} should be of class {.cls ukbrapr_emr}", "x" = "You've supplied a {.cls {class(diagnosis_list)}} - behaviour may not be as intended."))
	
	# "use" if there is any data (i.e., provide an individual _df column) -- "include" in the main combined only if specified by user
	use_selfrep <- use_gp_clinical <- use_death_cause <- use_hesin_diag <- use_cancer_registry <- use_hesin_oper <- FALSE
	
	if (include_selfrep_illness)  if ( !is.null(diagnosis_list$selfrep_illness) )  if ( nrow(diagnosis_list$selfrep_illness)>0 )  use_selfrep <- TRUE
	
	if ( !is.null(diagnosis_list$gp_clinical) )      if ( nrow(diagnosis_list$gp_clinical)>0 )      use_gp_clinical     <- TRUE
	if ( !is.null(diagnosis_list$death_cause) )      if ( nrow(diagnosis_list$death_cause)>0 )      use_death_cause     <- TRUE
	if ( !is.null(diagnosis_list$hesin_diag) )       if ( nrow(diagnosis_list$hesin_diag)>0 )       use_hesin_diag      <- TRUE
	if ( !is.null(diagnosis_list$cancer_registry) )  if ( nrow(diagnosis_list$cancer_registry)>0 )  use_cancer_registry <- TRUE
	if ( !is.null(diagnosis_list$hesin_oper) )       if ( nrow(diagnosis_list$hesin_oper)>0 )       use_hesin_oper      <- TRUE
	
	#
	#
	#
	
	# Convert self-reported illness to "wide" Date First 
	if (use_selfrep)  {
		if (verbose) cli::cli_alert("Get date first diagnosis: selfrep_df\n")
		selfrep_illness <- ukbrapR:::get_selfrep_illness_df(codes_df=diagnosis_list$codes_df, ukb_dat=diagnosis_list$selfrep_illness, verbose=verbose)
	}
	
	# Convert gp_clinical to "wide" Date First
	if (use_gp_clinical)  {
		if (verbose) cli::cli_alert("Get date first diagnosis: gp_df\n")
		gp_clinical <- diagnosis_list$gp_clinical |>
			dplyr::filter(!is.na(event_dt)) |>
			dplyr::group_by(eid) |>
			dplyr::summarize(gp_df=min(event_dt, na.rm=TRUE)) |>
			dplyr::mutate(gp_df = dplyr::if_else(is.finite(gp_df), gp_df, NA))
	}
	
	# Convert death_cause to "wide" Date First
	if (use_death_cause)  {
		if (verbose) cli::cli_alert("Get date first diagnosis: death_df\n")
		death_cause <- diagnosis_list$death_cause |>
			dplyr::filter(!is.na(date_of_death)) |>
			dplyr::group_by(eid) |>
			dplyr::summarize(death_df=min(date_of_death, na.rm=TRUE)) |>
			dplyr::mutate(death_df = dplyr::if_else(is.finite(death_df), death_df, NA))
	}
	
	# Convert hesin_diag to "wide" Date First
	if (use_hesin_diag)  {
		if (verbose) cli::cli_alert("Get date first diagnosis: hes_df\n")
		hesin_diag <- diagnosis_list$hesin_diag |>
			dplyr::mutate(diagnosis_date = epistart) |>
			dplyr::mutate(diagnosis_date = dplyr::if_else(is.na(diagnosis_date), epiend, diagnosis_date)) |>
			dplyr::mutate(diagnosis_date = dplyr::if_else(is.na(diagnosis_date), admidate, diagnosis_date)) |>
			dplyr::mutate(diagnosis_date = lubridate::as_date(dplyr::if_else(is.na(diagnosis_date), disdate, diagnosis_date))) |>
			dplyr::filter(!is.na(diagnosis_date)) |>
			dplyr::group_by(eid) |>
			dplyr::summarize(hes_df=min(diagnosis_date, na.rm=TRUE)) |>
			dplyr::mutate(hes_df = dplyr::if_else(is.finite(hes_df), hes_df, NA)) 
	}
	
	# Convert cancer registry to "wide" Date First 
	if (use_cancer_registry)  {
		if (verbose) cli::cli_alert("Get date first diagnosis: canreg_df\n")
		cancer_registry <- ukbrapR:::get_cancer_registry_df(codes_df=diagnosis_list$codes_df, ukb_dat=diagnosis_list$cancer_registry, verbose=verbose)
	}
	
	# Convert hesin_oper to "wide" Date First
	if (use_hesin_oper)  {
		if (verbose) cli::cli_alert("Get date first diagnosis: oper_df\n")
		hesin_oper <- diagnosis_list$hesin_oper |>
			dplyr::filter(!is.na(opdate)) |>
			dplyr::group_by(eid) |>
			dplyr::summarize(oper_df=min(opdate, na.rm=TRUE)) |>
			dplyr::mutate(oper_df = dplyr::if_else(is.finite(oper_df), oper_df, NA))
	}
	
	#
	#
	#
	
	# Combine into single data frame
	if (verbose) cli::cli_alert("Combine into single wide data frame\n")
	diagnosis_df <- NULL
	if (use_selfrep)  {
		diagnosis_df <- selfrep_illness
	}
	if (use_gp_clinical)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- gp_clinical
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, gp_clinical, by="eid")
		}
	}
	if (use_hesin_diag)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- hesin_diag
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, hesin_diag, by="eid")
		}
	}
	if (use_death_cause)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- death_cause
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, death_cause, by="eid")
		}
	}
	if (use_cancer_registry)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- cancer_registry
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, cancer_registry, by="eid")
		}
	}
	if (use_hesin_oper)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- hesin_oper
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, hesin_oper, by="eid")
		}
	}
	
	#
	#
	#
	
	# Combined "date first, any source" variable & "source" variable
	if (verbose) cli::cli_alert("Combined \"date first, any source\" variable\n")
	diagnosis_df$df <- NA
	diagnosis_df$src <- ""
	
	if (include_selfrep_illness & use_selfrep)  {
		diagnosis_df <- diagnosis_df |> 
			dplyr::mutate(
				df = selfrep_df,
				src = dplyr::if_else(! is.na(selfrep_df), stringr::str_c("selfrep_i", selfrep_i), NA)) |>
			dplyr::select(-selfrep, -selfrep_i)
	}
	
	if (include_gp_clinical & use_gp_clinical)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(
			src = dplyr::case_when(
				!is.na(gp_df) & is.na(df)  ~ "gp",
				!is.na(gp_df) & !is.na(df) & gp_df<df ~ "gp",
				TRUE ~ src),
			df = dplyr::case_when(
				!is.na(gp_df) & is.na(df)  ~ gp_df,
				!is.na(gp_df) & !is.na(df) & gp_df<df ~ gp_df,
				TRUE ~ df)
		)
	}
	
	if (include_hesin_diag & use_hesin_diag)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(
			src = dplyr::case_when(
				!is.na(hes_df) & is.na(df)  ~ "hes",
				!is.na(hes_df) & !is.na(df) & hes_df<df ~ "hes",
				TRUE ~ src),
			df = dplyr::case_when(
				!is.na(hes_df) & is.na(df)  ~ hes_df,
				!is.na(hes_df) & !is.na(df) & hes_df<df ~ hes_df,
				TRUE ~ df)
		)
	}
	
	if (include_death_cause & use_death_cause)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(
			src = dplyr::case_when(
				!is.na(death_df) & is.na(df)  ~ "death",
				!is.na(death_df) & !is.na(df) & death_df<df ~ "death",
				TRUE ~ src),
			df = dplyr::case_when(
				!is.na(death_df) & is.na(df)  ~ death_df,
				!is.na(death_df) & !is.na(df) & death_df<df ~ death_df,
				TRUE ~ df)
		)
	}
	
	if (include_cancer_registry & use_cancer_registry)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(
			src = dplyr::case_when(
				!is.na(canreg_df) & is.na(df)  ~ "canreg",
				!is.na(canreg_df) & !is.na(df) & canreg_df<df ~ "canreg",
				TRUE ~ src),
			df = dplyr::case_when(
				!is.na(canreg_df) & is.na(df)  ~ canreg_df,
				!is.na(canreg_df) & !is.na(df) & canreg_df<df ~ canreg_df,
				TRUE ~ df)
			) |>
			dplyr::select(-canreg)
		
	}
	
	if (include_hesin_oper & use_hesin_oper)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(
			src = dplyr::case_when(
				!is.na(oper_df) & is.na(df)  ~ "hesin_oper",
				!is.na(oper_df) & !is.na(df) & oper_df<df ~ "hesin_oper",
				TRUE ~ src),
			df = dplyr::case_when(
				!is.na(oper_df) & is.na(df)  ~ oper_df,
				!is.na(oper_df) & !is.na(df) & oper_df<df ~ oper_df,
				TRUE ~ df)
		)
	}
	
	# if src & df are empty drop these rows
	diagnosis_df = diagnosis_df |> dplyr::filter(src!="" & !is.na(df))
	
	# adding variable name prefix?
	if (!is.null(prefix))  {
		if (is.character(prefix) & length(prefix) == 1)  {
			names(diagnosis_df)[2:ncol(diagnosis_df)] = stringr::str_c(prefix, "_", names(diagnosis_df)[2:ncol(diagnosis_df)])
		} else {
			cli::cli_alert_warning("Prefix was not a single string - variables names left as default")
		}
	}
	
	#
	#
	# done!
	
	#if (verbose)  cat("Done. Time taken:", Sys.time() - start_time, "\n")
	
	diagnosis_df_nrow = nrow(diagnosis_df)
	if (is.null(prefix))   cli::cli_alert_success("Identified date of first diagnosis in {diagnosis_df_nrow} participants.")
	if (!is.null(prefix))  cli::cli_alert_success("Identified date of first {prefix} diagnosis in {diagnosis_df_nrow} participants.")
	
	# Return data frame
	diagnosis_df
	
}



#' Add binary variables and censoring date
#'
#' @description Ever and prevalent binary vars. Censoring date. Only to the combined _df variable
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name get_df1_add_bin
#'
#' @noRd
get_df1_add_bin = function(
	df,
	bd,
	cd,
	prefix = NULL,
	verbose = FALSE
)  {
	
	if (verbose) cli::cli_alert("Creating binary \"ever diagnosed\" field - adding censoring date {cd} to date first `_df` field")
	
	# if no prefix them colnames are just `df` etc. - if one provided then include an underscore
	if (is.null(prefix))  {
		prefix = ""
	} else {
		prefix = stringr::str_c(prefix, "_")
	}
	
	# define new variable names 
	var_df       = rlang::sym(stringr::str_c(prefix, "df"))
	var_bin      = rlang::sym(stringr::str_c(prefix, "bin"))
	var_bin_prev = rlang::sym(stringr::str_c(prefix, "bin_prev"))
	
	# merge df and baseline data 
	df = dplyr::full_join(df, bd, by="eid")
	
	# create binary "ever"
	df = df |> dplyr::mutate(!!var_bin := dplyr::if_else(!is.na(!!var_df), 1, 0))
	
	# create prevalent variables 
	df = df |> dplyr::mutate(!!var_bin_prev := dplyr::if_else(!!var_bin==1 & !!var_df<assessment_date_0, 1, 0))
	
	# remove extra cols 
	df = df |> dplyr::select(!assessment_date_0)
	
	# relocate src to end
	df = df |> dplyr::relocate(!!rlang::sym(stringr::str_c(prefix, "src")), .after = dplyr::last_col())
	
	# add censoring date
	if (!is.na(cd))  df = df |> dplyr::mutate(!!var_df := dplyr::if_else(!!var_bin==0, cd, !!var_df))
	
	# return
	return(df)
}


#' Get date first for cancer registry data
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name get_cancer_registry_df
#'
#' @noRd

get_cancer_registry_df <- function(
	codes_df,
	ukb_dat,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	if (verbose) cat("Getting cancer registry data\n")
	
	# format codes 
	vocab_col = "vocab_id"
	codes_col = "code"

	codes <- codes_df |>
		dplyr::filter(!!rlang::sym(vocab_col) == "ICD10") |>
		dplyr::select(!!rlang::sym(codes_col)) |>
		dplyr::pull() |>
		unique() |>
		stringr::str_remove(stringr::fixed(".")) |> 
		stringr::str_sub(1, 5)
	codes_string = stringr::str_flatten(codes, collapse = "|")
	
	# create empty vars in ukb_dat to modify
	ukb_dat$canreg    <- 0
	ukb_dat$canreg_df <- NA
	
	# for this instance, check if participant self-reported this code and record which array
	
	# Number of diagnosis columns
	n_i <- length(unique(ukb_dat$instance))
	
	# Iterate through each diagnosis column
	for (i in 0:(n_i-1)) {
		
		if (verbose) cat("Get cancer registry data from instance ", i, "\n")
	
		# Update where the code matches
		ukb_dat <- ukb_dat |> dplyr::mutate(
			canreg_df = dplyr::if_else(canreg == 0 & stringr::str_detect(icd10, codes_string), date, canreg_df, canreg_df),
			canreg    = dplyr::if_else(canreg == 0 & stringr::str_detect(icd10, codes_string), 1, canreg, canreg)
			)
	}
	
	# keep date first for each participant
	ukb_dat <- ukb_dat |>
		dplyr::filter(!is.na(canreg_df)) |>
		dplyr::group_by(eid) |>
		dplyr::summarize(canreg_df=min(canreg_df, na.rm=TRUE)) |>
		dplyr::mutate(canreg_df = dplyr::if_else(is.finite(canreg_df), canreg_df, NA))
	
	# finish
	if (verbose)  cli::cli_alert_info(c("Finished cancer registry: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data
	return(ukb_dat[,c("eid", "canreg", "canreg_df")])
	
}


#' Get date first for self-reported illness data
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name get_selfrep_illness_df
#'
#' @noRd
get_selfrep_illness_df <- function(
	codes_df,
	ukb_dat,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	if (verbose) cat("Getting self-reported illness data\n")
	
	# format codes 
	vocab_col = "vocab_id"
	codes_col = "code"
	
	# get codes
	codes_cancer <- codes_noncancer <- NULL
	if (any(codes_df$vocab_id == "ukb_cancer"))     codes_cancer = codes_df$code[ codes_df$vocab_id == "ukb_cancer" ]
	if (any(codes_df$vocab_id == "ukb_noncancer"))  codes_noncancer = codes_df$code[ codes_df$vocab_id == "ukb_noncancer" ]
	
	# split instance and array
	ukb_dat <- ukb_dat |> 
		tidyr::separate_wider_delim(instance, delim = "_", names = c("instance", "array")) |>
		dplyr::mutate(
			instance = stringr::str_replace_all(instance, "i", ""),
			array = stringr::str_replace_all(array, "a", "")
		)
	
	# create empty vars in ukb_dat to modify
	ukb_dat$selfrep    <- 0
	ukb_dat$selfrep_df <- NA
	ukb_dat$selfrep_i  <- NA
	
	# for each instance, check if participant self-reported this code and record which array
	
	# Update where the code matches
	if (!is.null(codes_cancer))  {
		ukb_dat <- ukb_dat |> dplyr::mutate(
			selfrep_i  = dplyr::if_else(selfrep == 0 & cancer_code %in% codes_cancer, stringr::str_c(instance, "_cancer"), selfrep_i, selfrep_i),
			selfrep_df = dplyr::if_else(selfrep == 0 & cancer_code %in% codes_cancer, cancer_year, selfrep_df, selfrep_df),
			selfrep    = dplyr::if_else(selfrep == 0 & cancer_code %in% codes_cancer, 1, selfrep, selfrep)
			)
	}
	if (!is.null(codes_noncancer))  {
		ukb_dat <- ukb_dat |> dplyr::mutate(
			selfrep_i  = dplyr::if_else(selfrep == 0 & noncancer_code %in% codes_noncancer, stringr::str_c(instance, "_noncancer"), selfrep_i, selfrep_i),
			selfrep_df = dplyr::if_else(selfrep == 0 & noncancer_code %in% codes_noncancer, noncancer_year, selfrep_df, selfrep_df),
			selfrep    = dplyr::if_else(selfrep == 0 & noncancer_code %in% codes_noncancer, 1, selfrep, selfrep)
			)
	}
	
	# determine earliest date
	ukb_dat <- ukb_dat |>
		dplyr::filter(!is.na(selfrep_df)) |>
		dplyr::group_by(eid) |>
		dplyr::slice(which.min(selfrep_df)) |>
		dplyr::ungroup()
	
	# make sure date is actually a date and not year
	if (! lubridate::is.Date(ukb_dat$selfrep_df))  ukb_dat <- ukb_dat |> dplyr::mutate(selfrep_df = lubridate::as_date(lubridate::date_decimal(selfrep_df)))
	
	# finish
	if (verbose)  cli::cli_alert_info(c("Finished self-reported illness: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data
	return(ukb_dat[,c("eid", "selfrep", "selfrep_df", "selfrep_i")])
	
}




