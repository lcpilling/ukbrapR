#' Get UK Biobank participant Date First (DF) diagnosis 
#'
#' @description For each participant identify the date of first diagnosis from all available electronic medical records & self-reported data.
#'
#' @return Returns a single, "wide" data frame: the participant data for the requested diagnosis codes with "date first" `_df` variables. One for each source of data, and a combined variable.
#'
#' @author Luke Pilling
#'
#' @name get_df
#'
#' @param diagnosis_list A list of data frames. The participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, and `gp_clinical`.
#' @param include_selfrep logical. Include self-reported diagnosesin the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_gp_clinical logical. Include the GP data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_hesin logical. Include the HES data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_death_cause logical. Include the cause of death in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param prefix String. Prefix to add to variable names (e.g., if prefix="chd") the output variables would be "chd_selfrep_df", "chd_df" etc.
#'        \code{default=NULL}
#' @param group_by String. If the codes list provided to `get_emr()` (i.e., in diagnosis_list[['codes_df']]) contained a grouping variable, indicate the variable name here. 
#'        "Date first" variables will be created for each prefix in the grouping variable. The `prefix` option is ignored, in favour of the names in the grouping variable.
#'        \code{default=NULL}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' ###############################################
#' # example 1. haemochromatosis
#' print(codes_df_hh)
#'
#' # get diagnosis data - returns list of data frames (one per source)
#' diagnosis_list <- get_emr(codes_df_hh)
#'
#' # get self-reported illess data - returns a data frame
#' selfrep_df <- get_selfrep_illness(codes_df_hh)
#'
#' # add self-reported to the `diagnosis_list` object
#' diagnosis_list[["selfrep"]] <- selfrep_df
#'
#' # for each participant, get Date First diagnosed with the condition
#' diagnosis_df = get_df(diagnosis_list, prefox="hh")
#'
#' ###############################################
#' # example 2. get multiple diseases at once
#'
#' codes = rbind(codes_df_hh, codes_df_ckd)
#' print(codes)
#'
#' # get diagnosis data - returns list of data frames (one per source)
#' diagnosis_list <- get_emr(codes)
#'
#' # for each participant, get Date First diagnosed with the condition
#' diagnosis_df = get_df(diagnosis_list, group_by="condition")
#'
#' @export
#'
get_df <- function(
	diagnosis_list,
	include_selfrep = TRUE,
	include_gp_clinical = TRUE,
	include_hesin = TRUE,
	include_death_cause = TRUE,
	prefix = NULL,
	group_by = NULL,
	verbose = FALSE
)  {
	
	# are we using a grouping variable?
	if (is.null(group_by))  {
		
		ukbrapR:::get_df1(
			diagnosis_list=diagnosis_list, 
			include_selfrep=include_selfrep, include_gp_clinical=include_gp_clinical, include_hesin=include_hesin, include_death_cause=include_death_cause,
			prefix=prefix, verbose=verbose
		)
		
	} else {
		
		if (verbose) cli::cli_alert("Grouping variable detected - checking codes")
		
		# check input codes and group variable
		if (class(diagnosis_list) != "ukb_emr")  cli::cli_warning(c("{.var diagnosis_list} should be of class {.cls ukb_emr}", "x" = "You've supplied a {.cls {class(diagnosis_list)}} - behaviour may not be as intended."))
		
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
				if (any(codes_sub$vocab_id == "Read2"))  Read2s = codes_sub$code[codes_sub$vocab_id == "Read2"]
				if (any(codes_sub$vocab_id == "CTV3"))   CTV3s  = codes_sub$code[codes_sub$vocab_id == "CTV3"]
				diagnosis_list_sub$gp_clinical = diagnosis_list_sub$gp_clinical |> dplyr::filter(read_2 %in% !!Read2s | read_3 %in% !!CTV3s) 
			}
			
			## hesin_diag
			if (!is.null(diagnosis_list_sub$hesin_diag) & any(codes_sub$vocab_id == "ICD10"))  {  
				ICD10s = ""
				if (any(codes_sub$vocab_id == "ICD10"))  {
					ICD10s <- codes_sub |>
						dplyr::filter(vocab_id == "ICD10") |>
						dplyr::select(code) |>
						dplyr::pull() |>
						unique() |>
						stringr::str_remove(stringr::fixed(".")) |> 
						stringr::str_sub(1, 5)
				}
				ICD10_search = stringr::str_flatten(ICD10s, collapse = "|")
				colnames(diagnosis_list_sub$hesin_diag) = tolower(colnames(diagnosis_list_sub$hesin_diag))
				diagnosis_list_sub$hesin_diag = diagnosis_list_sub$hesin_diag |> dplyr::filter(stringr::str_detect(diag_icd10, !! ICD10_search))
			}
			
			## death_cause
			if (!is.null(diagnosis_list_sub$death_cause) & any(codes_sub$vocab_id == "ICD10"))  {  
				ICD10s = ""
				if (any(codes_sub$vocab_id == "ICD10"))  {
					ICD10s <- codes_sub |>
						dplyr::filter(vocab_id == "ICD10") |>
						dplyr::select(code) |>
						dplyr::pull() |>
						unique() |>
						stringr::str_remove(stringr::fixed(".")) |> 
						stringr::str_sub(1, 5)
				}
				ICD10_search = stringr::str_flatten(ICD10s, collapse = "|")
				diagnosis_list_sub$death_cause = diagnosis_list_sub$death_cause |> dplyr::filter(stringr::str_detect( cause_icd10, !! ICD10_search))
			}
			
			# get DF for this condition
			df_tbl_sub = ukbrapR:::get_df1(
				diagnosis_list=diagnosis_list_sub, 
				include_selfrep=include_selfrep, include_gp_clinical=include_gp_clinical, include_hesin=include_hesin, include_death_cause=include_death_cause,
				prefix=group, verbose=verbose
			)
			
			# merge with main DF table
			if (is.null(df_tbl))  {
				df_tbl = df_tbl_sub
			} else {
				df_tbl = dplyr::full_join(df_tbl, df_tbl_sub, by="eid")
			}
			
		}
		
		cli::cli_alert_success("Finished getting date first diagnosed for each group/condition.")
		cli::cli_alert_info("Warnings for a small number of participants are common and indicate missing dates.")
		
		# return combined table
		return(df_tbl)
		
	}
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
#' @param diagnosis_list A list of data frames. The participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, and `gp_clinical`.
#' @param include_selfrep logical. Include self-reported diagnosesin the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_gp_clinical logical. Include the GP data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_hesin logical. Include the HES data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_death_cause logical. Include the cause of death in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param prefix String. Prefix to add to variable names (e.g., if prefix="chd") the output variables would be "chd_selfrep_df", "chd_df" etc.
#'        \code{default=NULL}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # for each participant, get Date First diagnosed with the condition
#' diagnosis_df = get_df1(diagnosis_list, prefix="cdk")
#'
#' @noRd
get_df1 <- function(
	diagnosis_list,
	include_selfrep = TRUE,
	include_gp_clinical = TRUE,
	include_hesin = TRUE,
	include_death_cause = TRUE,
	prefix = NULL,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	# Check input
	if (verbose) cat("Check inputs\n")
	if (class(diagnosis_list) != "ukb_emr")  warning("This is designed to use output from function `get_emr()` - may not work properly\n")
	
	use_selfrep <- use_gp_clinical <- use_hesin <- use_death_cause <- TRUE
	if ( is.null(diagnosis_list$selfrep) )      use_selfrep <- FALSE
	if ( is.null(diagnosis_list$gp_clinical) )  use_gp_clinical <- FALSE
	if ( is.null(diagnosis_list$hesin_diag) )   use_hesin <- FALSE
	if ( is.null(diagnosis_list$death_cause) )  use_death_cause <- FALSE
	
	#
	#
	#
	
	# Convert gp_clinical to "wide" Date First
	if (use_gp_clinical)  {
		if (verbose) cat("Get date first diagnosis: gp_df\n")
		gp_clinical <- diagnosis_list$gp_clinical |>
			dplyr::group_by(eid) |>
			dplyr::summarize(gp_df=min(event_dt, na.rm=TRUE)) |>
			dplyr::mutate(gp_df = dplyr::if_else(is.finite(gp_df), gp_df, NA))
	}
	
	# Convert hesin_diag to "wide" Date First
	if (use_hesin)  {
		if (verbose) cat("Get date first diagnosis: hes_df\n")
		hesin_diag <- diagnosis_list$hesin_diag |>
			dplyr::mutate(diagnosis_date = epistart) |>
			dplyr::mutate(diagnosis_date = dplyr::if_else(is.na(diagnosis_date), epiend, diagnosis_date)) |>
			dplyr::mutate(diagnosis_date = dplyr::if_else(is.na(diagnosis_date), admidate, diagnosis_date)) |>
			dplyr::mutate(diagnosis_date = lubridate::as_date(dplyr::if_else(is.na(diagnosis_date), disdate, diagnosis_date))) |>
			dplyr::group_by(eid) |>
			dplyr::summarize(hes_df=min(diagnosis_date, na.rm=TRUE)) |>
			dplyr::mutate(hes_df = dplyr::if_else(is.finite(hes_df), hes_df, NA)) 
	}
	
	# Convert death_cause to "wide" Date First
	if (use_death_cause)  {
		if (verbose) cat("Get date first diagnosis: death_df\n")
		death_cause <- diagnosis_list$death_cause |>
			dplyr::group_by(eid) |>
			dplyr::summarize(death_df=min(date_of_death, na.rm=TRUE)) |>
			dplyr::mutate(death_df = dplyr::if_else(is.finite(death_df), death_df, NA))
	}
	
	#
	#
	#
	
	# Combine into single data frame
	if (verbose) cat("Combine into single wide data frame\n")
	diagnosis_df <- NULL
	if (use_selfrep)  {
		diagnosis_df <- na.omit(diagnosis_list$selfrep)
	}
	if (use_gp_clinical)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- gp_clinical
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, gp_clinical, by="eid")
		}
	}
	if (use_hesin)  {
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
	
	#
	#
	#
	
	# Combined "date first, any source" variable & "source" variable
	if (verbose) cat("Combined \"date first, any source\" variable\n")
	diagnosis_df$df <- NA
	diagnosis_df$src <- ""
	
	if (include_selfrep & use_selfrep)  {
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
	
	if (include_hesin & use_hesin)  {
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
	
	# if src & df are empty drop these rows
	diagnosis_df = diagnosis_df |> dplyr::filter(src!="" & (!is.na(df) | !is.na(gp_df) | !is.na(hes_df) | !is.na(death_df)))
	
	# adding variable name prefix?
	if (!is.null(prefix))  {
		if (is.character(prefix) & length(prefix) == 1)  {
			names(diagnosis_df)[2:ncol(diagnosis_df)] = stringr::str_c(prefix, "_", names(diagnosis_df)[2:ncol(diagnosis_df)])
		} else {
			warning("Prefix was not a single string - variables names left as default")
		}
	}
	
	#
	#
	#
	
	if (verbose)  cat("Done. Time taken:", Sys.time() - start_time, "\n")
	
	diagnosis_df_nrow = nrow(diagnosis_df)
	if (is.null(prefix))   cli::cli_alert_success("Identified date of first diagnosis in {diagnosis_df_nrow} participants.")
	if (!is.null(prefix))  cli::cli_alert_success("Identified date of first {prefix} diagnosis in {diagnosis_df_nrow} participants.")
	
	# Return data frame
	diagnosis_df
	
}


