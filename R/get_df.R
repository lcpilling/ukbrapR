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
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for haemochromatosis
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
#' diagnosis_df = get_df(diagnosis_list)
#'
#' # save to files on the RAP worker node
#' save(diagnosis_list, diagnosis_df, file="ukbrap.HH.date_first.20240221.RDat")
#' 
#' # upload data to RAP storage
#' upload_to_rap(file="ukbrap.HH.date_first.20240221.RDat", dir="")
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
	
	# Return data frame
	diagnosis_df
	
}


