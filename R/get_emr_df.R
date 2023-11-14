#' Get UK Biobank participant Date First (DF) diagnosis from Electronic Medical Records (EMR) data 
#'
#' @description For each participant identify the date of first diagnosis for the provided electronic medical records.
#'
#' @return Returns a single, "wide" data frame: the participant data for the requested diagnosis codes with "date first" `_df` variables. One for each source of data, and a combined variable.
#'
#' @author Luke Pilling
#'
#' @name get_emr_df
#'
#' @param diagnosis_list A list of data frames. The participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, and `gp_clinical`.
#' @param include_death_cause logical. Include the cause of death in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_hesin logical. Include the HES data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param include_gp_clinical logical. Include the GP data in the combined Date First output? If present in `diagnosis_list` will still provide a separate `_df` variable
#'        \code{default=TRUE}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for CKD from GEMINI multimorbidity project
#' codes_df <- readr::read_tsv("https://raw.githubusercontent.com/GEMINI-multimorbidity/diagnostic_codes/main/codelists/CKD.txt")
#' codes_df
#'
#' # get diagnosis data - returns list of data frames (one per source)
#' diagnosis_list <- get_emr_diagnoses(codes_df)
#' # 7 ICD10 codes, 40 Read2 codes, 37 CTV3 codes 
#' # 298.18 sec elapsed
#'
#' # for each participant, get Date First diagnosed with the condition
#' diagnosis_df = get_emr_df(diagnosis_list)
#' # 0.9 sec elapsed
#'
#' # save to files on the RAP worker node
#' readr::write_tsv(diagnosis_df, "ukbrap.CKD.date_first.20231114.txt.gz")
#' 
#' # upload data to RAP storage
#' upload_to_rap(file="ukbrap.CKD.date_first.20231114.txt.gz", dir="")
#'
#' @export
#'
get_emr_df <- function(diagnosis_list,
                       include_death_cause = TRUE,
                       include_hesin = TRUE,
                       include_gp_clinical = TRUE,
                       verbose = FALSE)  {
	
	start_time <- Sys.time()

	# Check input
	if (verbose) cat("Check inputs\n")
	if (class(diagnosis_list) != "ukb_emr")  warning("This is designed to use output from function `get_emr_diagnoses()` - may not work properly\n")
	
	use_death_cause = TRUE
	use_hesin = TRUE
	use_gp_clinical = TRUE
	if ( is.null(diagnosis_list$death_cause) )  use_death_cause <- FALSE
	if ( is.null(diagnosis_list$hesin_diag) )   use_hesin <- FALSE
	if ( is.null(diagnosis_list$gp_clinical) )  use_gp_clinical <- FALSE
	
	#
	#
	
	# Do death_cause
	if (use_death_cause)  {
		if (verbose) cat("Get date first diagnosis: death_df\n")
		death_cause <- diagnosis_list$death_cause |>
			dplyr::group_by(eid) |>
			dplyr::summarize(death_df=min(date_of_death, na.rm=TRUE))
	}
	
	# Do hesin_diag
	if (use_hesin)  {
		if (verbose) cat("Get date first diagnosis: hes_df\n")
		hesin_diag <- diagnosis_list$hesin_diag |>
			dplyr::mutate(diagnosis_date = epistart) |>
			dplyr::mutate(diagnosis_date = dplyr::if_else(is.na(diagnosis_date), epiend, diagnosis_date)) |>
			dplyr::mutate(diagnosis_date = dplyr::if_else(is.na(diagnosis_date), admidate, diagnosis_date)) |>
			dplyr::mutate(diagnosis_date = lubridate::as_date(dplyr::if_else(is.na(diagnosis_date), disdate, diagnosis_date))) |>
			dplyr::group_by(eid) |>
			dplyr::summarize(hes_df=min(diagnosis_date, na.rm=TRUE))
	}
	
	# Do gp_clinical
	if (use_gp_clinical)  {
		if (verbose) cat("Get date first diagnosis: gp_df\n")
		gp_clinical <- diagnosis_list$gp_clinical |>
			dplyr::group_by(eid) |>
			dplyr::summarize(gp_df=min(event_dt, na.rm=TRUE))
	}
	
	#
	#
	
	# Combine into single data frame
	if (verbose) cat("Combine into single wide data frame\n")
	diagnosis_df <- NULL
	if (use_death_cause)  diagnosis_df <- death_cause
	if (use_hesin)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- hesin_diag
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, hesin_diag, by="eid")
		}
	}
	if (use_gp_clinical)  {
		if (is.null(diagnosis_df))  {
			diagnosis_df <- gp_clinical
		} else {
			diagnosis_df <- dplyr::full_join(diagnosis_df, gp_clinical, by="eid")
		}
	}
	
	# Combined "date first, any source" variable
	if (verbose) cat("Combined \"date first, any source\" variable\n")
	diagnosis_df$df <- NA
	if (include_death_cause & use_death_cause)  diagnosis_df$df <- diagnosis_df$death_df
	if (include_hesin & use_hesin)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(df = dplyr::case_when(
			!is.na(hes_df) & is.na(df)  ~ hes_df,
			!is.na(hes_df) & !is.na(df) & hes_df<df~ hes_df,
			TRUE ~ df))
	}
	if (include_gp_clinical & use_gp_clinical)  {
		diagnosis_df <- diagnosis_df |> dplyr::mutate(df = dplyr::case_when(
			!is.na(gp_df) & is.na(df)  ~ gp_df,
			!is.na(gp_df) & !is.na(df) & gp_df<df~ gp_df,
			TRUE ~ df))
	}
	
	#
	#
	
	end_time <- Sys.time()
	if (verbose)  cat("Done.\nTime taken:", end_time - start_time, "\n")
	
	# Return data frame
	diagnosis_df

}














