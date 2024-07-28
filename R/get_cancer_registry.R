#' Get UK Biobank participant self-reported illness/year data for specific codes
#'
#' @description For a specific self-reported illness code or codes, identify whether the participant has self-reported at any visit, and identify the year. 
#' Intended for use on the UK Biobank DNnexus Research Analysis Platform, but if the user provides their own dataframe of UK Biobank self-reported fields this works on any system.
#'
#' @return Returns a data frame with four variables: eid, selfrep [binary, codes identified?], selfrep_df [date of reported illness], selfrep_i [instance the illness was first reported]
#'
#' @author Luke Pilling
#'
#' @name get_cancer_registry
#'
#' @param codes A vector. The ICD10 codes to ascertain cancer registry entires
#' @param ukb_dat A data frame. Contains the participant-level cancer registry fields e.g., `p40005_i0`
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for haemochromatosis
#' print(codes_df_hh)
#'
#' # get self-reported data - a data frame
#' selfrep_df <- get_selfrep_illness(codes_df_hh)
#'
#' # inspect variables
#' table(selfrep_df$selfrep)
#' summary(selfrep_df$selfrep_df)
#' table(selfrep_df$selfrep_i)
#'
#' @noRd
get_cancer_registry <- function(
	codes,
	ukb_dat,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	# Check input
	if (verbose) cli::cli_alert_info("Searching cancer registry data for {length(unique(codes))} ICD10 codes")
	
	# remove rows where participant has no cancer data 
	ukb_dat = ukb_dat |> dplyr::filter(
		dplyr::if_any(
			dplyr::starts_with("p"),
			~!is.na(.)
		)
	)
	
	# check all visits for participant - create `canreg` (binary, ever), `canreg_df` (date first) and `canreg_i` (the "instance" i.e., visit)
	#   https://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=100092
	#   date vars = 40005
	#   cancer vars = 40006
	#   age vars = 40008
	#   histology vars = 40011
	#   behaviour vars = 40012
	
	# variable prefix 
	v_icd10     <- "p40006_"
	v_date      <- "p40005_"
	v_age       <- "p40008_"
	v_histology <- "p40011_"
	v_behaviour <- "p40012_"
	
	# use `tidyr::pivot_longer` to reduce the number of columns and increase the number of rows
	if (verbose) cli::cli_alert("Pivot cancer registry data")
	pivot_cancer <- function(d, v, n)  {
		d |> 
			dplyr::select(eid, dplyr::contains(v)) |>
			tidyr::pivot_longer(!eid, names_to = "instance", names_prefix = v, values_to = n)
	}
	ukb_dat_icd10     <- pivot_cancer(ukb_dat, v_icd10, "icd10")
	ukb_dat_date      <- pivot_cancer(ukb_dat, v_date, "date")
	ukb_dat_age       <- pivot_cancer(ukb_dat, v_age, "age")
	ukb_dat_histology <- pivot_cancer(ukb_dat, v_histology, "histology")
	ukb_dat_behaviour <- pivot_cancer(ukb_dat, v_behaviour, "behaviour")
	
	# join tables
	if (verbose) cli::cli_alert("Join cancer registry data")
	ukb_dat_cr = purrr::reduce(list(ukb_dat_icd10, ukb_dat_date, ukb_dat_age, ukb_dat_histology, ukb_dat_behaviour), dplyr::full_join, by = c("eid"="eid", "instance"="instance"))
	
	# remove rows where participant has no cancer data 
	ukb_dat_cr = ukb_dat_cr |> dplyr::filter(
		dplyr::if_any(
			c("icd10","date","age","histology","behaviour"),
			~!is.na(.)
		)
	)
	
	# subset to ICD10s in provided codes
	if (verbose) cli::cli_alert("Identify matching codes")
	ukb_dat_cr = ukb_dat_cr |> 
		dplyr::filter(
			stringr::str_detect(
				icd10,
				stringr::str_flatten(codes, collapse = "|")
			)
		)
	
	# finish
	#if (verbose)  cli::cli_alert_success(c("Finished cancer registry: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data
	return(ukb_dat_cr)
	
}

