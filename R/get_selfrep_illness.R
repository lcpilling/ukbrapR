#' Get UK Biobank participant self-reported illness/year data for specific codes
#'
#' @description For a specific self-reported illness code or codes, identify whether the participant has self-reported at any visit, and identify the year. 
#'
#' @return Returns a data frame with four variables: eid, selfrep [binary, codes identified?], selfrep_df [date of reported illness], selfrep_i [instance the illness was first reported]
#'
#' @author Luke Pilling
#'
#' @name get_selfrep_illness
#'
#' @param codes_df A data frame. Contains the `vocab_col` and `codes_col` i.e., an indicator of the vocabulary and the diagnostic codes.
#' @param ukb_dat A data frame. Contains the participant-level self-reported illness fields e.g., `p20008_i0_a0`.
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
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
#' @export
#'
get_selfrep_illness <- function(
	codes_df,
	ukb_dat,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	vocab_col = "vocab_id"
	codes_col = "code"
	
	# Check input
	if (verbose) cat("Check inputs\n")
	if (! any(class(codes_df) %in% c("data.frame","tbl_df")))  stop("Codelist needs to be provided as a data frame")
	codes_df = as.data.frame(codes_df)  # in case a tibble
	
	# check `codes_df` -- does it contain "ukb_cancer" or "ukb_noncancer" codes? Cannot have both. Makes no sense.
	if (! any(c("ukb_cancer","ukb_noncancer") %in% codes_df[,vocab_col]))  stop("`vocab_col` column in `codes_df` needs to contain either 'ukb_cancer' or 'ukb_noncancer'")
	if (all(c("ukb_cancer","ukb_noncancer") %in% codes_df[,vocab_col]))  stop("`vocab_col` column in `codes_df` needs to contain either 'ukb_cancer' or 'ukb_noncancer' (not both)")
	
	# get codes
	if (verbose) cat("Get codes, determine if cancer\n")
	codes <- codes_df[ codes_df[,vocab_col] %in% c("ukb_cancer","ukb_noncancer") ,codes_col]
	if (verbose) cat("Getting data on ", length(unique(codes)), " codes\n")
	
	# is cancer?
	is_cancer <- TRUE
	if ("ukb_noncancer" %in% codes_df[,vocab_col])  is_cancer <- FALSE
	
	# check all visits for participant - create `selfrep` (binary, ever), `selfrep_df` (the "Date First" variable) and `selfrep_i` (the "instance" i.e., visit)
	#   https://biobank.ctsu.ox.ac.uk/crystal/label.cgi?id=100074
	#   will use 20001 (cancer code) and 20002 (non-cancer code)
	#   will use 20006-9 the interpolated year/age (6 = cancer year, 8 = non-cancer year)
	#   interpolated:
	#     - If the participant gave a calendar year, then the best-fit time is half-way through that year. For example if the year was given as 1970, then the value presented is 1970.5
	#     - If the participant gave their age then the value presented is the fractional year corresponding to the mid-point of that age. For example, if the participant said they were 30 years old then the value is the date at which they were 30years+6months.
	#     - Interpolated values before the date of birth were truncated forwards to that time.
	#     - Interpolated values after the time of data acquisition were truncated back to that time.
	#   [-1] = "Date uncertain or unknown" and [-3] = "Preferred not to answer" (exclude both from DF)
	
	# create empty vars in ukb_dat to modify
	ukb_dat$selfrep    <- 0
	ukb_dat$selfrep_df <- NA
	ukb_dat$selfrep_i  <- NA
	
	# for this instance, check if participant self-reported this code and record which array
	get_selfrep_i <- function(ukb_dat, codes, i, verbose)  {
		
		if (verbose) cat("Get data from instance ", i, "\n")
		
		# variable prefix depends on cancer or not:
		if (is_cancer)  {
			v_diag <- paste0("p20001_i", i)
			v_year <- paste0("p20006_i", i)
		}
		if (!is_cancer)  {
			v_diag <- paste0("p20002_i", i)
			v_year <- paste0("p20008_i", i)
		}
		
		# Number of diagnosis columns
		n_cols <- sum(stringr::str_detect(names(ukb_dat), v_diag))
		
		# Iterate through each diagnosis column
		for (a in 0:(n_cols-1)) {
			
			if (verbose) cat("Get data from instance ", i, " array ", a, "\n")
			
			diag_col <- rlang::sym(paste0(v_diag, "_a", a))
			year_col <- rlang::sym(paste0(v_year, "_a", a))
		
			# Update where the code matches
			ukb_dat <- ukb_dat |> dplyr::mutate(
				selfrep_i  = dplyr::if_else(selfrep == 0 & !!diag_col %in% codes, i, selfrep_i, selfrep_i),
				selfrep_df = dplyr::if_else(selfrep == 0 & !!diag_col %in% codes, !!year_col, selfrep_df, selfrep_df),
				selfrep    = dplyr::if_else(selfrep == 0 & !!diag_col %in% codes, 1, selfrep, selfrep)
				)
		}
		
		return(ukb_dat)
	
	}
	
	# update selfrep variables with data from each instance
	ukb_dat = get_selfrep_i(ukb_dat, codes, 0, verbose)
	ukb_dat = get_selfrep_i(ukb_dat, codes, 1, verbose)
	ukb_dat = get_selfrep_i(ukb_dat, codes, 2, verbose)
	ukb_dat = get_selfrep_i(ukb_dat, codes, 3, verbose)
	
	if (verbose) cat("Format date first variable\n")
	
	# replace unknown or missing years with NA
	ukb_dat = dplyr::mutate(ukb_dat, selfrep_df = dplyr::if_else(selfrep_df < 0, NA, selfrep_df))
	
	# convert decimal to date
	ukb_dat = dplyr::mutate(ukb_dat, selfrep_df = lubridate::as_date(lubridate::date_decimal(selfrep_df)))
	
	# finish
	if (verbose)  if (verbose)  cli::cli_alert_info(c("Finished self-reported illness: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data
	return(ukb_dat[,c("eid", "selfrep", "selfrep_df", "selfrep_i")])
	
}

