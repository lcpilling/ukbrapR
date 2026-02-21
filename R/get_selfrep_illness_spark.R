#' Get UK Biobank participant self-reported illness/year data for specific codes
#'
#' @description For a specific self-reported illness code or codes, identify whether the participant has self-reported at any visit, and identify the year.
#' Intended for use on the UK Biobank DNnexus Research Analysis Platform, but if the user provides their own dataframe of UK Biobank self-reported fields this works on any system.
#'
#' @return Returns a data frame with four variables: eid, selfrep [binary, codes identified?], selfrep_df [date of reported illness], selfrep_i [instance the illness was first reported]
#'
#' @author Luke Pilling
#'
#' @name get_selfrep_illness_spark
#'
#' @param codes_df A data frame. Contains the `vocab_col` and `codes_col` i.e., an indicator of the vocabulary and the diagnostic codes.
#' @param vocab_col A string. Column name in `codes_df` that contains the vocabulary indicator for the code (for self-reported it needs to be either "ukb_cancer" or "ukb_noncancer").
#'        \code{default='vocab_id'}
#' @param codes_col A string. Column name in `codes_df` that contains the self-reported disease code (e.g., 1507).
#'        \code{default='code'}
#' @param ukb_dat A data frame. Optional. If not provided, will get the phenotypes from the RAP. Contains the self-reported illness fields e.g., `p20008_i0_a0`.
#'        \code{default=NULL}
#' @param n_cancer_arrays An integer. It is not trivial to determine the max number of arrays to request from Spark for the self-reported illnesses. The defaults match the currently (Feb 2024) available data but may need increasing in the future.
#'        \code{default=5}
#' @param n_noncancer_arrays An integer. It is not trivial to determine the max number of arrays to request from Spark for the self-reported illnesses. The defaults match the currently (Feb 2024) available data but may need increasing in the future.
#'        \code{default=30}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for haemochromatosis
#' print(codes_df_hh)
#'
#' # get self-reported data - a data frame
#' selfrep_df <- get_selfrep_illness_spark(codes_df_hh)
#'
#' # inspect variables
#' table(selfrep_df$selfrep)
#' summary(selfrep_df$selfrep_df)
#' table(selfrep_df$selfrep_i)
#'
#' @export
#'
get_selfrep_illness_spark <- function(
	codes_df,
	vocab_col = "vocab_id",
	codes_col = "code",
	ukb_dat = NULL,
	n_cancer_arrays = 5,
	n_noncancer_arrays = 30,
	verbose = FALSE
)  {

	# start up messages
  pkg_version <- utils::packageVersion("ukbrapR")
  cli::cli_alert_info("{.pkg ukbrapR} v{pkg_version}")
  .ukbrapr_startup_notice()

	start_time <- Sys.time()

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
	cat("Getting data on ", length(unique(codes)), " codes\n")

	# is cancer?
	is_cancer <- TRUE
	if ("ukb_noncancer" %in% codes_df[,vocab_col])  is_cancer <- FALSE

	# if `ukb_dat` is NULL use `get_rap_phenos()` to get phenotype data - all instances, all arrays
	#    need: eid, 20001, 20002, 20006, 20007, 20008, 20009
	if (is.null(ukb_dat))  {

		if (verbose) cat("No `ukb_dat` object provided - will get from RAP\n")

		# Determine variable names needed (depends if cancer or non-cancer)
		#   will use 20001 (cancer code) and 20002 (non-cancer code)
		#   will use the interpolated year (20006 = cancer year, 20008 = non-cancer year)

		# RAP stores arrays as separate variables

		# get field names
		if (verbose) cat("Determine field names to request\n")
		names = "eid"

		# if cancer or non-cancer code:
		if (is_cancer)  {
			#   cancer code = 20001
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 0, "_a", a))
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 1, "_a", a))
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 2, "_a", a))
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 3, "_a", a))
			#   cancer year = 20006
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 0, "_a", a))
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 1, "_a", a))
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 2, "_a", a))
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 3, "_a", a))
		}  else  {
			#   non-cancer illness code = 20002
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 0, "_a", a))
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 1, "_a", a))
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 2, "_a", a))
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 3, "_a", a))
			#   non-cancer illness year = 20008
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 0, "_a", a))
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 1, "_a", a))
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 2, "_a", a))
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 3, "_a", a))
		}

		if (verbose) print(names)

		# get fields from the RAP
		if (verbose) cat("Get fields from the RAP\n")
		ukb_dat <- ukbrapR::get_rap_phenos(names, value_coding = "raw", verbose = verbose)

	}  else  {
		cat("User is providing own `ukb_dat` with self-reported illness fields\n")
	}

	# remove any cols with all missing values
	ukb_dat = ukb_dat |> dplyr::select_if(~ !all(is.na(.)))

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
	if (verbose)  cat("Done. Time taken:", Sys.time() - start_time, "\n")

	# Return data
	return(ukb_dat[,c("eid", "selfrep", "selfrep_df", "selfrep_i")])

}

