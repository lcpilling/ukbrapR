#' Get UK Biobank participant self-reported illness/year data for specific codes
#'
#' @author Luke Pilling
#'
#' @name get_selfrep_illness
#'
#' @noRd
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
	#if (all(c("ukb_cancer","ukb_noncancer") %in% codes_df[,vocab_col]))  stop("`vocab_col` column in `codes_df` needs to contain either 'ukb_cancer' or 'ukb_noncancer' (not both)")
	
	# get codes
	codes_cancer    <- codes_df[ codes_df[,vocab_col] == "ukb_cancer", codes_col]
	codes_noncancer <- codes_df[ codes_df[,vocab_col] == "ukb_noncancer" , codes_col]
	if (verbose) cat(" - ", length(unique(codes_cancer)), " cancer codes\n")
	if (verbose) cat(" - ", length(unique(codes_noncancer)), " non-cancer codes\n")
	
	# which are we getting?
	get_noncancer <- get_cancer <- FALSE
	if (length(codes_cancer)>0)     get_cancer    <- TRUE
	if (length(codes_noncancer)>0)  get_noncancer <- TRUE
	
	# remove rows where participant has no data 
	ukb_dat = ukb_dat |> dplyr::filter(
		dplyr::if_any(
			dplyr::starts_with("p"),
			~!is.na(.)
		)
	)
	
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
	
	# use `tidyr::pivot_longer` to reduce the number of columns and increase the number of rows
	if (verbose) cli::cli_alert("Pivot self-report data")
	pivot_table <- function(d, v, n)  {
		d |> 
			dplyr::select(eid, dplyr::contains(v)) |>
			tidyr::pivot_longer(!eid, names_to = "instance", names_prefix = v, values_to = n) |>
			dplyr::filter(!is.na(!!rlang::sym(n)))
	}
	
	# create tmp empty objects 
	ukb_dat_sr <- ukb_dat_noncancer_long <- ukb_dat_cancer_long <- NULL
	
	if (get_cancer)  {
		ukb_dat_code <- pivot_table(ukb_dat, "p20001_", "cancer_code")
		ukb_dat_year <- pivot_table(ukb_dat, "p20006_", "cancer_year")
		ukb_dat_cancer_long <- dplyr::full_join(ukb_dat_code, ukb_dat_year, by = c("eid"="eid", "instance"="instance"))
	}
	if (get_noncancer)  {
		ukb_dat_code <- pivot_table(ukb_dat, "p20002_", "noncancer_code")
		ukb_dat_year <- pivot_table(ukb_dat, "p20008_", "noncancer_year")
		ukb_dat_noncancer_long <- dplyr::full_join(ukb_dat_code, ukb_dat_year, by = c("eid"="eid", "instance"="instance"))
	}
	
	# subset to ukb-codes in provided list
	if (verbose) cli::cli_alert("Identify matching codes")
	if (get_cancer)     ukb_dat_cancer_long    = ukb_dat_cancer_long    |> dplyr::filter(cancer_code %in% codes_cancer)
	if (get_noncancer)  ukb_dat_noncancer_long = ukb_dat_noncancer_long |> dplyr::filter(noncancer_code %in% codes_noncancer)
	
	# join tables if both types of codes provided, otherwise just use one
	if (verbose) cli::cli_alert("Joining tables")
	if (!is.null(ukb_dat_cancer_long) & !is.null(ukb_dat_noncancer_long))  {
		# both code types provided
		ukb_dat_sr <- dplyr::full_join(ukb_dat_cancer_long, ukb_dat_noncancer_long, by = c("eid"="eid", "instance"="instance"))
	} else if (!is.null(ukb_dat_noncancer_long))  {
		# just non-cancer
		ukb_dat_sr <- ukb_dat_noncancer_long
	} else {
		# just cancer
		ukb_dat_sr <- ukb_dat_cancer_long
	}
	
	# finish
	#if (verbose)  cli::cli_alert_success(c("Finished cancer registry: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data
	return(ukb_dat_sr)
	
}

