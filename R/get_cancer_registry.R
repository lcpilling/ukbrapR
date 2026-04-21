#' Get cancer registry data for specific codes
#'
#' @author Luke Pilling
#'
#' @name get_cancer_registry
#'
#' @noRd
get_cancer_registry <- function(
	ICD9s,
	ICD10s,
	ukb_dat,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	# Check input
	if (verbose & ICD9s[1]!="")  cli::cli_alert_info("Searching cancer registry data for {length(unique(codes))} ICD9 codes")
	if (verbose & ICD10s[1]!="") cli::cli_alert_info("Searching cancer registry data for {length(unique(codes))} ICD10 codes")
	
	# if "missing" (empty string) replace with impossible code so grep doesn't catch all rows 
	if (ICD9s[1]=="")   ICD9s <- "not_a_code"
	if (ICD10s[1]=="")  ICD10s <- "not_a_code"
	
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
	v_icd9      <- "p40013_"
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
	
	# some older exports may not have icd9
	ukb_dat_icd9 <- NULL
	if ("" %in% colnames(ukb_dat))  {
		ukb_dat_icd9      <- pivot_cancer(ukb_dat, v_icd9, "icd9")
	} else {
		cli::cli_alert_warning("'icd9' not in exported cancer registry data. Consider re-exporting raw tables with `export_tables()`")
	}

	# join tables
	if (verbose) cli::cli_alert("Join cancer registry data")
	ukb_dat_cr = purrr::reduce(list(ukb_dat_icd10, ukb_dat_date, ukb_dat_age, ukb_dat_histology, ukb_dat_behaviour), dplyr::full_join, by = c("eid"="eid", "instance"="instance"))
	if (!is.null(ukb_dat_icd9))  {
		ukb_dat_cr <- dplyr::full_join(ukb_dat_icd9, ukb_dat_cr)
	}  else  {
		ukb_dat_cr$icd9 <- NA
	}
	
	# remove rows where participant has no cancer data 
	ukb_dat_cr = ukb_dat_cr |> dplyr::filter(
		dplyr::if_any(
			c("icd9","icd10","date","age","histology","behaviour"),
			~!is.na(.)
		)
	)
	
	# subset to ICD9s/ICD10s in provided codes
	if (verbose) cli::cli_alert("Identify matching codes")
	ukb_dat_cr = ukb_dat_cr |> 
		dplyr::filter(
			stringr::str_detect(
				icd9,
				stringr::str_flatten(ICD9s, collapse = "|")
			) |
			stringr::str_detect(
				icd10,
				stringr::str_flatten(ICD10s, collapse = "|")
			)
		)
	
	# finish
	#if (verbose)  cli::cli_alert_success(c("Finished cancer registry: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))
	
	# Return data
	return(ukb_dat_cr)
	
}

