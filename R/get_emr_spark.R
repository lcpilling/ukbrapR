#' Get UK Biobank participant Electronic Medical Records (EMR) data in a RAP Spark environment
#'
#' @description 
#' 
#' This function is not maintained. Better to use `get_diagnoses()`.
#' 
#' Using a Spark node/cluster on the UK Biobank Research Analysis Platform (DNAnexus), use R to get medical records for specific diagnostic codes list
#'
#' @return Returns a list of data frames (the participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, and `gp_clinical`. Also includes the original codes list)
#'
#' @author Luke Pilling
#'
#' @name get_emr_spark
#'
#' @param codes_df A data frame. Contains two columns: `code` and `vocab_id` i.e., a list of diagnostic codes, and an indicator of the vocabulary. Other columns are ignored.
#' @param spark_master A string. The `master` argmuent passed to `sparklyr::spark_connect()`.
#'        \code{default='spark://master:41000'}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for CKD from GEMINI multimorbidity project
#' head(codes_df_ckd)
#'
#' # get EMR data - returns list of data frames (one per source)
#' emr_dat <- get_emr(codes_df_ckd)
#'
#' # save to files on the RAP worker node -- either as an R object, or separate as text files:
#' save(emr_dat, "ukbrap.CKD.emr.20231114.RDat")
#' readr::write_tsv(emr_dat$hesin_diag,  "ukbrap.CKD.hesin_diag.20231114.txt.gz")
#' 
#' # upload data to RAP storage
#' upload_to_rap(file="ukbrap.CKD.*.20231114.*", dir="")
#'
#' @export
#'
get_emr_spark <- function(
	codes_df,
	spark_master = "spark://master:41000",
	verbose=FALSE
)  {
	
  lifecycle::deprecate_warn("0.2.0", "get_emr_spark()", "get_diagnoses()", details="Spark functions are no longer maintained any may contain bugs compared to newer functions.")
  
	start_time <- Sys.time()

	vocab_col = "vocab_id"
	codes_col = "code"

	# Connect to Spark
	cli::cli_alert("Connecting to Spark {spark_master} -- if this fails you might be on the wrong cluster/server")
	sc <- sparklyr::spark_connect(master = spark_master)
	

	# Check input
	if (verbose) cat("Check inputs\n")
	if (! any(class(codes_df) %in% c("data.frame","tbl_df")))  stop("Codelist needs to be provided as a data frame")
	codes_df = as.data.frame(codes_df)  # in case a tibble

	if (! vocab_col %in% colnames(codes_df))  stop("Codelist data frame needs to include vocabulary column `vocab_id`")
	if (! codes_col %in% colnames(codes_df))  stop("Codelist data frame needs to include codes colum `code`")

	if (! any(c("ICD10","Read2","CTV3") %in% codes_df[,vocab_col]))  stop("Vocabularies need to include ICD10, Read2 or CTV3")


	# Check code lists - only first 5 digits are used by UK Biobank
	cli::cli_alert("Checking provided codes (remember only the first 5 digits are used by UK Biobank)")
	get_icd10 <- FALSE
	get_read2 <- FALSE
	get_ctv3  <- FALSE
	ICD10s    <- ""
	Read2s    <- ""
	CTV3s     <- ""
	
	# get ICD10s. Remove "." dot characters. First 5 characters only. Add "%" suffix to each for SQL LIKE query
	if (any(codes_df[,vocab_col] == "ICD10"))  {
		get_icd10 <- TRUE
		ICD10s    <- codes_df |>
			dplyr::filter(!!rlang::sym(vocab_col) == "ICD10") |>
			dplyr::select(!!rlang::sym(codes_col)) |>
			dplyr::pull() |>
			unique() |>
			stringr::str_remove(stringr::fixed(".")) |> 
			stringr::str_sub(1, 5) |> 
			stringr::str_c("%")
		cat(" - N unique ICD10 codes:", length(ICD10s), "\n")
	}

	# get Read2 and CTV3s. First 5 characters only. 
	if (any(codes_df[,vocab_col] == "Read2"))  {
		get_read2 <- TRUE
		Read2s    <- codes_df[ codes_df[,vocab_col] == "Read2" , codes_col ]
		Read2s    <- stringr::str_sub(Read2s, 1, 5) |> unique()
		cat(" - N unique Read2 codes:", length(Read2s), "\n")
	}
	if (any(codes_df[,vocab_col] == "CTV3"))  {
		get_ctv3 <- TRUE
		CTV3s    <- codes_df[ codes_df[,vocab_col] == "CTV3" , codes_col ]
		CTV3s    <- stringr::str_sub(CTV3s, 1, 5) |> unique()
		cat(" - N unique CTV3 codes:", length(CTV3s), "\n")
	}

	#
	#
	#
	
	# Get app database ID
	if (verbose) cat("Get app database ID\n")
	app_id <- system("dx describe *dataset | grep  app | awk -F ' ' '{print $2}' | sort | head -n 1", intern = TRUE) |>
		stringr::str_replace(".dataset", "")
	if (verbose) cat(" - [", app_id, "]\n")
	
	# Connect Spark to this database
	if (verbose) cat("Connect Spark to this database\n")
	sparklyr::tbl_change_db(sc, app_id) 
	
	# List tables
	#DBI::dbListTables(sc)
	
	# Create reference to tables without loading into memory
	if (verbose) cat("Create query for Spark tables without yet loading into memory\n")
	if (get_icd10)  {
	
		# because we want to also match child codes for e.g., chapter codes we will use SQL `LIKE` 
		# need to make the query manually because only fixed patterns are supported on database backends
		# need to join with the main "death" table to get the date
		if (verbose) cat(" - death_cause\n")
		death_cause_query <- stringr::str_c(
			"SELECT death_cause.eid, death_cause.cause_icd10, death_cause.dnx_death_id, death_cause.arr_index, death.date_of_death, death.dsource, death.source ",
			"FROM death_cause ",
			"INNER JOIN death on death_cause.dnx_death_id=death.dnx_death_id ",
			"WHERE cause_icd10 LIKE '",
			stringr::str_flatten(ICD10s, collapse = "' OR cause_icd10 LIKE '"),
			"'"
		)
		death_cause_tbl <- dplyr::tbl(sc, dbplyr::sql(death_cause_query))
	
		# same for hesin_diag, but will join with the episode date info etc.
		if (verbose) cat(" - hesin_diag\n")
		hesin_diag_query <- stringr::str_c(
			"SELECT hesin_diag.eid, hesin_diag.diag_ICD10, hesin_diag.dnx_hesin_id, hesin.epistart, hesin.epiend, hesin.admidate, hesin.disdate ",
			"FROM hesin_diag ",
			"INNER JOIN hesin on hesin_diag.dnx_hesin_id=hesin.dnx_hesin_id ",
			"WHERE diag_icd10 LIKE '",
			stringr::str_flatten(ICD10s, collapse = "' OR diag_icd10 LIKE '"),
			"'"
		)
		hesin_diag_tbl <- dplyr::tbl(sc, dbplyr::sql(hesin_diag_query))
		
	}
	if (get_read2 | get_ctv3)  {
		if (verbose) cat(" - gp_clinical\n")
		gp_clinical_tbl <- dplyr::tbl(sc, "gp_clinical")
		gp_clinical_tbl <- gp_clinical_tbl |> dplyr::filter(read_2 %in% !!Read2s | read_3 %in% !!CTV3s) 
	}
	
	# Collect data to memory
	if (verbose) cat("Collect data to memory\n")
	death_cause <- NULL
	hesin_diag  <- NULL
	gp_clinical <- NULL
	if (get_icd10)  {
		if (verbose) cat(" - death_cause\n")
		death_cause <- sparklyr::collect(death_cause_tbl)
		death_cause$eid <- as.numeric(death_cause$eid)  # can sometimes become CHR 
	
		if (verbose) cat(" - hesin\n")
		hesin_diag <- sparklyr::collect(hesin_diag_tbl)
		hesin_diag$eid <- as.numeric(hesin_diag$eid)  # can sometimes become CHR 
	}
	if (get_read2 | get_ctv3)  {
		if (verbose) cat(" - gp_clinical\n")
		gp_clinical <- sparklyr::collect(gp_clinical_tbl)
		gp_clinical$eid <- as.numeric(gp_clinical$eid)  # can sometimes become CHR 
	}
	
	#
	#
	#
	
	time_taken = as.numeric(difftime(Sys.time(), start_time, units="secs"))
	
	cli::cli_alert_success(c("Finished. Time taken: ", "{prettyunits::pretty_sec(time_taken)}."))
	
	# Return data as list
	output_list <- list(gp_clinical=gp_clinical, hesin_diag=hesin_diag, death_cause=death_cause, codes_df=codes_df)
	class(output_list) <- "ukb_emr"
	return(output_list)
	
}



