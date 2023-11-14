#' Use R on the UK Biobank RAP (Spark cluster) to get phenotype data
#'
#' @description Using a Spark node/cluster on the UK Biobank RAP, run R to extract a provided set of variables. Using code from the UK Biobank DNAnexus team https://github.com/UK-Biobank/UKB-RAP-Notebooks/blob/main/NBs_Prelim/105_export_participant_data_to_r.ipynb
#'
#' @return Returns a data.frame (the participant data for the requested variables)
#'
#' @author Luke Pilling
#'
#' @name get_rap_phenos
#'
#' @param names A string or vector of strings. The variable name(s) required. e.g., c("eid","p31","p21003_i0") (character)
#' @param record A string. The `dnanexus_link` file descriptor of the .dataset to use. Default (if left as NULL) is to use the most recent update (character)
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' # example diagnostic codes for CKD from GEMINI multimorbidity project
#' codes_df <- readr::read_tsv("https://raw.githubusercontent.com/GEMINI-multimorbidity/diagnostic_codes/main/codelists/CKD.txt")
#'
#' # get diagnosis data - returns list of data frames (one per source)
#' diagnoses_list <- get_emr_diagnoses(codes_df, verbose=TRUE)
#' 
#' # save to file on the RAP worker node
#' write_tsv(df, "ukb14631.data_output.20231114.txt.gz")
#' 
#' # upload data to RAP storage
#' ukbrapR::upload_to_rap(file="ukb14631.data_output.20231114.txt.gz", dir="")
#'
#' @export
#'
get_emr_diagnoses <- function(codes_df,
                              vocab_col = "vocab_id",
                              codes_col = "code",
                              spark_master = "spark://master:41000",
                              verbose=FALSE)  {
	
	# Check input
  if (verbose) cat("Check inputs\n")
  if (! any(class(codes_df) %in% c("data.frame","tbl_df")))  stop("Codelist needs to be provided as a data frame")
  codes_df = as.data.frame(codes_df)  # in case a tibble
  
  if (! vocab_col %in% colnames(codes_df))  stop("Codelist data frame needs to include vocabulary column. Specify with `vocab_col`")
  if (! codes_col %in% colnames(codes_df))  stop("Codelist data frame needs to include vocabulary column. Specify with `vocab_col`")
  
  if (! any(c("ICD10","Read2","CTV3") %in% codes_df[,vocab_col]))  stop("Vocabularies need to include ICD10, Read2 or CTV3")
  
  
	# Check code lists - only first 5 digits are used by UK Biobank
	if (verbose) cat("Check code lists - only first 5 digits are used by UK Biobank\n")
		get_icd10 = FALSE
		get_read2 = FALSE
		get_ctv3  = FALSE
		ICD10s    = ""
		Read2s    = ""
		CTV3s     = ""
	if (any(codes_df[,vocab_col] == "ICD10"))  {
		get_icd10 = TRUE
		ICD10s    = codes_df |>
			dplyr::filter(!!rlang::sym(vocab_col) == "ICD10") |>
			dplyr::select(!!rlang::sym(codes_col)) |>
			dplyr::pull() |>
			unique() |>
			stringr::str_remove(stringr::fixed(".")) |> 
			stringr::str_sub(1, 5) |> 
			stringr::str_c("%")
		cat("Will search `hesin` and `death_cause` for", length(ICD10s), "unique ICD10 codes\n")
	}
	if (any(codes_df[,vocab_col] == "Read2"))  {
		get_read2 = TRUE
		Read2s    = codes_df[ codes_df[,vocab_col] == "Read2" , codes_col ]
		Read2s    = stringr::str_sub(Read2s, 1, 5) |> unique()
		cat("Will search `gp_clinical` for", length(Read2s), "unique Read2 codes\n")
	}
	if (any(codes_df[,vocab_col] == "CTV3"))  {
		get_ctv3 = TRUE
		CTV3s    = codes_df[ codes_df[,vocab_col] == "CTV3" , codes_col ]
		CTV3s    = stringr::str_sub(CTV3s, 1, 5) |> unique()
		cat("Will search `gp_clinical` for", length(CTV3s), "unique CTV3 codes\n")
	}

	#
	#
	#
	
	# Connect to Spark
	if (verbose) cat("Connect to Spark [", spark_master, "]\n")
	sc <- sparklyr::spark_connect(master = spark_master)
	
	# Get app database ID
	if (verbose) cat("Get app database ID\n")
	app_id <- system("dx describe *dataset | grep  app | awk -F ' ' '{print $2}' | tail -n 1", intern = TRUE) |>
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
		if (verbose) cat(" - `death_cause`\n")
		death_cause_query = stringr::str_c(
			"SELECT * FROM death_cause WHERE cause_icd10 LIKE '",
			stringr::str_flatten(ICD10s, collapse = "' OR cause_icd10 LIKE '"),
			"'"
		)
		death_cause_tbl = dplyr::tbl(sc, dbplyr::sql(death_cause_query))

		# same for hesin_diag, but will also join with the episode date info etc.
		if (verbose) cat(" - `hesin_diag`\n")
		hesin_diag_query = stringr::str_c(
			"SELECT hesin_diag.eid, hesin_diag.diag_ICD10, hesin_diag.dnx_hesin_id, hesin.epistart, hesin.epiend, hesin.admidate, hesin.disdate ",
			"FROM hesin_diag ",
			"INNER JOIN hesin on hesin_diag.dnx_hesin_id=hesin.dnx_hesin_id ",
			"WHERE diag_icd10 LIKE '",
			stringr::str_flatten(ICD10s, collapse = "' OR diag_icd10 LIKE '"),
			"'"
		)
		hesin_diag_tbl = dplyr::tbl(sc, dbplyr::sql(hesin_diag_query))
		
	}
	if (get_read2 | get_ctv3)  {
		if (verbose) cat(" - `gp_clinical`\n")
		gp_clinical_tbl = dplyr::tbl(sc, "gp_clinical")
		gp_clinical_tbl = gp_clinical_tbl |> dplyr::filter(read_2 %in% !!Read2s | read_3 %in% !!CTV3s) 
	}
	
	# Collect data to memory
	if (verbose) cat("Collect data to memory\n")
	if (get_icd10)  {
		if (verbose) cat("- death_cause\n")
		death_cause = sparklyr::collect(death_cause_tbl)

		if (verbose) cat("- hesin\n")
		hesin_diag = sparklyr::collect(hesin_diag_tbl)
	}
	if (get_read2 | get_ctv3)  {
		if (verbose) cat("- gp_clinical\n")
		gp_clinical = sparklyr::collect(gp_clinical_tbl)
	}
	
	#
	#
	#
	
	# Return data as list
	list(death_cause, hesin, gp_clinical)

}














