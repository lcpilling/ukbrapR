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
#' # get phenotype data
#' df <- get_rap_phenos(c("eid","p31","p21003_i0"))
#' 
#' # save to file on the RAP worker node
#' write_tsv(df, "ukb14631.data_output.20231026.txt.gz")
#' 
#' # upload data to RAP storage
#' ukbrapR::upload_to_rap(file="ukb14631.data_output.20231026.txt.gz", dir="")
#'
#' @export
#'
get_emr_diagnoses <- function(codes,
                              spark_master = "spark://master:41000",
                              verbose=FALSE)  {
	
	# Connect to Spark
	if (verbose) cat("Connect to Spark [", spark_master, "]\n")
	sc <- sparklyr::spark_connect(master = spark_master)
	
	# Get app database ID
	if (verbose) cat("Get app database ID\n")
	app_id <- system("dx describe *dataset | grep  app | awk -F ' ' '{print $2}' | tail -n 1", intern = TRUE) |>
		stringr::str_replace(".dataset", "")
	
	# Connect Spark to this database
	if (verbose) cat("Connect Spark to this database\n")
	sparklyr::tbl_change_db(sc, app_id) 
	
	# List tables
	#DBI::dbListTables(sc)
	
	#death <- dplyr::tbl(sc, "death")
	death_head <- DBI::dbGetQuery(sc, "SELECT * FROM death LIMIT 10")
	#death_head <- sparklyr::sdf_sql(sc, "SELECT * FROM death LIMIT 10")
	
	
	

}














