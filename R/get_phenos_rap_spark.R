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
get_rap_phenos <- function(names,
                           record=NULL,
                           verbose=FALSE)  {

	if (verbose) cat("Check input & options\n")
	if (! is.character(names) ) stop("`names` needs to be a character vector of UK Biobank RAP phenotype names, e.g., c(\"eid\",\"p31\",\"p21003_i0\")")

	if (verbose) cat("Check `eid` is included\n")
	if (! "eid" %in% names)  {
		cat("Adding `eid` to the requested variable names\n")
		names <- c("eid",names)
	}
	
	cat("Getting data for ", length(names), " phenotypes\n")

	if (verbose) cat("Import dxdata package and initialize Spark (dxdata) engine\n")
	cat("*** If asked about creating a python environment, choose 'no' ****\n")
	dxdata <- reticulate::import("dxdata")

	if (verbose) cat("Connect to the dataset\n")
	project <- system("dx env | grep project- | awk -F '\t' '{print $2}'", intern = TRUE)
	if (is.null(record))  {
		record <- system("dx describe *dataset | grep  record- | awk -F ' ' '{print $2}' | head -n 1" , intern = TRUE)
	}
	DATASET_ID <- paste0(project, ":", record)
	dataset <- dxdata$load_dataset(id=DATASET_ID)

	if (verbose) cat("Select participant table\n")
	pheno <- dataset$entities_by_name[['participant']]

	if (verbose) cat("Select fields from participant table\n")
	fld = purrr::map(names, function(x) pheno$find_field(name=x))

	if (verbose) cat("Define the Spark engine\n")
	engine <- dxdata$connect(dialect="hive+pyspark")

	if (verbose) cat("Retrieve the fields defined in fld list\n")
	df <- pheno$retrieve_fields(engine=engine, fields=fld, coding_values="replace")

	if (verbose) cat("Write the data to a temporary parquet file\n")
	uid_parquet <- runif(n=1, min=1e6, max=9e6)
	parquet_name <- paste0('tmpdf.',uid_parquet,'.parquet')
	system(paste0('hadoop fs -rm -r -f ', parquet_name), intern = TRUE)
	df$write$parquet(parquet_name)
	if (verbose) cat(paste0("Witten to ", parquet_name, "\n"))

	if (verbose) cat("Copy the temporary parquet file from distributed to the local file system\n")
	if(dir.exists(parquet_name)) unlink(paste0('tmpdf.',uid_parquet,'.parquet'), recursive=TRUE)
	system(paste0('hadoop fs -copyToLocal ', parquet_name), intern = TRUE)

	if (verbose) cat("Read the dataset information R using Apache arrow package\n")
	ds <- arrow::open_dataset(parquet_name)

	if (verbose) cat("Collect the data from the dataset to R memory\n")
	tbl <- ds |> dplyr::collect()
	tbl

}


