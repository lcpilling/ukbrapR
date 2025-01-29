#' Get UK Biobank participant Electronic Medical Records (EMR) data in a RAP Spark environment
#'
#' @description 
#' 
#' This function is completely removed. Better to use `get_diagnoses()`. Use a historic release of this package if you really need it https://github.com/lcpilling/ukbrapR/releases
#'
#' @return Returns nothing
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
#' @export
#'
get_emr_spark <- function(
	codes_df=NULL,
	spark_master = "spark://master:41000",
	verbose=FALSE
)  {
	
	lifecycle::deprecate_warn("0.3.0", "get_emr_spark()", "get_diagnoses()", details="This function is now completely removed. Use a historic release of this package if you really need it https://github.com/lcpilling/ukbrapR/releases")
	
}



