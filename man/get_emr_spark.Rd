% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_emr_spark.R
\name{get_emr_spark}
\alias{get_emr_spark}
\title{Get UK Biobank participant Electronic Medical Records (EMR) data in a RAP Spark environment}
\usage{
get_emr_spark(codes_df, spark_master = "spark://master:41000", verbose = FALSE)
}
\arguments{
\item{codes_df}{A data frame. Contains two columns: `code` and `vocab_id` i.e., a list of diagnostic codes, and an indicator of the vocabulary. Other columns are ignored.}

\item{spark_master}{A string. The `master` argmuent passed to `sparklyr::spark_connect()`.
\code{default='spark://master:41000'}}

\item{verbose}{Logical. Be verbose,
\code{default=FALSE}}
}
\value{
Returns a list of data frames (the participant data for the requested diagnosis codes: `death_cause`, `hesin_diag`, and `gp_clinical`. Also includes the original codes list)
}
\description{
This function is not maintained. Better to use `get_diagnoses()`.

Using a Spark node/cluster on the UK Biobank Research Analysis Platform (DNAnexus), use R to get medical records for specific diagnostic codes list
}
\examples{
# example diagnostic codes for CKD from GEMINI multimorbidity project
head(codes_df_ckd)

# get EMR data - returns list of data frames (one per source)
emr_dat <- get_emr(codes_df_ckd)

# save to files on the RAP worker node -- either as an R object, or separate as text files:
save(emr_dat, "ukbrap.CKD.emr.20231114.RDat")
readr::write_tsv(emr_dat$hesin_diag,  "ukbrap.CKD.hesin_diag.20231114.txt.gz")

# upload data to RAP storage
upload_to_rap(file="ukbrap.CKD.*.20231114.*", dir="")

}
\author{
Luke Pilling
}
