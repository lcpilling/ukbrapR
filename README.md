# ukbrapR
{ukbrapR} (phonetically: 'U-K-B-rapper') is a small collection of R functions to use in the UK Biobank Research Analysis Platform (RAP).

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.0.2-informational.svg)](https://github.com/lukepilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lukepilling/ukbrapR.svg)](https://github.com/lukepilling/ukbrapR/commits/master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

*This will only work on DNAnexus (https://ukbiobank.dnanexus.com) in an approved UK Biobank project in a JupyterLab environment on a Spark Cluster.*

In tools, create a JupyterLab environment on a Spark Cluster. Start an R session and install this package. This will  install the necessary dependencies for interacting with the Python environment, Apache Spark, and the Arrow C++ library.

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("lukepilling/ukbrapR")
  # this takes a few minutes to build the dependencies
```

## Get participant phenotype data

Extracts phenotype variables from the Spark database to an R data frame. The underlying code is mostly from the [UK Biobank GitHub](https://github.com/UK-Biobank/UKB-RAP-Notebooks/blob/main/NBs_Prelim/105_export_participant_data_to_r.ipynb). 

```r
# get phenotype data (participant ID, sex, and baseline age)
df <- get_rap_phenos(c("eid","p31","p21003_i0"))
  # takes a few minutes to complete interactions with Apache Spark etc.

# summary of data
table(df$p31)
summary(df$p21003_i0)

# save to file on the RAP worker node
write_tsv(df, "ukb14631.data_output.20231026.txt.gz")
```

## Upload file to the RAP

A wrapper to quickly upload a file from the worker node to the RAP space.

```r
# uploads to current working directory
upload_to_rap("ukb14631.data_output.20231026.txt.gz")

# uploads to directory "extracts"
upload_to_rap("ukb14631.data_output.20231026.txt.gz", dir="extracts/")
```

## Get participant diagnosis data

For a given set of diagnostic codes (ICD10, Read2, CTV3) get the participant medical records information. Returns a list containing 3 data frames: the subset of `death_cause`, `hesin_diag` and `gp_clinical` with matched codes.

```r
# example diagnostic codes for CKD from GEMINI multimorbidity project
codes_df <- readr::read_tsv("https://raw.githubusercontent.com/GEMINI-multimorbidity/diagnostic_codes/main/codelists/CKD.txt")
codes_df

# get diagnosis data - returns list of data frames (one per source)
diagnoses_list <- get_emr_diagnoses(codes_df)
# 7 ICD10 codes, 40 Read2 codes, 37 CTV3 codes 
# 298.18 sec elapsed

# save to files on the RAP worker node
readr::write_tsv(diagnoses_list$death_cause, "ukb14631.CKD.death_cause.20231114.txt.gz")
readr::write_tsv(diagnoses_list$hesin_diag,  "ukb14631.CKD.hesin_diag.20231114.txt.gz")
readr::write_tsv(diagnoses_list$gp_clinical, "ukb14631.CKD.gp_clinical.20231114.txt.gz")
```



