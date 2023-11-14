# ukbrapR
{ukbrapR} (phonetically: 'U-K-B-rapper') is an R package for use in the UK Biobank Research Analysis Platform (RAP).

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.0.2-informational.svg)](https://github.com/lukepilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lukepilling/ukbrapR.svg)](https://github.com/lukepilling/ukbrapR/commits/master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

**Only works on [DNAnexus](https://ukbiobank.dnanexus.com) in a UK Biobank project in JupyterLab on a Spark Cluster.**

In tools, create a JupyterLab environment on a Spark Cluster. Start an R session and install this package. This will install the necessary dependencies for interacting with the Python environment, Apache Spark, and the Arrow C++ library.

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("lukepilling/ukbrapR")
  # Takes a few minutes to build the dependencies. Suggest saving as a snapshot.
```

## Get phenotype data

Extracts phenotype variables from the Spark database to an R data frame. Recommend launching a Spark cluster with at least `mem1_hdd1_v2_x16` and **2 nodes** otherwise this can fail with error "...ensure that workers...have sufficient resources"

The underlying code is mostly from the [UK Biobank GitHub](https://github.com/UK-Biobank/UKB-RAP-Notebooks/blob/main/NBs_Prelim/105_export_participant_data_to_r.ipynb). 

```r
# get phenotype data (participant ID, sex, and baseline age)
df <- get_rap_phenos(c("eid", "p31", "p21003_i0"))
#> 48.02 sec elapsed

# summary of data
table(df$p31)
#> Female   Male 
#> 273297 229067
summary(df$p21003_i0)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  37.00   50.00   58.00   56.53   63.00   73.00 

# save to file on the RAP worker node
readr::write_tsv(df, "ukbrap.phenos.20231114.txt.gz")
```

## Get medical records diagnoses

For a given set of diagnostic codes (ICD10, Read2, CTV3) get the participant Electronic Medical Records (EMR) data. Returns a list containing 3 data frames in "long" format (>1 row per participant): the subset of `death_cause`, `hesin_diag` and `gp_clinical` with matched codes.

```r
# example diagnostic codes for CKD from GEMINI multimorbidity project
codes_df <- readr::read_tsv("https://raw.githubusercontent.com/GEMINI-multimorbidity/diagnostic_codes/main/codelists/CKD.txt")

# get diagnosis data - returns list of data frames (one per source)
diagnoses_list <- get_emr_diagnoses(codes_df)
#> 7 ICD10 codes, 40 Read2 codes, 37 CTV3 codes 
#> 298.18 sec elapsed

# save to files on the RAP worker node
readr::write_tsv(diagnoses_list$death_cause, "ukbrap.CKD.death_cause.20231114.txt.gz")
readr::write_tsv(diagnoses_list$hesin_diag,  "ukbrap.CKD.hesin_diag.20231114.txt.gz")
readr::write_tsv(diagnoses_list$gp_clinical, "ukbrap.CKD.gp_clinical.20231114.txt.gz")
```

## Get date first diagnosis

Identify the date first diagnosed for each participant from any of the "long" datasets ascertained from `get_emr_diagnoses()` (cause of death, HES, and GP).

```r
# for each participant, get Date First diagnosed with the condition
diagnosis_df <- get_emr_df(diagnosis_list)
#> 0.98 sec elapsed

# save to files on the RAP worker node
readr::write_tsv(diagnosis_df, "ukbrap.CKD.date_first.20231114.txt.gz")
```

## Upload file to the RAP

A wrapper to quickly upload a file from the worker node to the RAP space.

```r
# uploads to current working directory
upload_to_rap("ukbrap.phenos.20231114.txt.gz")

# uploads to directory "extracts"
upload_to_rap("ukbrap.*.20231114.txt.gz", dir="extracts/")
```


