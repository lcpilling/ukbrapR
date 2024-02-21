# ukbrapR
{ukbrapR} (phonetically: 'U-K-B-wrapper') is an R package for use in the UK Biobank Research Analysis Platform (RAP).

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.1.0-informational.svg)](https://github.com/lukepilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lukepilling/ukbrapR.svg)](https://github.com/lukepilling/ukbrapR/commits/master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

**Only works on [DNAnexus](https://ukbiobank.dnanexus.com) in a UK Biobank project in JupyterLab on a Spark Cluster.**

In tools, launch a JupyterLab environment on a Spark Cluster. In R, install {ukbrapR}. This will install the necessary dependencies for interacting with Python, Apache Spark, and the Arrow C++ library.

```r
# Install the development version
remotes::install_github("lukepilling/ukbrapR")

# To install the latest release, use:
remotes::install_github("lukepilling/lukesRlib@*release")

# To install a specific version (see tags), use:
remotes::install_github("lukepilling/lukesRlib@v0.2.0")
```

I highly recommend saving a "snapshot" once all the packages are installed, and loading this when launching JupyterLab.


## Get phenotype data

Pull phenotypes from Spark to an R data frame. Recommend launching a Spark cluster with at least `mem1_hdd1_v2_x16` and **2 nodes** otherwise this can fail with error "...ensure that workers...have sufficient resources"

The underlying code is mostly from the [UK Biobank GitHub](https://github.com/UK-Biobank/UKB-RAP-Notebooks/blob/main/NBs_Prelim/105_export_participant_data_to_r.ipynb). 

```r
# get phenotype data (participant ID, sex, baseline age, and baseline assessment date)
ukb <- get_rap_phenos(c("eid", "p31", "p21003_i0", "p53_i0"))
#> 48.02 sec elapsed

# summary of data
table(ukb$p31)
#> Female   Male 
#> 273297 229067
summary(ukb$p21003_i0)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  37.00   50.00   58.00   56.53   63.00   73.00 
```

## Get medical records diagnoses

For a given set of diagnostic codes (ICD10, Read2, CTV3) get the participant Electronic Medical Records (EMR) data. Returns a list containing 3 data frames in "long" format (>1 row per participant): the subset of `gp_clinical`, `hesin_diag` and `death_cause` with matched codes.

```r
# example diagnostic codes for CKD from GEMINI multimorbidity project are included
head(codes_df_ckd)
#>   vocab_id  code
#> 1    ICD10 N18.3
#> 2    ICD10 N18.4
#> 3    ICD10 N18.5
#> 4    ICD10 N18.6
#> 5    ICD10 N18.9
#> 6    ICD10   N19

# get diagnosis data - returns list of data frames (one per source)
diagnosis_list <- get_emr(codes_df_ckd)
#> 7 ICD10 codes, 40 Read2 codes, 37 CTV3 codes 
#> 298.18 sec elapsed

# N records for each source
nrow(diagnosis_list$gp_clinical)  #  29,088
nrow(diagnosis_list$hesin_diag)   # 206,394
nrow(diagnosis_list$death_cause)  #   1,962
```

## Get date first diagnosis

Identify the date first diagnosed for each participant from any of the "long" datasets ascertained from `get_emr()` (cause of death, HES, and GP).

```r
# for each participant, get Date First diagnosed with the condition
diagnosis_df <- get_df(diagnosis_list)
#> 0.98 sec elapsed

# skim data 
skimr::skim(diagnosis_df)
#> ── Data Summary ────────────────────────
#>                            Values      
#> Name                       diagnosis_df
#> Number of rows             31945       
#> Number of columns          5           
#> 
#> ── Variable type: Date ───────────────────────────────────────────────────────────
#>   skim_variable  n_missing complete_rate min        max        median     n_unique
#> 1 gp_df              19195        0.399  1958-01-01 2017-09-06 2009-09-15     3264
#> 2 hes_df              7242        0.773  1995-08-29 2022-10-31 2018-05-15     5562
#> 3 death_df           30018        0.0603 2008-02-20 2022-12-15 2020-03-03     1429
#> 4 df                     6        1.00   1958-01-01 2022-12-01 2015-02-17     6367
```

## Identify self-reported illness / cancer

UK Biobank cancer (https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20001) and non-cancer (https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=20002) illness codes can be included in the codes list:

```r
# Example, for haemochromatosis:
print(codes_df_hh)
#>         vocab_id   code
#> 1  ukb_noncancer   1507
#> 2           ICD9 275.03
#> 3          ICD10  E83.1
#> 4          Read2  126A.
#> 5          Read2  4L41.
#> 6          Read2  677C0
#> 7          Read2  C350.
#> 8          Read2  C3500
#> 9           CTV3  C3500
#> 10          CTV3  X40QQ
#> 11          CTV3  XaIyI
#> 12          CTV3  XaIyx
#> 13          CTV3  XaXHI
#> 14          CTV3  XE13K
#> 15          CTV3  X307o
#> 16          CTV3  X307p
```

The below function will by default pull the appropriate self-reported fields from the RAP Spark system to determine whether a participant has reported any of the provided codes, and identify the self-reported date of diagnosis:

```r
selfrep_df = get_selfrep_illness(codes_df)

table(selfrep_df$selfrep)
#>     0      1 
#> 502099    170 
summary(selfrep_df$selfrep_df)
#>         Min.      1st Qu.       Median         Mean      3rd Qu.         Max.         NA's 
#> "1941-11-25" "2003-07-02" "2007-07-02" "2006-06-26" "2011-07-02" "2022-07-02"     "502100" 
table(selfrep_df$selfrep_i)
#>  0  1  2  3 
#> 86 16 65  3 
```

This can add quite a bit of time. If you prefer to get the fields yourself and provide them to the function each time, you can:

```r
selfrep_df = get_selfrep_illness(codes_df, ukb_dat = ukb_dat)
```

You can add this data to the diagnosis list object to get an overall date first (self-reported plus medical records):

```r
diagnosis_list              <- get_emr(codes_df_hh)
selfrep_df                  <- get_selfrep_illness(codes_df_hh)
diagnosis_list[["selfrep"]] <- selfrep_df
diagnosis_df                <- get_df(diagnosis_list)
```


## Example analysis

Hypothesis: baseline age and sex are associated with risk of incident CKD diagnosis in the medical record follow-up 

```r 
# merge phenotype data with ascertained diagnoses
ukb <- dplyr::left_join(ukb, diagnosis_df, by="eid")

# create binary "ever diagnosed" variable and 
# time-to-event from baseline (incident only) (or time to censoring, if no diagnosis)
ukb <- ukb |> dplyr::mutate(
	CKD_bin  = dplyr::if_else(!is.na(df), 1, 0),
	CKD_time = dplyr::case_when(
		!is.na(df) & df > p53_i0 ~ as.numeric(df - p53_i0)/365.25,
		is.na(df) ~ as.numeric(lubridate::dmy("31-10-2022") - p53_i0)/365.25,
		TRUE ~ NA
	)
)

table(ukb$CKD_bin)
#>      0      1 
#> 470425  31939 
summary(ukb$CKD_time[ ukb$CKD_bin==1 ])
#>   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#>  0.003   4.427   7.863   7.694  11.244  16.512    6607 

# fit time to event CoxPH model 
library(survival)
fit_CKD_coxph <- coxph(Surv(CKD_time, CKD_bin) ~ p21003_i0 + p31, data = ukb)

# get tidy model output with 95% CIs and extreme p-values
lukesRlib::tidy_ci(fit_CKD_coxph)
#> CoxPH model (estimate=Hazard Ratio) :: N=495757, Nevents=25332 :: C-statistic=0.71
#> # A tibble: 2 × 8
#>   term      estimate std.error statistic  p.value conf.low conf.high p.extreme 
#>   <chr>        <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl> <chr>     
#> 1 p21003_i0     1.12   0.00104     106.  0            1.11      1.12 2.22e-2462
#> 2 p31Male       1.16   0.0126       11.8 3.27e-32     1.13      1.19 NA
```

## Upload file to the RAP

A wrapper to quickly upload a file from the worker node to the RAP space.

```r
# save phenotypes to a file and upload to current working directory
readr::write_tsv(ukb, "ukbrap.phenos.20231114.txt.gz")
upload_to_rap("ukbrap.phenos.20231114.txt.gz")

# save "long" diagnosis data plus the derived "wide" date first and upload to directory "extracts"
readr::write_tsv(diagnosis_list$death_cause, "ukbrap.CKD.death_cause.20231114.txt.gz")
readr::write_tsv(diagnosis_list$hesin_diag,  "ukbrap.CKD.hesin_diag.20231114.txt.gz")
readr::write_tsv(diagnosis_list$gp_clinical, "ukbrap.CKD.gp_clinical.20231114.txt.gz")
readr::write_tsv(diagnosis_df, "ukbrap.CKD.date_first.20231114.txt.gz")

upload_to_rap("ukbrap.*.20231114.txt.gz", dir="extracts/")
```

## Benchmarking

On a DNAnexus Spark cluster: `mem1_hdd1_v2_x16` with **2 nodes** on "Normal" priority

Thing | Time taken | Note
----- | ---------- | ----
Launch JupyterLab Spark cluster | 8 minutes | 
Build R package plus dependencies | 5 minutes | Can be skipped if using a saved Snapshot
Get phenotype data | 1 minute | 
Ascertain medical records diagnoses | 5 minutes | Includes converting to wide "date first"
Simple analysis | <1 minute | 

Approx. time: 20 mins

Approx. cost: £0.20

*Note: time can vary depending on number of phenotypes/diagnostic codes requested, and how busy the platform is.*
