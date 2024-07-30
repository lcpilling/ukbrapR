# ukbrapR <a href="https://lcpilling.github.io/ukbrapR/"><img src="man/figures/ukbrapR.png" align="right" width="150" /></a>

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.2.0-informational.svg)](https://github.com/lcpilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lcpilling/ukbrapR.svg)](https://github.com/lcpilling/ukbrapR/commits/master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
[![DOI](https://zenodo.org/badge/709765135.svg)](https://zenodo.org/doi/10.5281/zenodo.11517716)
<!-- badges: end -->

ukbrapR (phonetically: 'U-K-B-wrapper') is an R package for working in the UK Biobank Research Analysis Platform (RAP). The aim is to make it quicker, easier, and more reproducible.

> Since version `0.2.0` the package works best in a "normal" cluster using RStudio and raw UK Biobank data from the table-exporter. Prior versions were designed with Spark clusters in mind. These functions are still available but are not updated.

<sub>Wrapped server icon by DALL-E</sub>

## Installation

In the DNAnexus Tools menu launch an RStudio environment on a normal priority instance. Install {ukbrapR} as below:

```r
remotes::install_github("lcpilling/ukbrapR")        # development version
remotes::install_github("lcpilling/ukbrapR@v0.1.7") # specific release (see tags)
```

## Export tables of raw data

This only needs to happen once per project. Running `ukbrapR::export_tables()` will submit the necessary `table-exporter` jobs to save the raw medical records files to the RAP persistent storage for the project. ~10Gb of text files are created. This will cost ~£0.15 per month to store in the RAP standard storage.

Once the files are exported (~15mins) these can then be used by the below functions to extract diagnoses based on codes lists. 

## Get GP, HES, cancer registry, and self-reported illness data

For a given set of diagnostic codes get the participant Electronic Medical Records (EMR) and self-reported illess data. Returns a list containing up to 6 data frames: the subset of the clinical files with matched codes. 

Codes need to be provided as a data frame with two fields: `vocab_id` and `code`. Valid code vocabularies are:
 - `ICD10` (for searching HES diagnoses, cause of death, and cancer registry)
 - `Read2` and `CTV3` (for GP clinical events)
 - `OPCS3` and `OPCS4` (for HES operations)
 - `ukb_cancer` and `ukb_noncancer` (for self-reported illness at UK Biobank assessments - all instances will be searched)

```r
# example diagnostic codes for CKD 
codes_df_ckd <- ukbrapR:::codes_df_ckd
head(codes_df_ckd)
#>   condition vocab_id  code
#> 1       ckd    ICD10 N18.3
#> 2       ckd    ICD10 N18.4
#> 3       ckd    ICD10 N18.5
#> 4       ckd    ICD10 N18.6
#> 5       ckd    ICD10 N18.9
#> 6       ckd    ICD10   N19

# get diagnosis data - returns list of data frames (one per source)
diagnosis_list <- get_diagnoses(codes_df_ckd) 
#> 7 ICD10 codes, 40 Read2 codes, 37 CTV3 codes 
#> ~3 minutes

# N records for each source
nrow(diagnosis_list$gp_clinical)  #  29,083
nrow(diagnosis_list$hesin_diag)   # 206,390
nrow(diagnosis_list$death_cause)  #   1,962
```

## Get date first diagnosed

Identify the date first diagnosed for each participant from any of datasets searched with `get_diagnoses()` (cause of death, HES diagnoses, GP clinical, cancer registry, HES operations, and self-reported illness fields). 

Also included are:
 - a `src` field indicating the source of the date of first diagnosis.
 - a `bin` field indicating the cases [1] and controls [0]. This relies on a small number of baseline fields also exported. The `df` field for the controls is the date of censoring (currently 30 October 2022).
 - a `bin_prev` field indicating whether the case was before the UK Biobank baseline assessment

```r
# for each participant, get Date First diagnosed with the condition
diagnosis_df <- get_df(diagnosis_list)
#> ~2 seconds

# skim data 
skimr::skim(diagnosis_df)
#> ── Data Summary ────────────────────────
#>                            Values      
#> Name                       diagnosis_df
#> Number of rows             502269      
#> Number of columns          8           
#> 
#> ── Variable type: character ─────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate min max empty n_unique whitespace
#> 1 src              470334        0.0636   2   5     0        3          0
#> 
#> ── Variable type: Date ──────────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate min        max        median     n_unique
#> 1 gp_df            489522       0.0254  1958-01-01 2017-09-06 2009-09-15     3263
#> 2 hes_df           477568       0.0492  1995-08-29 2022-10-31 2018-05-15     5562
#> 3 death_df         500342       0.00384 2008-02-20 2022-12-15 2020-03-03     1429
#> 4 df                    0       1       1958-01-01 2022-12-01 2022-10-30     6367
#> 
#> ── Variable type: numeric ───────────────────────────────────────────────────────
#>   skim_variable n_missing complete_rate         mean          sd
#> 1 bin                   0             1       0.0636       0.244
#> 2 bin_prev              0             1       0.0131       0.114
```

You can add a prefix to all the variable names by specifying the "prefix" option:

```r
diagnosis_df <- get_df(diagnosis_list, prefix="ckd")

# how many cases ascertained?
table(diagnosis_df$ckd_bin)
#>     0      1 
#>470334  31935 

# source of earliest diagnosis date
table(diagnosis_df$ckd_src)
#>    death         gp        hes selfrep_i0 selfrep_i1 selfrep_i2 selfrep_i3 
#>      224      12394      19310         85         16         63          3

# date of diagnosis for prevalent cases (i.e., before UK Biobank baseline assessment)
summary(diagnosis_df$ckd_df[ diagnosis_df$ckd_bin_prev == 1 ])
#>         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#> "1958-01-01" "2006-06-21" "2007-01-12" "2006-06-24" "2007-11-19" "2010-06-16" 
```

## Ascertaining multiple conditions at once 

The default `get_df()` behaviour is to use all available codes. However the most time-efficient way to get multiple conditions is to run `get_diagnoses()` once for all codes for the conditions you wish to ascertain, then get the "date first diagnosed" for each condition separately. In the codes data frame you just need a field indicating the condition name, that will become the variable prefixes.

```r
# combine haemochromatosis and CKD codes together
#   each contain there columns: condition, vocab_id, and code
#   where `condition` is either "hh" or "ckd" and will become the variable prefix
codes_df_combined = rbind(ukbrapR:::codes_df_hh, ukbrapR:::codes_df_ckd)

# get diagnosis data - returns list of data frames (one per source)
diagnosis_list <- get_diagnoses(codes_df_combined)

# for each participant, get Date First diagnosed with the condition
diagnosis_df = get_df(diagnosis_list, group_by="condition")

# each condition has full set of output
table(diagnosis_df$hh_bin)
#>      0      1 
#> 500254   2015 

table(diagnosis_df$ckd_bin)
#>     0      1 
#>470334  31935 
```

In the above example we also included a UK Biobank self-reported illness code for haemochromatosis, that was also ascertained (the Date First is run on each condition separately, they do not all need to have the same data sources).


## Pull phenotype data from Spark environment

**Pull phenotypes from Apache Spark on DNAnexus to an R data frame.** Recommend launching a Spark cluster with at least `mem1_hdd1_v2_x16` and **2 nodes** otherwise this can fail with error "...ensure that workers...have sufficient resources"

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

### Previous Spark functionality

If you need to see the previous release documentation follow the tags to the version required: https://github.com/lcpilling/ukbrapR/tree/v0.1.7


## Questions and comments

I am very open to suggestions and comments, either as [issues](https://github.com/lcpilling/ukbrapR/issues) or [pull requests](https://github.com/lcpilling/ukbrapR/pulls), or via e-mail L.Pilling@exeter.ac.uk
