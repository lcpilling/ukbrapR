# ukbrapR <a href="https://lcpilling.github.io/ukbrapR/"><img src="man/figures/ukbrapR.png" align="right" width="150" /></a>

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.2.7.9000-informational.svg)](https://github.com/lcpilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lcpilling/ukbrapR.svg)](https://github.com/lcpilling/ukbrapR/commits/main)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
[![DOI](https://zenodo.org/badge/709765135.svg)](https://zenodo.org/doi/10.5281/zenodo.11517716)
<!-- badges: end -->

ukbrapR (phonetically: 'U-K-B-wrapper') is an R package for working in the UK Biobank Research Analysis Platform (RAP). The aim is to make it quicker, easier, and more reproducible.

> Since `v0.2.0` ukbrapR works best on a "normal" cluster using RStudio and raw data from the table-exporter. Old Spark functions are still available but are not updated.

<sub>Wrapped server icon by DALL-E</sub>

## Installation

In the DNAnexus Tools menu launch an RStudio environment on a normal priority instance.

```r
# install latest release (recommended)
remotes::install_github("lcpilling/ukbrapR@*release")

# development version
# remotes::install_github("lcpilling/ukbrapR")
```

## Ascertain diagnoses

Diagnosis of conditions in UK Biobank participants come from multiple data sources. {ukbrapR} makes it fast and easy to ascertain diagnoses from multiple UK Biobank data sources in the DNAnexus Research Analysis Platform (RAP). Follow the below steps. See the website article for more details.


### 1. Export tables of raw data

This only needs to happen once per project. Run `export_tables()` to submit the `table-exporter` jobs to save the required files to the RAP persistent storage. ~10Gb of text files are created, costing ~£0.15 per month to store.

### 2. Get diagnoses from all data sources

For a given set of diagnostic codes get the participant Electronic Medical Records (EMR) and self-reported illess data. Returns a list containing up to 6 data frames: the subset of the clinical files with matched codes. 

Codes need to be provided as a data frame with two fields: `vocab_id` and `code`. Valid code vocabularies are:

 - `ICD10` (for searching HES diagnoses, cause of death, and cancer registry)
 - `ICD9` (for searching older HES diagnosis data)
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
#> ...

# get diagnosis data - returns list of data frames (one per source)
diagnosis_list <- get_diagnoses(codes_df_ckd) 
#> 7 ICD10 codes, 40 Read2 codes, 37 CTV3 codes 
#> ~2 minutes

# N records for each source
nrow(diagnosis_list$gp_clinical)  #  29,083
nrow(diagnosis_list$hesin_diag)   # 206,390
nrow(diagnosis_list$death_cause)  #   1,962
```

### 3. Get date first diagnosed

Identify the date first diagnosed for each participant from any of datasets searched with `get_diagnoses()` (cause of death, HES diagnoses, GP clinical, cancer registry, HES operations, and self-reported illness fields). 

Also included are:

 - a `src` field indicating the source of the date of first diagnosis.
 - a `bin` field indicating the cases [1] and controls [0]. This relies on a small number of baseline fields also exported. The `df` field for the controls is the date of censoring (currently 30 October 2022).
 - a `bin_prev` field indicating whether the case was before the UK Biobank baseline assessment

```r
# for each participant, get Date First diagnosed with the condition
#   {optional} add a prefix to the variable names with "prefix"
diagnosis_df <- get_df(diagnosis_list, prefix="ckd")
#> ~2 seconds

# how many cases ascertained?
table(diagnosis_df$ckd_bin)
#>      0      1 
#> 470334  31935 

# source of earliest diagnosis date
table(diagnosis_df$ckd_src)
#>    death         gp        hes selfrep_i0 selfrep_i1 selfrep_i2 selfrep_i3 
#>      224      12394      19310         85         16         63          3

# date of diagnosis for prevalent cases (i.e., before UK Biobank baseline assessment)
summary(diagnosis_df$ckd_df[ diagnosis_df$ckd_bin_prev == 1 ])
#>         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
#> "1958-01-01" "2006-06-21" "2007-01-12" "2006-06-24" "2007-11-19" "2010-06-16" 
```

### Ascertaining multiple conditions at once 

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
#>      0      1 
#> 470334  31935 
```

In the above example we also included a UK Biobank self-reported illness code for haemochromatosis, that was also ascertained (the Date First is run on each condition separately, they do not all need to have the same data sources).

## Other functions

* Label UK Biobank data fields with `label_ukb_fields()`
* Upload/download files between worker and RAP with `upload_to_rap()` and `download_from_rap()`
* Pull phenotypes from Spark instance with `get_rap_phenos()`

## Questions and comments

Please report any bugs or [issues](https://github.com/lcpilling/ukbrapR/issues), and feel free to suggest changes as [pull requests](https://github.com/lcpilling/ukbrapR/pulls). Alternatively, feel free to contact me via e-mail L.Pilling@exeter.ac.uk
