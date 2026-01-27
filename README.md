# ukbrapR <a href="https://lcpilling.github.io/ukbrapR/"><img src="man/figures/ukbrapR.png" align="right" width="150" /></a>

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.3.10-informational.svg)](https://github.com/lcpilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lcpilling/ukbrapR.svg)](https://github.com/lcpilling/ukbrapR/commits/main)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
[![DOI](https://zenodo.org/badge/709765135.svg)](https://zenodo.org/doi/10.5281/zenodo.11517716)
<!-- badges: end -->

ukbrapR (phonetically: 'U-K-B-wrapper') is an R package for working in the UK Biobank Research Analysis Platform (RAP). The aim is to make it quicker, easier, and more reproducible.

> Since `v0.2.0` ukbrapR works best on a "normal" cluster using RStudio and raw data from the table-exporter. Old Spark functions are still available but are not updated.

<sub>Wrapped server icon by DALL-E</sub>

## Installation

In the DNAnexus Tools menu launch Posit Workbench and start an RStudio environment.

```r
# install current version
remotes::install_github("lcpilling/ukbrapR")
```

## Features

There are three main groups of functions:

* :dna: [Genetics](#genetic-variants): extract genotypes from Bulk data, create polygenic score
* :clipboard: [Diagnoses](#ascertain-diagnoses): ascertain from health records and self-reported illness data, determine date first diagnosed
* :hammer_and_wrench: [Utilities](#utility-functions): Check field names, label data fields, upload/download files from RAP, and pull phenotypes from Spark

## Genetic variants

Bulk imputed genotypes and variant calls from Whole Genome Sequencing are available and can be easily accessed in an RStudio instance. 

### Extract variants

`extract_variants()` by default uses [bgenix](https://enkre.net/cgi-bin/code/bgen) and [plink](https://www.cog-genomics.org/plink/) to subset the [imputed BGEN files](https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=22828) and read it quickly and easily into R.

The only required input is a data frame (or path to file) containing "rsid" and "chr" variables. See function documentation for further details/options (including the available `make_imputed_bed()` and `load_bed()` internal functions).

```r
varlist <- data.frame(rsid=c("rs1800562","rs429358"), chr=c(6,19))

imputed_genotypes <- extract_variants(varlist)
#> ~10 seconds

dim(imputed_genotypes)
#> [1] 487409      3

# If you are missing RSIDs or want to extract using positions (build 37) specify using:
varlist_pos <- data.frame(chr=c(6,19), pos=c(26093141,45411941)) # RSID is ignored/not needed
imputed_genotypes_pos <- extract_variants(varlist_pos)
dim(imputed_genotypes_pos)
#> [1] 487409      3
```

You can use function `get_imputed_variant_info()` to get the UK Biobank imputed genotype variant IDs, alleles, MAF, and INFO from the MFI files. Provide a data.frame containing `chr` and `pos` fields (in build 37).

#### Extract from DRAGEN WGS variant calls

By setting option `source="dragen"` the function will instead subset the [DRAGEN WGS BGEN files](https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=24309). This requires "pos" in the input data frame (build 38). 

```r
varlist_b38 <- data.frame(rsid=c("rs1800562","rs429358"), chr=c(6,19), pos=c(26092913,44908684))

dragen_genotypes <- extract_variants(varlist_b38, source="dragen")
#> ~15 seconds 
```

The highlight of developing this feature was naming the internal function `make_dragen_bed()` :dragon: :bed:

> This assumes your project has access to the WGS BGEN files released April 2025. If not, run `ukbrapR:::make_dragen_bed_from_pvcfs()` to use [tabix](https://www.htslib.org/doc/tabix.html) and [plink](https://www.cog-genomics.org/plink/) to subset the [DRAGEN WGS pVCF files](https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=24310). Much slower.


### Create polygenic score

`create_pgs()` takes a data frame containing a list of variant associations with a trait and creates a weighted allele score using [plink](https://www.cog-genomics.org/plink/1.9/score). By default it uses the [imputed](https://biobank.ctsu.ox.ac.uk/crystal/field.cgi?id=22828) genotypes.

The only required input is a data frame (or path to file) containing rsid, chr, pos, effect_allele, other_allele, beta. For DRAGEN pos is build 38.

```r
# weights from GWAS of liver cirrhosis (Innes 2020 Gastroenterology doi:10.1053/j.gastro.2020.06.014)
varlist_pgs <- readr::read_tsv(system.file("files", "pgs_liver_cirrhosis.txt", package="ukbrapR"))
head(varlist_pgs)
#>   rsID         CHR       POS effect_allele other_allele effect_weight locus_name
#>   <chr>      <dbl>     <dbl> <chr>         <chr>                <dbl> <chr>     
#> 1 rs2642438      1 220796686 A             G                   -0.177 MARC1     
#> 2 rs11925835     3  56831417 T             C                   -0.235 ARHGEF3   
#> 3 rs72613567     4  87310241 TA            T                   -0.166 HSD17B13  
#> 4 rs2954038      8 125495147 C             A                    0.16  TRIB1     
#> 5 rs11065384    12 120985482 T             C                    0.275 HNF1A     
#> 6 rs28929474    14  94378610 T             C                    0.561 SERPINA1  

liver_pgs <- create_pgs(
	in_file=varlist_pgs,                     # can be a data frame or file path
	out_file="liver_cirrhosis.imputed.pgs",  # {optional} prefix for created .bed and .tsv files
	pgs_name="liver_cirrhosis_pgs")          # {optional} variable name
#> → Extracting 9 variants from 8 imputed files
#> ✔ PGS created! See file liver_cirrhosis.imputed.pgs.tsv
#> ~1 minute

summary(liver_pgs$liver_cirrhosis_pgs)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#> 0.00000 0.06006 0.08200 0.08589 0.10722 0.26639

# Default is to search by RSID in the imputed genotype data.

# Either set `use_imp_pos` to TRUE to use the b37 positions, or set `source` to "dragen" to extract from WGS BGENs (using b38 pos)
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

## Utility functions

* Check if field IDs are valid and return phenotype names with `fields_to_phenos()`
* Label UK Biobank data fields with `label_ukb_fields()`
* Upload/download files between worker and RAP with `upload_to_rap()` and `download_from_rap()`
* Pull phenotypes from Spark instance with `get_rap_phenos()`

## Questions and comments

Please report any bugs or [issues](https://github.com/lcpilling/ukbrapR/issues), and feel free to suggest changes as [pull requests](https://github.com/lcpilling/ukbrapR/pulls). Alternatively, feel free to contact me via e-mail L.Pilling@exeter.ac.uk

Thanks to Dr Harry Green (@[hdg204](https://github.com/hdg204)) for the ideas sharing, brainstorming, and bug testing.
