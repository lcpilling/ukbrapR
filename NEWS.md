# ukbrapR v0.3.8 (2nd Dec 2025)

### Changes
 - Implement suggestion by @nsandau (issue #34) to use `--brief --yes` flags when exporting tables with `export_tables()` (if running from terminal user is prompted for each job).


# ukbrapR v0.3.7 (20th June 2025)

### Changes
 - `make_XXX_bed()` functions now give a more accurate ETA. Previously only considered the number of CHRs to search. Now considers the number of variants also.

### Bug fixes
 - Fix issue #31 where `fields_to_phenos()` incorrectly arrayed some "multiple choice" questions


# ukbrapR v0.3.6 (29th May 2025)

### Change
 - `make_dragen_bed()` returns a more informative error if DRAGEN BGENs not found (suggests user is in a Project not updated since April 2025).
 - `make_XXX_bed()` functions now give a more accurate ETA. Previously only considered the number of CHRs to search. Now considers the number of variants also.
 - Removed indy/snow paths (i.e., my local servers) because we are now fully RAP working.
 - Removed Windows functionality. The RAP is always UNIX.


# ukbrapR v0.3.5 (16th April 2025)

### New features
 - `create_pgs()` can now use a local BED file of already-extracted variants. Before, it assumed the user wanted to extract the variants prior to creating the PGS.


# ukbrapR v0.3.4 (6th April 2025)

### New features
 - `make_dragen_bed()` now uses the BGEN files released in April 2025 (in prior versions used the pVCFs).
   - Means when `extract_variants()` or `create_pgs()` are called with option `source="dragen"` this is the default. 
   - If your project does not have the new BGEN files available you can use `ukbrapR:::make_dragen_bed_from_pvcfs()` to extract from the pVCFs (slow).

### Changes
 - Updated internal schema to include new/updated fields.
 - Added "filename" option to `fields_to_phenos()` so that phenotype names can be written straight to a file ready for table-exporter.


# ukbrapR v0.3.3 (18th March 2025)

### New features
 - New function `fields_to_phenos()` takes a vector of field IDs, check they are valid, and returns all possible UK Biobank RAP phenotypes from the schema


# ukbrapR v0.3.2 (19th February 2025)

### Changes
 - Remove bundled plink, plink2 and bgenix files. Instead, download only if needed.
 - Add more consistent progress updates for `make_dragen_bed()` and `make_imputed_bed()`
 - Add "progress" options to `extract_variants()` and `create_pgs()` (default is FALSE). Default is TRUE if you directly call `make_dragen_bed()` or `make_imputed_bed()`

### Bug fixes
 - Fix `make_dragen_bed()` so it doesn't crash if the pVCF subset is empty (i.e., a searched-for chr:pos was missing)


# ukbrapR v0.3.1 (10th February 2025)

### Bug fixes
 - Fix `make_dragen_bed()` position awk search, plink call
 - Fix `create_pgs()` when using WGS - needed to use chr:pos:a1:a2 not rsid
 - Fix `make_imputed_bed()` so it doesn't crash if the BGEN subset is empty (i.e., a searched-for rsid was missing)


# ukbrapR v0.3.0 (29th January 2025)

### New features
Suite of functions to extract and load genetic variants. Main ones of interest will be:
 1. `extract_variants()` takes a list of variant rsIDs as input and extracts the imputed genotypes, loading to memory. This is really a wrapper around two other new functions: `make_imputed_bed()` and `load_bed()`. Also available in `make_dragen_bed()` to extract from whole genome sequence VCF files but this is pretty slow so usually user wants imputed variants.
 2. `create_pgs()` creates a polygenic score (weighted allele score) using user-provided variants and weights. Loaded to memory but also saves a nicely formatted .tsv 

### Breaking changes
 - Removing dependencies: reticulate, arrow, sparklyr. These take a few previous seconds to install every time and are rarely needed. Instead will be installed if user tries to use `get_rap_phenos()`
 - `get_emr_spark()` removed entirely. Much better to use `get_diagnoses()` which has had a *lot* of updates to functionality ad bud fixes.



# ukbrapR v0.2.9 (12th January 2025)

### Bug fixes
 - Fixes for issue #19 (thanks to @nsandau for the help):
   1. Where OPCS searches were not always performed correctly if only OPCS3/4 codes were provided.
   2. When using "group_by" in `get_df()` some diagnoses were incorrectly carried over between groups when different vocabs were provided for each group (condition). 

### Updates
 - Additional checking of `get_diagnoses()` input to abort if "blank" codes are provided to the grep.
 - When getting date first from self-reported illness data exclude "year" if < 1936 (earliest birth year for any participant)


# ukbrapR v0.2.8 (05 October 2024)

### Bug fixes
 - Baseline dates TSV is now correctly located even if user changes working directory 
 - HES operations dates were sometimes parsed as character - this is now fixed to parse as dates

### Updates
 - Warnings relating to parsing issues during grepping that are safe to ignore are now suppressed
 - Updates to documentation / examples / pkgdown site
 - New website articles to `ascertain_diagnoses`, `label_fields` and for `spark_functions`


# ukbrapR v0.2.7 (30 September 2024)

### Updates
 - New function `label_ukb_field()` allows user to add titles and labels to UK Biobank fields provided as integers but are categorical. 
 - New function `label_ukb_fields()` is a wrapper for the above. User just provides a data frame containing UK Biobank fields, and they all get formatted with titles (and labels if categorical).
 - Data from the UK Biobank schema (https://biobank.ctsu.ox.ac.uk/crystal/schema.cgi) are stored internally in `ukbrapR:::ukb_schema`
 - {haven} dependency added for labelling
 - Exported `baseline_dates.tsv` now also includes the assessment centres for completeness (but keeps the same filename to avoid any issues for current projects relying on already-exported files)


# ukbrapR v0.2.6 (16 September 2024)

### Bug fix
 - Fix for issue #10. Grep issues if user provided only Read2 or CTV3 codes, if Read2 or CTV3 were <5 characters, or if Read2/CTV3 codes contained a hyphen. Thanks to @Simon-Leyss for highlighting.
 - Fix for issue #11. When getting self-reported illness codes there was a problem joining the tables if user only provided cancer codes. Thanks to @LauricF for highlighting.
 - Fix for when both types self-reported illness codes were provided. (Incorrect subsetting to just those codes provided after pivoting the long object.)


# ukbrapR v0.2.5 (07 September 2024)

### Bug fix
 - When getting the date first cancer registry diagnosis, some rows were duplicated. This is now fixed so only one row per participant (the date first for any matched cancer ICD10) is returned.
 

# ukbrapR v0.2.4 (05 September 2024)

### Changes
 - Updated internal paths for my servers `indy` and `snow` (for ongoing projects whilst we can still use local files...)
 - Updated how `get_diagnoses()` and `get_df()` handle a user-provided `file_paths` object
 

# ukbrapR v0.2.3 (22 August 2024)

### Bug fixes 
 - Fix for issue #8. In moving the HES ICD10 code block below the cancer registry code I acctidently put it within the `if (get_canreg)  { }` condition. Thanks to @LauricF for highlighting.
 - Fix bullet points in pkgdown version of docs


# ukbrapR v0.2.2 (21 August 2024)

### Update
 - The HESIN diagnosis search can now also include ICD9 codes in the provided codes data frame. These use fuzzy matching (similar to the ICD10s) so that searching for "280" also returns "2809" etc


# ukbrapR v0.2.1 (10 August 2024)

### Bug fix 
 - Fix for issue #5. The file paths for exported tables were not correctly specified in later calls of `get_diagnoses()` when the working directory is not the home directory. Thanks to @LauricF for highlighting.


# ukbrapR v0.2.0 (30 July 2024)

This is a major update as I move away from using Spark as the default environment, mostly due to the cost implications; it is significantly cheaper (and quicker!) to store and search exported raw text files in the RAP persistant storage than do everything in a Spark environment (plus the added benefit that the RStudio interface is available in "normal" instances). 

The Spark functions are available as before but all updates are to improve functionality in "normal" instances using RStudio, as we move to the new era of RAP-only UK Biobank analysis. 

### Changes
 - Added internal data frame containing default paths for exported files in a RAP project (view with `ukbrapR:::ukbrapr_paths`)
 - Added function `export_tables()` which only needs to be run once when a new project is created. This submits the required table exporter commands to extract each of the tables in `ukbrapR:::ukbrapr_paths`. This can take ~15 minutes to export all the tables. ~10Gb of text files are created. This will cost ~£0.15 per month to store in the RAP standard storage.
 - `get_emr()` is split into two primary underlying functions: `get_emr_spark()` which has not changed, and `get_emr()` which is the "new way" (i.e., `get_emr_local()` is entirely removed)
 - Added functionality for `hesin_oper` (HES OPCS operations) searching for ICD10 codes in `get_emr()`
 - New/updated internal functions `get_cancer_registry()` asceratains cases using ICD10s in the `cancer_registry` data, and works much the same as `get_selfrep_illness()`
 - New function `get_diagnoses()` is a wrapper to get HES diagnosis, operations, cause of death, GP, cancer registry, and self-reported illness data -- i.e., once function to provide all codes to, and return all health-related data
 - `get_df()` takes all output from `get_diagnoses()` i.e., now also identifies date of first in matched `cancer_registry` and `hesin_oper` entries, in addition to `hes_diag`, `gp_clinical`, `death_cause` and `selfrep_illness` as before.
 - When getting "date first" using `get_df()` the baseline data is used to create binary case/control variables (for ever and prevalent), and for controls the censoring date is included in the overall `_df` variable (default is 30-10-2022).

**To make it absolutely clear:** the Spark function `get_emr_spark()` has not been updated but I am no longer focussed on doing things this way. If you want to submit Pull Requests to improve functions please do. The below changes are to substantially improve the experience of using exported tables in the RAP environment only (if you have all the data on a local system already it will work, assuming you format correctly and provide the paths, but the RAP is the future).


# ukbrapR v0.1.7 (28 July 2024)

### Bug fixes
 - Fix Spark database error when >1 dataset file is available. Fixes issue #3


# ukbrapR v0.1.6 (03 July 2024)

### Bug fixes
 - Fix `get_df()` error when ascertaining GP diagnoses if 7-character codes were provided rather than 5

### Changes
 - `get_emr()` now accepts option "file_paths" - if not provided, attempts to get from Spark
 - Improve documentation and examples


# ukbrapR v0.1.5 (01 July 2024)

### Bug fixes
 - Fix `get_df()` error occurring when not all sources are desired

### Changes
 - `get_emr_local()` option "local_paths" is now "file_paths"
 - Improve documentation and examples


# ukbrapR v0.1.4 (12 June 2024)

### Bug fixes
 - Fix problem identifying ICD10 column name in RAP HESIN
 - Fix problem getting date first for GP data (excluding missing dates before summarizing)


# ukbrapR v0.1.3 (8 June 2024)

### New feature
- It is quicker/easier to ascertain multiple conditions at once to supply `get_emr()` with all the codes (as before), but now can use `get_df()` with option "group_by" to indicate the condition names in the `codes_df` object provided. See documentation.

### Changes 
- It is no longer possible to provide custom names for the `codes_df` to `get_emr()` -- these now must be `vocab_id` and `code` -- makes things much simpler.
- Remove ICD9 code from `codes_df_hh` example as these are not currently used


# ukbrapR v0.1.2 (6 June 2024)

### New features
- New function `get_emr_local()`. If the user has text files for `hesin_diag` and `gp_clinical` etc. these can be searched (rather than Apache Spark queries). This therefore can work on "normal" DNAnexus nodes, or local servers. Most downstream functions also do not rely on Spark clusters if data extracts are available.

### Changes
- Change URL to reflect my GitHub username change from `lukepilling` to `lcpilling` to be more consistent between different logins, websites, and social media
 -- https://lcpilling.github.io/ukbrapR
 -- https://github.com/lcpilling/ukbrapR
- Added dependency {cli} for improved alert/error reporting


# ukbrapR v0.1.1 (6 March 2024)

### New features
 - New argument "prefix" for `get_df()` - user can provide a string to prefix to the output variable names


# ukbrapR v0.1.0 (21 Feb 2024)

### New features
 - `get_selfrep_illness()` - gets illness information from self-report fields. Derives a "date first" from the age/year reported, incorporating all visits for the participant
 - Two example code lists are incuded: `codes_df_ckd` (GEMINI CKD), and `codes_df_hh` (haemochromatosis, with self-report)

### Changes 
 - `get_emr_df()` is re-named `get_df()` to reflect it can now include information from self-reported illness
 - `get_emr_diagnoses()` is re-named `get_emr()` to reflect it actually retrieves any record in `gp_clinical` not just diagnoses (e.g., BMI if appropriate codes provided)

### Bug fixes
 - So many


# ukbrapR v0.0.2 (14 Nov 2023)

### New features
 - `get_emr_diagnoses()` - function to get electronic medical records diagnoses from Spark-based death records, hospital episode statistics, and primary care (GP) databases.
 - `get_emr_df()` - function to get date first diagnosed with any provided code from any above Electronic Medical Record source.

### Bug fixes
 - Extra input checking in `get_rap_phenos()` and output more consistent for direct use with `get_emr_*()` functions
 - Updated URL for example CKD clinical codes


# ukbrapR v0.0.1 (26 Oct 2023)

Initial release containing two functions:
 - `get_rap_phenos()`
 - `upload_to_rap()`

