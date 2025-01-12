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

