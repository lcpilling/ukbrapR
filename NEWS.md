# ukbrapR v0.1.6 (03 July 2024)

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

