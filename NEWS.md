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

