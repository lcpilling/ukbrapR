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

