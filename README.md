# ukbrapR
R functions to use in the UK Biobank Research Analysis Platform (RAP).

** **Development version and does not yet work as expected.** **

<!-- badges: start -->
[![](https://img.shields.io/badge/version-0.0.0.9000-informational.svg)](https://github.com/lukepilling/ukbrapR)
[![](https://img.shields.io/github/last-commit/lukepilling/ukbrapR.svg)](https://github.com/lukepilling/ukbrapR/commits/master)
[![](https://img.shields.io/badge/lifecycle-experimental-orange)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

This will only work on DNAnexus (https://ukbiobank.dnanexus.com) in an approved UK Biobank project. In tools, create a JupyterLab environment on the Spark Cluster. Start an R session and install this package, which will also install the necessary dependencies for interacting with the Python environment, Apache Spark, and the Arrow C++ library.

```r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("lukepilling/ukbrapR")
```

# get_rap_phenos()

My main motivation was to create a wrapper function in R to extract phenotype variables from the Spark distributed database functions (like SQL). The primary function is `get_rap_phenos()` which mostly contains code from the UK Biobank DNAnexus team https://github.com/UK-Biobank/UKB-RAP-Notebooks/blob/main/NBs_Prelim/105_export_participant_data_to_r.ipynb. 

```r
library(ukbrapR)

# get phenotype data (participant ID, sex, and baseline age)
df <- get_rap_phenos(c("eid","p31","p21003_i0"))

# save to file on the RAP worker node
write_tsv(df, "ukb14631.data_output.20231026.txt.gz")
```

# upload_to_rap()

The only other function currently, it a wrapper to quickly upload a file from the worker node to the RAP space.

```r
# uploads to current working directory
upload_to_rap("ukb14631.data_output.20231026.txt.gz")

# uploads to directory "extracts"
upload_to_rap("ukb14631.data_output.20231026.txt.gz", dir="extracts/")
```



