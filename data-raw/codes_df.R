library(tidyverse)

# save 2 code lists:
#  GEMINI CKD
#  Haemochromatosis (include self-reported)

#  CKD -- from https://github.com/GEMINI-multimorbidity/GEMINI-LTC-code-list-Public
ICD10   <- c("N18.3", "N18.4", "N18.5", "N18.6", "N18.9", "N19", "Z94.0")
Read2   <- c("1Z12.", "1Z13.", "1Z14.", "1Z15.", "1Z16.", "1Z1a.", "1Z1B.", "1Z1b.", "1Z1C.", "1Z1c.", "1Z1D.", "1Z1d.", "1Z1E.", "1Z1e.", "1Z1F.", "1Z1f.", "1Z1G.", "1Z1H.", "1Z1J.", "1Z1K.", "1Z1L.", "1Z1T.", "1Z1V.", "1Z1W.", "1Z1X.", "1Z1Y.", "1Z1Z.", "K053.", "K054.", "K055.", "1Z1..", "K0E..", "K05..", "Kyu21", "D2150", "K06..", "6AA..", "66i..", "661M2", "661N2")
CTV3    <- c("X30In", "XaLHI", "XaLHJ", "XaLHK", "XaNbn", "XaNbo", "XacAb", "XacAd", "XaO3t", "XacAe", "XaO3u", "XacAf", "XaO3v", "XacAh", "XaO3w", "XacAi", "XaO3x", "XaO3y", "XaO3z", "XaO40", "XaO41", "XaO42", "XacAM", "XacAN", "XacAO", "XacAV", "XacAW", "XacAX", "XaYb9", "XaYZW", "XaMJD", "XaMGE", "XaCLy", "XE0df", "XE0dg", "X30Iz", "Kyu21")
codes_df_ckd <- data.frame(
  vocab_id = c(
    rep("ICD10", length(ICD10)),
    rep("Read2", length(Read2)),
    rep("CTV3", length(CTV3))
  ),
  code = c(ICD10, Read2, CTV3)
)
codes_df_ckd$condition = "ckd"
codes_df_ckd = codes_df_ckd[,c("condition","vocab_id","code")]
usethis::use_data(codes_df_ckd, overwrite = TRUE, compress = 'xz')

#  Haemochromatosis (include self-reported)
selfrep <- c("1507")
ICD9    <- c("275.03")
ICD10   <- c("E83.1")
Read2   <- c("126A.","4L41.","677C0","C350.","C3500")
CTV3    <- c("C3500","X40QQ","XaIyI","XaIyx","XaXHI","XE13K","X307o","X307p")
codes_df_hh <- data.frame(
  vocab_id = c(
    rep("ICD10", length(ICD10)),
    rep("Read2", length(Read2)),
    rep("CTV3", length(CTV3)),
    rep("ukb_noncancer", length(selfrep))
  ),
  code = c(selfrep, ICD10, Read2, CTV3)
)
codes_df_hh$condition = "hh"
codes_df_hh = codes_df_hh[,c("condition","vocab_id","code")]
usethis::use_data(codes_df_hh, overwrite = TRUE, compress = 'xz')
