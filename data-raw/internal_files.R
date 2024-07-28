
#
# file paths ################################################
#

ukbrapr_paths = data.frame(
	object=c(
		"hesin",
		"hesin_diag",
		"hesin_oper",
		"gp_clinical",
		"gp_scripts",
		"death",
		"death_cause",
		"selfrep_illness",
		"cancer_registry",
		"baseline_dates"),
	path=c(
		"ukbrapr_data/hesin.tsv",
		"ukbrapr_data/hesin_diag.tsv",
		"ukbrapr_data/hesin_oper.tsv",
		"ukbrapr_data/gp_clinical.tsv",
		"ukbrapr_data/gp_scripts.tsv",
		"ukbrapr_data/death.tsv",
		"ukbrapr_data/death_cause.tsv",
		"ukbrapr_data/selfrep_illness.tsv",
		"ukbrapr_data/cancer_registry.tsv",
		"ukbrapr_data/baseline_dates.tsv"
	)
)
indy_paths = data.frame(
	object=c("hesin","hesin_diag","hesin_oper","death","death_cause","gp_clinical","gp_scripts","selfrep_illness","cancer_registry"),
	path=c(
		"/indy/ukbiobank/data_14631/hes/2022/_download/hesin.txt",
		"/indy/ukbiobank/data_14631/hes/2022/_download/hesin_diag.txt",
		"/indy/ukbiobank/data_14631/hes/2022/_download/hesin_oper.txt",
		"/indy/ukbiobank/data_14631/mortality/ukb14631_death_20230412.txt",
		"/indy/ukbiobank/data_14631/mortality/ukb14631_death_cause_20230412.txt",
		"/indy/ukbiobank/data_14631/gp/2017/raw/gp_clinical.txt",
		"/indy/ukbiobank/data_14631/gp/2017/raw/gp_scripts.txt",
		"/indy/ukbiobank/data_14631/derived_phenotypes/diagnoses/ukb14631.selfrep_phenos.txt.gz",
		"/indy/ukbiobank/data_14631/derived_phenotypes/diagnoses/ukb14631.cancer_registry.txt.gz"
	)
)
snow_paths = data.frame(
	object=c("hesin","hesin_diag","hesin_oper","death","death_cause","gp_clinical","gp_scripts","selfrep_illness","cancer_registry"),
	path=c(
		"H:/Projects/BioBank/14631_ageing-well/HES up to 2022 Oct/_download/hesin.txt",
		"H:/Projects/BioBank/14631_ageing-well/HES up to 2022 Oct/_download/hesin_diag.txt",
		"H:/Projects/BioBank/14631_ageing-well/HES up to 2022 Oct/_download/hesin_oper.txt",
		"H:/Projects/BioBank/14631_ageing-well/Death data/ukb14631_death_20230412.txt",
		"H:/Projects/BioBank/14631_ageing-well/Death data/ukb14631_death_cause_20230412.txt",
		"H:/Projects/BioBank/14631_ageing-well/GP up to 2017 June/_download/gp_clinical.txt",
		"H:/Projects/BioBank/14631_ageing-well/GP up to 2017 June/_download/gp_scripts.txt",
		"H:/Projects/BioBank/14631_ageing-well/diagnoses/ukb14631.selfrep_phenos.txt.gz",
		"H:/Projects/BioBank/14631_ageing-well/diagnoses/ukb14631.cancer_registry.txt.gz"
	)
)


#
# codes lists ###################################################
#

library(tidyverse)

# CKD -- from https://github.com/GEMINI-multimorbidity/GEMINI-LTC-code-list-Public
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

# Haemochromatosis (include self-reported)
selfrep <- c("1507")
ICD10   <- c("E83.1")
Read2   <- c("126A.","4L41.","677C0","C350.","C3500")
CTV3    <- c("C3500","X40QQ","XaIyI","XaIyx","XaXHI","XE13K","X307o","X307p")
codes_df_hh <- data.frame(
  vocab_id = c(
    rep("ukb_noncancer", length(selfrep)),
    rep("ICD10", length(ICD10)),
    rep("Read2", length(Read2)),
    rep("CTV3", length(CTV3))
  ),
  code = c(selfrep, ICD10, Read2, CTV3)
)
codes_df_hh$condition = "hh"
codes_df_hh = codes_df_hh[,c("condition","vocab_id","code")]

# synthetic - all types of code!
cancers = c("C22")
OPCS3 = c("502","509")
OPCS4 = c("J01")
codes_df_test = rbind(
	codes_df_hh,
	data.frame(
		condition="test",
		vocab_id = c(
			rep("ICD10", length(cancers)),
			rep("OPCS3", length(OPCS3)),
			rep("OPCS4", length(OPCS4))
		),
		code = c(cancers, OPCS3, OPCS4)
	)
)
codes_df_test$condition = "test"


#
# save as internal ###########################################
#
usethis::use_data(
	ukbrapr_paths, indy_paths, snow_paths,
	codes_df_ckd, codes_df_hh, codes_df_test, 
	internal = TRUE, overwrite = TRUE, compress = 'xz')





