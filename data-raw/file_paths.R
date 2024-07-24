ukbrapr_paths = data.frame(
	object=c(
		"hesin",
		"hesin_diag",
		"hesin_oper",
		"death",
		"death_cause",
		"gp_clinical",
		"gp_scripts",
		"selfrep_illness",
		"cancer_registry",
		"baseline_dates"),
	path=c(
		"ukbrapr_data/hesin.tsv",
		"ukbrapr_data/hesin_diag.tsv",
		"ukbrapr_data/hesin_oper.tsv",
		"ukbrapr_data/death.tsv",
		"ukbrapr_data/death_cause.tsv",
		"ukbrapr_data/gp_clinical.tsv",
		"ukbrapr_data/gp_scripts.tsv",
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
usethis::use_data(ukbrapr_paths, indy_paths, snow_paths, internal = TRUE, overwrite = TRUE, compress = 'xz')

