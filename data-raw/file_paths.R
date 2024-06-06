indy_paths = data.frame(
	object=c("death","death_cause","hesin","hesin_diag","gp_clinical"),
	path=c(
		"/indy/ukbiobank/data_14631/mortality/ukb14631_death_20230412.txt",
		"/indy/ukbiobank/data_14631/mortality/ukb14631_death_cause_20230412.txt",
		"/indy/ukbiobank/data_14631/hes/2022/_download/hesin.txt",
		"/indy/ukbiobank/data_14631/hes/2022/_download/hesin_diag.txt",
		"/indy/ukbiobank/data_14631/gp/2017/raw/gp_clinical.txt"
	)
)
snow_paths = data.frame(
	object=c("death","death_cause","hesin","hesin_diag","gp_clinical"),
	path=c(
		"H:/Projects/BioBank/14631_ageing-well/Death data/ukb14631_death_20230412.txt",
		"H:/Projects/BioBank/14631_ageing-well/Death data/ukb14631_death_cause_20230412.txt",
		"H:/Projects/BioBank/14631_ageing-well/HES up to 2022 Oct/_download/hesin.txt",
		"H:/Projects/BioBank/14631_ageing-well/HES up to 2022 Oct/_download/hesin_diag.txt",
		"H:/Projects/BioBank/14631_ageing-well/GP up to 2017 June/_download/gp_clinical.txt"
	)
)
usethis::use_data(indy_paths, snow_paths, internal = TRUE, overwrite = TRUE, compress = 'xz')

