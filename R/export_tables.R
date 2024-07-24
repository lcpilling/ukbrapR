#' Export tables for {ukbrapR} analyses
#'
#' @description In the UK Biobank RAP export tables for HES, GP, death, and cancer registry data, plus self-reported illness fields, using the table-exporter. This is essentially a wrapper function to submit jobs to the table exporter.
#'
#' Suggest executing in an RStudio session. ~10Gb of text files are created. This will cost ~£0.15 per month to store in the RAP standard storage.
#'
#' Once the jobs are completed it is important to move the files to `ukbrapr_data` directory in your project to keep the files tidy
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name export_tables
#'
#' @param submit Logical. Actually submit `dx` commands. Default is FALSE i.e., just check inputs & file paths, then print the commands,
#'        \code{default=FALSE}
#' @param ignore_warnings Logical. If an exported table already exists do not submit the table-exporter command unless this is TRUE,
#'        \code{default=FALSE}
#' @param file_paths A data frame. Columns must be `object` and `path` containing paths to outputted files. If blank, will use the default paths,
#'        \code{default=ukbrapR:::ukbrapr_paths}
#' @param dataset A string. If you wish to specify dataset. If blank, will use the most recently dispensed dataset in the main project directory.
#'        \code{default=app#####_#####.dataset}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' 
#' # To keep files organised this package assumes the following file structure will be created in your RAP space (override by providing a new `file_paths`):
#' ukbrapr_paths = data.frame(
#' 	object=c("death","death_cause","hesin","hesin_diag","hesin_oper","gp_clinical","gp_scripts","selfrep_illness","cancer_registry","baseline_dates"),
#' 	path=c(
#' 		"ukbrapr_data/death.tsv",
#' 		"ukbrapr_data/death_cause.tsv",
#' 		"ukbrapr_data/hesin.tsv",
#' 		"ukbrapr_data/hesin_diag.tsv",
#' 		"ukbrapr_data/hesin_oper.tsv",
#' 		"ukbrapr_data/gp_clinical.tsv",
#' 		"ukbrapr_data/gp_scripts.tsv",
#' 		"ukbrapr_data/selfrep_illness.tsv",
#' 		"ukbrapr_data/cancer_registry.tsv",
#' 		"ukbrapr_data/baseline_dates.tsv"
#' 	)
#' )
#' ukbrapr_paths
#'
#' # test run to see `dx run table-exporter` commands - but will not submit jobs
#' export_tables()
#'
#' # Submit all `dx run table-exporter` commands. ~10Gb of text files are created. This will cost ~£0.15 per month to store in the RAP standard storage.
#' export_tables(submit=TRUE)
#'
#' @export
#'
export_tables <- function(
	submit = FALSE,
	ignore_warnings = FALSE,
	file_paths = ukbrapR:::ukbrapr_paths,
	dataset = NULL,
	verbose = FALSE
)  {
	
	# is this just a test? (Will not actually run any dx commands)
	if (!submit)  {
		cli::cli_alert_info("Test run. Will not submit any {.code dx} system commands")
		ignore_warnings = TRUE
	}
	
	# do any files already exist? Warn if so!
	for (fp in file_paths$path)  {
		if (file.exists(stringr::str_c("/mnt/project/", fp)))  {
			if (ignore_warnings)   cli::cli_alert_danger("File already exists on RAP at {.path {fp}}.")
			if (!ignore_warnings)  cli::cli_abort("File already exists on RAP at {.path {fp}}.")
		}
	}
	
	# if output directory does not exist in the RAP, create it
	fp_dir = file_paths$path[1]
	if (stringr::str_detect(fp_dir, "/"))  {
		fp_dir = stringr::str_split(fp_dir, "/")[[1]][1]
		if (!dir.exists(stringr::str_c("/mnt/project/", fp_dir)))  {
			if (verbose) cli::cli_alert("Creating output directory in your RAP storage space: {.code dx mkdir {fp_dir}}")
			if (submit)  system(stringr::str_c("dx mkdir ", fp_dir))
		}
	}
	
	# get dataset id 
	if (is.null(dataset))  {
		dataset = system("dx describe *dataset | grep  app | awk -F ' ' '{print $2}' | sort | tail -n 1", intern = TRUE)
	}
	if (verbose) cli::cli_alert_info("Using dataset {.file {dataset}}")
	
	# run table exporter commands
	ukbrapR:::export_tables_emr(dataset=dataset, submit=submit, verbose=verbose)
	ukbrapR:::export_tables_selfrep_illness(dataset=dataset, submit=submit, verbose=verbose)
	ukbrapR:::export_tables_cancer_registry(dataset=dataset, submit=submit, verbose=verbose)
	ukbrapR:::export_tables_baseline_dates(dataset=dataset, submit=submit, verbose=verbose)
	
	cli::cli_alert_success("Submitted all table-exporter jobs.")
	cli::cli_alert_info("Can take ~15mins to complete.")
	cli::cli_alert_info("Files will be saved to `ukbrapr_data` directory in your RAP project presistent storage space.")
	cli::cli_alert_warning("~10Gb of text files are created. This will cost ~£0.15 per month to store in the RAP standard storage.")
	
	# was this just a test? 
	if (!submit)  {
		cli::cli_abort("This was a test run. No {.code dx table-exporter} commands were submitted. Re-run with {.code submit = TRUE} to submit.")
	}

}


#' Export tables for HES, GP, and death
#'
#' @description In the UK Biobank RAP export tables for HES, GP, and death using the table-exporter
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name export_tables_emr
#'
export_tables_emr <- function(
	dataset = NULL,
	submit = FALSE,
	verbose = FALSE
)  {

	cli::cli_alert("Export tables: Electronic Medical Records")

	# get dataset id 
	if (is.null(dataset))  {
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	if (verbose) cli::cli_alert("dx run table-exporter for 'death'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='death' -ioutput='death' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

	if (verbose) cli::cli_alert("dx run table-exporter for 'death_cause'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='death_cause' -ioutput='death_cause' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

	if (verbose) cli::cli_alert("dx run table-exporter for 'hesin'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='hesin' -ioutput='hesin' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

	if (verbose) cli::cli_alert("dx run table-exporter for 'hesin_diag'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='hesin_diag' -ioutput='hesin_diag' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

	if (verbose) cli::cli_alert("dx run table-exporter for 'hesin_oper'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='hesin_oper' -ioutput='hesin_oper' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

	if (verbose) cli::cli_alert("dx run table-exporter for 'gp_clinical'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='gp_clinical' -ioutput='gp_clinical' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

	if (verbose) cli::cli_alert("dx run table-exporter for 'gp_scripts'")
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='gp_scripts' -ioutput='gp_scripts' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

}




#' Extract self-reported illess fields 
#'
#' @description In the UK Biobank RAP, submit a table-exporter job to extract the self-reported illness fields:
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name export_tables_selfrep_illness
#'
export_tables_selfrep_illness <- function(
	n_cancer_arrays = 5,
	n_noncancer_arrays = 30,
	n_instances = 3,
	dataset = NULL,
	submit = FALSE,
	verbose = FALSE
)  {
	
	cli::cli_alert("Export table: Self-reported Illness fields")
	
	# RAP stores arrays as separate variables
	if (verbose) cli::cli_alert("Determine field names to request")
	if (verbose) cli::cli_alert(c("n_instances = ", n_instances))
	if (verbose) cli::cli_alert(c("n_cancer_arrays = ", n_cancer_arrays))
	if (verbose) cli::cli_alert(c("n_noncancer_arrays = ", n_noncancer_arrays))

	# Determine variable names needed (depends if cancer or non-cancer)
	#   will use 20001 (cancer code) and 20002 (non-cancer code)
	#   will use the interpolated year (20006 = cancer year, 20008 = non-cancer year)
	
	# get field names 
	names = "eid"
	
	# phenotypes
	#   cancer code = 20001
	#   cancer year = 20006
	for (p in c(20001, 20006))  {
		
		# instances 0:n_instances
		for (i in c(0:n_instances))  {
			
			# cancer arrays
			for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p", p, "_i", i, "_a", a))
			
		}
	}
	
	# phenotypes
	#   non-cancer illness code = 20002
	#   non-cancer illness year = 20008
	for (p in c(20002, 20008))  {
		
		# instances 0:3
		for (i in c(0:n_instances))  {
			
			# non-cancer arrays
			for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p", p, "_i", i, "_a", a))
			
		}
	}

	if (verbose) print(names)
	
	# save and upload names file 
	readr::write_tsv(data.frame(names), "fieldnames_selfrep_illness.txt", col_names=FALSE)
	if (submit)  ukbrapR::upload_to_rap("fieldnames_selfrep_illness.txt", dir="ukbrapr_data")

	# get dataset id 
	if (is.null(dataset))  {
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ifield_names_file_txt='ukbrapr_data/fieldnames_selfrep_illness.txt' -ientity='participant' -ioutput='selfrep_illness' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)


}



#' Extract cancer registry fields 
#'
#' @description In the UK Biobank RAP, submit a table-exporter job to extract the cancer registry fields:
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name export_tables_cancer_registry
#'
export_tables_cancer_registry <- function(
	n_cancer_arrays = 21,
	dataset = NULL,
	submit = FALSE,
	verbose = FALSE
)  {
	
	cli::cli_alert("Export table: Cancer Registry fields")

	# RAP stores arrays as separate variables
	if (verbose) cli::cli_alert("Determine field names to request")
	if (verbose) cli::cli_alert(c("n_cancer_arrays = ", n_cancer_arrays))
	#   date vars = 40005
	#   cancer vars = 40006
	#   age vars = 40008
	#   histology vars = 40011
	#   behaviour vars = 40012
	
	# get field names 
	names = "eid"
	
	# phenotypes
	for (p in c(40005, 40006, 40008, 40011, 40012))  {
		
		# instances 0:n_instances
		for (i in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p", p, "_i", i))
		
	}
	
	if (verbose) print(names)
	
	# save and upload names file 
	readr::write_tsv(data.frame(names), "fieldnames_cancer_registry.txt", col_names=FALSE)
	if (submit)  ukbrapR::upload_to_rap("fieldnames_cancer_registry.txt", dir="ukbrapr_data")

	# get dataset id 
	if (is.null(dataset))  {
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ifield_names_file_txt='ukbrapr_data/fieldnames_cancer_registry.txt' -ientity='participant' -ioutput='cancer_registry' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

}



#' Extract useful baseline date fields and save to file
#'
#' @description In the UK Biobank RAP, submit a table-exporter job to extract some useful baseline fields:
#'
#' 1) date of baseline assessment [p53_i*], 2) month of birth [p52], 3) year of birth [p34], and 4) sex [p31]
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name export_tables_baseline_dates
#'
export_tables_baseline_dates <- function(
	dataset = NULL,
	submit = FALSE,
	verbose = FALSE
)  {
	
	cli::cli_alert("Export table: Baseline Dates fields")

	# get dataset id 
	if (is.null(dataset))  {
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	table_exporter_command = stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ifield_names=\"eid\" -ifield_names=\"p53_i0\" -ifield_names=\"p53_i1\" -ifield_names=\"p53_i2\" -ifield_names=\"p53_i3\" -ifield_names=\"p52\" -ifield_names=\"p34\" -ifield_names=\"p53_i0\" -ifield_names=\"p31\" -ientity='participant' -ioutput='baseline_dates' -ioutput_format='TSV' -icoding_option='RAW' --destination ukbrapr_data/")
	if (verbose|submit) cli::cli_text(table_exporter_command)
	if (submit)  system(table_exporter_command)

}

