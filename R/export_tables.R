#' Export tables for {ukbrapR} analyses
#'
#' @description In the UK Biobank RAP export tables for HES, GP, death, and cancer registry data, plus self-reported illness fields, using the table-exporter
#'
#' This is essentially a wrapper function to submit jobs to the table exporter.
#'
#' Unfortunately, you will be asked for each submission to:
#'
#' - "Confirm running..." choose [Y], and 
#'
#' - whether you want to "Watch launched job..." choose [n]
#'
#' Once the jobs are completed it is important to move the files to `ukbrap_data` directory in your project to keep the files tidy
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name export_tables
#'
#' @param dataset A string. If you wish to specify dataset. If blank, will use the first in the project.
#'        \code{default=app#####.dataset}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#' 
#' Choose [Y] and [n] each time asked whether to confirm submission [Y] and watch the job [n]
#' export_tables()
#'
#' @export
#'
export_tables <- function(
	dataset = NULL,
	verbose = FALSE
)  {
	
	ukbrapR:::export_tables_emr(dataset=dataset, verbose=verbose)
	ukbrapR:::export_tables_selfrep_illness(dataset=dataset, verbose=verbose)
	ukbrapR:::export_tables_cancer_registry(dataset=dataset, verbose=verbose)
	
	cli::cli_alert_info("Once the jobs have all finished (~20mins), move the files to directory `ukbrap_data` in your project")
	
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
	verbose = FALSE
)  {

	if (verbose) cli::cli_alert("dx run table-exporter for Electronic Medical Records files")

	# get dataset id 
	if (is.null(dataset))  {
			# get dataset id - will just take the first
			#DATASET=($(ls /mnt/project/*.dataset))
			#DATASET=$(basename ${DATASET[0]})
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='death' -ioutput='death' -ioutput_format='TSV' -icoding_option='RAW'"))
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='death_cause' -ioutput='death_cause' -ioutput_format='TSV' -icoding_option='RAW'"))
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='hesin' -ioutput='hesin' -ioutput_format='TSV' -icoding_option='RAW'"))
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='hesin_diag' -ioutput='hesin_diag' -ioutput_format='TSV' -icoding_option='RAW'"))
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='hesin_oper' -ioutput='hesin_oper' -ioutput_format='TSV' -icoding_option='RAW'"))
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='gp_clinical' -ioutput='gp_clinical' -ioutput_format='TSV' -icoding_option='RAW'"))
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ientity='gp_scripts' -ioutput='gp_scripts' -ioutput_format='TSV' -icoding_option='RAW'"))

}




#' Extract self-reported illess fields from Spark and save to file
#'
#' @description In the UK Biobank RAP, extract (from Spark) the self-reported illness fields to a file + upload to RAP space
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
	verbose = FALSE
)  {
	
	if (verbose) cli::cli_alert("dx run table-exporter for Self-reported Illness fields")
	
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
	ukbrapR::upload_to_rap("fieldnames_selfrep_illness.txt", dir="ukbrap_data")

	# get dataset id 
	if (is.null(dataset))  {
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ifield_names_file_txt='ukbrap_data/fieldnames_selfrep_illness.txt' -ioutput='selfrep_illness' -ioutput_format='TSV' -icoding_option='RAW'"))


}



#' Extract cancer registry fields from Spark and save to file
#'
#' @description In the UK Biobank RAP, extract (from Spark) the cancer registry fields to a file + upload to RAP space
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
	verbose = FALSE
)  {
	
	if (verbose) cli::cli_alert("dx run table-exporter for Cancer Registry fields")

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
	ukbrapR::upload_to_rap("fieldnames_cancer_registry.txt", dir="ukbrap_data")

	# get dataset id 
	if (is.null(dataset))  {
		dataset = list.files("/mnt/project") |> stringr::str_subset(".dataset")
		dataset = dataset[1]
	}
	
	# submit table-exporter 
	system(stringr::str_c("dx run table-exporter -idataset_or_cohort_or_dashboard=", dataset, " -ifield_names_file_txt='ukbrap_data/fieldnames_cancer_registry.txt' -ioutput='cancer_registry' -ioutput_format='TSV' -icoding_option='RAW'"))

}

