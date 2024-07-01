#' Install data for ukbrapR functions
#'
#' @description In the UK Biobank RAP, install the data used by ukbrapR functions. 
#' 
#' If the user has just installed the package and not run before, then the data is extracted from the Spark environment to files for local use, but are also saved to the RAP space. This can take a little while to get all sources.
#'
#' If this function is run on any subsequent uses, data is copied from the RAP space to the worker node, ready for use as required. This means ukbrapR only needs to be run on a Spark cluster the first time, to get the data, then can be used on "normal" nodes.
#'
#' @return Nothing returned to environment. Data is saved to files for later use.
#'
#' @author Luke Pilling
#'
#' @name install_ukbrap_data
#'
#' @examples
#'
#' @export
#'
install_ukbrap_data <- function(
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	# does worker directory exist? create if not 
	dir.create(file.path("~/ukbrap_data/"), showWarnings = FALSE)
	
	# check if data exists in RAP space already - if so, just copy it over
	fname_rap = file.path("/mnt/project/", ukbrapR:::ukbrap_paths$path[ ukbrap_paths$object=="selfrep_illness" ] )
	fname_worker = file.path("~", ukbrapR:::ukbrap_paths$path[ ukbrap_paths$object=="selfrep_illness" ] )
	if (file.exists(fname_worker))  {
		if (verbose) cli::cli_alert("Found `selfrep_illness` on the worker, skipping")
	} else if (file.exists(fname_rap))  {
		if (verbose) cli::cli_alert("Found `selfrep_illness` in RAP space, copying to worker")
		system(str_c("cp ", fname_rap, " ", fname_worker))
	} else {
		if (verbose) cli::cli_alert("Did not find `selfrep_illness` in RAP space, getting from Spark")
		ukbrapR:::install_ukbrap_data_selfrep_illness(verbose=verbose)
	}
	
}



#' Extract self-reported illess fields from Spark and save to file
#'
#' @description In the UK Biobank RAP, extract (from Spark) the self-reported illness fields to a file + upload to RAP space
#'
#' @return NA
#'
#' @author Luke Pilling
#'
#' @name install_ukbrap_data_selfrep_illness
#'
install_ukbrap_data_selfrep_illness <- function(
	n_cancer_arrays = 5,
	n_noncancer_arrays = 30,
	verbose = FALSE
)  {
	
	start_time <- Sys.time()
	
	#  need: eid, 20001, 20002, 20006, 20007, 20008, 20009
	
	# Determine variable names needed (depends if cancer or non-cancer)
	#   will use 20001 (cancer code) and 20002 (non-cancer code)
	#   will use the interpolated year (20006 = cancer year, 20008 = non-cancer year)
	
	# RAP stores arrays as separate variables
	
	# get field names 
	if (verbose) cli::cli_alert("Determine field names to request")
	if (verbose) cli::cli_alert(c("n_cancer_arrays = ", n_cancer_arrays))
	if (verbose) cli::cli_alert(c("n_noncancer_arrays = ", n_noncancer_arrays))
	names = "eid"
	
	#   cancer code = 20001
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 0, "_a", a))
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 1, "_a", a))
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 2, "_a", a))
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20001_i", 3, "_a", a))
	#   cancer year = 20006
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 0, "_a", a))
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 1, "_a", a))
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 2, "_a", a))
	for (a in c(0:n_cancer_arrays))  names <- c(names, stringr::str_c("p20006_i", 3, "_a", a))
	
	#   non-cancer illness code = 20002
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 0, "_a", a))
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 1, "_a", a))
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 2, "_a", a))
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20002_i", 3, "_a", a))
	#   non-cancer illness year = 20008
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 0, "_a", a))
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 1, "_a", a))
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 2, "_a", a))
	for (a in c(0:n_noncancer_arrays))  names <- c(names, stringr::str_c("p20008_i", 3, "_a", a))
	
	if (verbose) print(names)

	# get fields from the RAP
	if (verbose) cli::cli_alert("Get fields from the RAP")
	ukb_dat <- ukbrapR::get_rap_phenos(names, value_coding = "raw", verbose = verbose)
	
	# save & upload
	dir.create(file.path("~/ukbrap_data/"), showWarnings = FALSE)
	fname_worker = file.path("~", ukbrapR:::ukbrap_paths$path[ ukbrap_paths$object=="selfrep_illness" ] )
	readr::write_tsv(ukb_dat, fname_worker)
	ukbrapR::upload_to_rap(file=fname_worker, dir="ukbrap_data/")
	
	# finished
	time_taken = as.numeric(difftime(Sys.time(), start_time, units="secs"))
	cli::cli_alert_success(c("Got self-reported illness fields. Time taken: ", "{prettyunits::pretty_sec(time_taken)}."))

}

