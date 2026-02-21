#' Update UK Biobank field with `title` and `label` from the schema
#'
#' @description Variables such as education and ethnicity are provided as integers but have specific codes.
#'
#' The UK Biobank schema are machine-readable dictionaries and mappings defining the internal structure of the online Showcase. https://biobank.ctsu.ox.ac.uk/crystal/schema.cgi
#'
#' This function updates a field in a data frame of UK Biobank with information from the Schema.
#'
#' @return Returns a data frame.
#'
#' @author Luke Pilling
#'
#' @name label_ukb_field
#'
#' @param d A data frame. The data frame containing the `field` to update.
#' @param field A string. The field (e.g., `p54_i0`) in the provided data frame to update.
#' @param field_id A string. If the field has been renamed (to e.g.,"assessment_centre") provide the field id here (e.g., "54").
#'        \code{default=NULL}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # update the Assessment Centre variable
#' ukb <- ukbrapR::label_ukb_field(ukb, field="p54_i0")
#'
#' table(ukb$p54_i0)                   # tabulates the values
#' table(haven::as_factor(ukb$p54_i0)) # tabulates the labels
#' haven::print_labels(ukb$p54_i0)     # show the value:label mapping for this variable
#'
#' # if the variable has been renamed, provide the "field" and "field_id" e.g.,
#' ukb <- ukbrapR::label_ukb_field(ukb, field="assessment_centre", field_id="54")
#'
#' @export
#'
label_ukb_field <- function(
	d,              # the data frame
	field,          # the variable name in the data
	field_id=NULL,  # if not in UKB format, provide the field ID
	verbose=FALSE
)  {

	# get symbol to use as variable name later
	if (verbose) cat(" - ", field, "\n")
	field_sym <- rlang::sym(field)

	# get field ID
	if (is.null(field_id))  field_id <- field |> stringr::str_remove("p") |> stringr::str_split_i("_", 1)
	if (verbose) print(" -- ", field_id, "\n")

	# get encoding ID & variable title
	field_eid   <- ukbrapR:::ukb_schema[["field"]] |> dplyr::filter(field_id==!!field_id) |> dplyr::select(encoding_id) |> dplyr::pull()
	field_title <- ukbrapR:::ukb_schema[["field"]] |> dplyr::filter(field_id==!!field_id) |> dplyr::select(title) |> dplyr::pull()

	# search schema for coding
	index <- 0
	for (i in 2:6)  if (field_eid %in% ukbrapR:::ukb_schema[[i]]$encoding_id)  index <- i

	# if index was found, include labels, otherwise just update title
	if (index != 0)  {

		# subset and convert to named vector
		field_coding <- ukbrapR:::ukb_schema[[index]] |>
			dplyr::filter(encoding_id == field_eid) |>
			dplyr::select(meaning, value) |>
			tibble::deframe()

		# skip labelling if values contain the "pipe" (implies collapsed array variables)
		if ( ! any(stringr::str_detect(na.omit(d[[field]]), stringr::fixed("|"))) )  {

			# update variable label
			d <- d |> dplyr::mutate(
				!! field_sym := haven::labelled(
					!! field_sym,
					labels = !! field_coding,
					label = !! field_title
					)
				)

		}

	}  else  {

		# update variable title
		d <- d |> dplyr::mutate(
			!! field_sym := haven::labelled(
				!! field_sym,
				label = !! field_title
				)
			)

	}

	return(d)

}


#' Update a data frame containing UK Biobank fields with `title` and `label` from the schema
#'
#' @description Variables such as education and ethnicity are provided as integers but have specific codes.
#'
#' The UK Biobank schema are machine-readable dictionaries and mappings defining the internal structure of the online Showcase. https://biobank.ctsu.ox.ac.uk/crystal/schema.cgi
#'
#' This function updates a data frame of UK Biobank data field with information from the Schema.
#'
#' It is in effect a wrapper to apply ukbrapR::label_ukb_field() to each variable in a data frame that looks like a UK Biobank field.
#'
#' Only recognised fields are modified (variables named things like "p54_i0"). Other variables are ignored.
#'
#' @return Returns a data frame.
#'
#' @author Luke Pilling
#'
#' @name label_ukb_fields
#'
#' @param d A data frame. The data frame containing UK Biobank fields to update.
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # say the below data frame contains 4 variables: `eid`, `p54_i0`, `p50_i0` and `age_at_assessment`
#' names(ukb)
#'
#' # update the variables that looks like UK Biobank fields with titles and, where cateogrical, labels
#' # i.e., `p54_i0` and `p50_i0` only -- `eid` and `age_at_assessment` are ignored
#' ukb <- ukbrapR::label_ukb_fields(ukb)
#'
#' table(ukb$p54_i0)                   # tabulates the values
#' table(haven::as_factor(ukb$p54_i0)) # tabulates the labels
#' haven::print_labels(ukb$p54_i0)     # show the value:label mapping for this variable
#'
#' @export
#'
label_ukb_fields <- function(
	d,
	verbose=FALSE
)  {

	# start up messages
  pkg_version <- utils::packageVersion("ukbrapR")
  cli::cli_alert_info("{.pkg ukbrapR} v{pkg_version}")
  .ukbrapr_startup_notice()

	start_time <- Sys.time()

	# identify variables matching UK Biobank field format: start with "p", then have an integer before any "_"
	fields <- colnames(d)
	fields_int <- fields |>
		stringr::str_subset("^p") |>
		stringr::str_replace("p", "") |>
		stringr::str_split_i("_", 1)
	int_check <- function(vect) {
		vect <- as.character(vect)
		vect_int <- purrr::map(vect, \(x) all(unlist(stringr::str_split(x, "")) %in% 0:9)) |> purrr::list_c()
		return(vect[vect_int])
	}
	fields_int <- int_check(fields_int)
	fields <- fields[ stringr::str_detect( fields, stringr::str_c( stringr::str_c("p", fields_int), collapse="|") ) ]

	# any matched?
	n_fields <- length(fields)
	if (n_fields==0)  cli::cli_abort(c("x" = "No UK Biobank fields (e.g., 'p54_i0') identified in the provided data frame."))

	# label each field identified
	cli::cli_alert("Labelling {n_fields} field{?s}")
	for (f in fields)  d <- ukbrapR::label_ukb_field(d=d, field=f, verbose=verbose)

	# finished!
	cli::cli_alert_success(c("Finished. Time taken: ", "{prettyunits::pretty_sec(as.numeric(difftime(Sys.time(), start_time, units=\"secs\")))}."))

	# return it
	return(d)

}
