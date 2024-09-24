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
#' ukb <- ukbrapR::label_ukb_field(d=ukb, field="p54_i0")
#' 
#' table(ukb$p54_i0)                   # tabulates the values
#' table(haven::as_factor(ukb$p54_i0)) # tabulates the labels
#' haven::print_labels(ukb$p54_i0)   # show the value:label mapping for this variable
#' 
#' # get labels for all field names in the dataset
#' fields <- colnames(ukb)[2:ncol(ukb)]  # all columns except the first (usually `eid`)
#' for (f in fields)  ukb <- label_ukb_field(d=ukb, field=f)
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
	if (verbose) print(field)
	field_sym <- rlang::sym(field)
	
	# get field ID
	if (is.null(field_id))  field_id <- field |> stringr::str_remove("p") |> stringr::str_split_i("_", 1)
	if (verbose) print(field_id)
	
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

