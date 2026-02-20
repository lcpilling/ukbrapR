#' Check UK Biobank field IDs
#'
#' @description Check if provided field IDs are valid and return all possible phenotype names in the UK Biobank RAP
#'
#' @return Returns a vector of strings (valid phenotypes).
#'
#' @author Luke Pilling
#'
#' @name fields_to_phenos
#'
#' @param fields A vector of character strings. The field IDs to check if valid.
#' @param filename A string. If provided, will save as a fieldname file ready for the table-exporter (including "eid").
#'        \code{default=""}
#' @param abort Logical. Abort if a field is missing?,
#'        \code{default=TRUE}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # not instanced, not arrayed
#' fields_to_phenos("31")  # sex
#'
#' # instanced, not arrayed
#' fields_to_phenos("53")  # assessment date
#'
#' # instanced and arrayed
#' fields_to_phenos("93")  # systolic blood pressure, manual reading
#'
#' # instanced and arrayed, MRI assessments only
#' fields_to_phenos("12673") # Heart rate recorded during vicorder run (Heart MRI)
#'
#' # check multiple simultaneously
#' fields_to_phenos(c("31","93"))
#'
#' # only warn if an invalid field is provided (default is to abort)
#' fields_to_phenos(c("31","notafield","93"))
#' fields_to_phenos(c("31","notafield","93"), abort=FALSE)
#'
#' # save as fieldname file for the table-exporter (don't forget to upload to the RAP)
#' fields_to_phenos(c("31","93"), filename="fieldnames.txt")
#'
#' @export
#'
fields_to_phenos <- function(
  fields,
  filename="",
  abort=TRUE,
  verbose=FALSE
)  {
	
	# start up messages
	.ukbrapr_startup_notice()
    
  # Check if 'field' is a character string of length 1
  if (class(fields)[1] != "character")  {
    cli::cli_abort("Provided fields need to be a vector of character strings") # Abort if field is not a character string
  }
  
  # If filename provided, check it is valid
  save_file <- FALSE
  if (filename != "")  {
    if (! is.character(filename))  cli::cli_abort("Provided filename need to be a character string")
    if (! length(filename) == 1)   cli::cli_abort("Provided filename need to be a character string of length 1")
    save_file <- TRUE
  }
  
  # Apply field_to_phenos() to each field
  phenos <- purrr::map(fields, \(x) ukbrapR:::field_to_phenos(field=x, abort=abort, verbose=verbose)) |> purrr::list_c()
  
  # Save file, or return vector?
  if (save_file)  {
    data.frame(c("eid",phenos)) |> readr::write_tsv(filename, col_names=FALSE, progress=FALSE)
    cli::cli_alert_success(stringr::str_c("Saved fields and phenos to {.file ", filename, "}"))
  } else {
    return(phenos)
  }
  
}


#' Function to check individual field_id
#'
#' @return Returns a vector of strings (valid phenotypes).
#'
#' @author Luke Pilling
#'
#' @name field_to_phenos
#'
#' @param field A string. The field ID (e.g., `31`) to check if valid.
#' @param abort Logical. Abort if field is missing?,
#'        \code{default=TRUE}
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @noRd
#'
field_to_phenos <- function(
  field,
  abort=TRUE,
  verbose=FALSE
)  {
	
	# start up messages
	.ukbrapr_startup_notice()

  # Check if 'field' is a character string of length 1
  if (class(field)[1] == "character")  {
    if (length(field) > 1)  {
      cli::cli_abort("field needs to be length 1") # Abort if field length is greater than 1
    }
  } else {
    cli::cli_abort("field needs to be a character string") # Abort if field is not a character string
  }

  # Extract the field ID, in case it's passed as 'p4080_i0_a0' or similar
  field_id <- field |> stringr::str_remove("p") |> stringr::str_split_i("_", 1)
  if (verbose)  cli::cli_alert(c("Field ID: ", field_id))
  
  # Get information for this field from the schema
  field_info <- ukbrapR:::ukb_schema[["field"]] |> dplyr::filter(field_id == !!field_id)
  
  # Was the provided field ID in the schema?
  if (nrow(field_info)==0)  {
    if (abort)   cli::cli_abort(stringr::str_c("Field ID [", field_id, "] not present in the UK Biobank schema (https://biobank.ctsu.ox.ac.uk/ukb/schema.cgi?id=1)"))
    if (!abort)  {
      cli::cli_warn(stringr::str_c("Field ID [", field_id, "] not present in the UK Biobank schema (https://biobank.ctsu.ox.ac.uk/ukb/schema.cgi?id=1)"))
      return(NULL)
    }
  }
  
  # Initialize the basic field ID and valid fields list
  p_field_id <- stringr::str_c("p", field_id)
  valid_fields <- NULL
  
  # Check if the field is instanced and generate instances if true
  if (field_info$instanced == 1)  {
    instances <- seq(field_info$instance_min, field_info$instance_max, 1)
    if (verbose)  cli::cli_alert(stringr::str_c("Is instaced [", stringr::str_c(instances, collapse=","), "]")) 
  }
  
  # Check if the field is arrayed and generate arrays if true
  # If the field is "multiple choice" value type then it is *not* arrayed
  if (field_info$arrayed == 1 & field_info$value_type != 22)  {
    arrays <- seq(field_info$array_min, field_info$array_max, 1)
    if (verbose)  cli::cli_alert(stringr::str_c("Is arrayed [", stringr::str_c(arrays, collapse=","), "]")) 
  
    # Generate valid fields for non-instanced arrayed fields
    if (field_info$instanced == 0)  {
      for (aa in 1:length(arrays))  {
        valid_fields <- c(valid_fields, stringr::str_c(p_field_id, "_a", arrays[aa]))
      }
    }
    
    # Generate valid fields for instanced arrayed fields
    if (field_info$instanced == 1)  {
      for (ii in 1:length(instances))  {
        for (aa in 1:length(arrays))  {
          valid_fields <- c(valid_fields, stringr::str_c(p_field_id, "_i", instances[ii], "_a", arrays[aa]))
        }
      }
    }
  
  }  else  {
  
    # Generate valid fields for instanced non-arrayed fields
    if (field_info$instanced == 1)  {
      for (ii in 1:length(instances))  {
        valid_fields <- c(valid_fields, stringr::str_c(p_field_id, "_i", instances[ii]))
      }
    } else {
    
      # not instanced or arrayed!
      valid_fields <- p_field_id
    
    }
  
  }
  
  # Return the list of valid fields for this field ID
  return(valid_fields)
  
}
