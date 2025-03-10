#' Check if provided string is a valid UK Biobank field
#'
#' @description 
#'
#' @return Returns a vector of strings (valid fields).
#'
#' @author Luke Pilling
#'
#' @name validate_ukb_field
#'
#' @param field A string. The field (e.g., `p54_i0`) to check if valid.
#' @param verbose Logical. Be verbose,
#'        \code{default=FALSE}
#'
#' @examples
#'
#' # update the Assessment Centre variable
#' ukbrapR:::validate_ukb_field(field="p54_i0")
#'
validate_ukb_field <- function(
  field,
  verbose=FALSE
)  {
  
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
  if (verbose)  {
    cli::cli_alert(field_id) 
  }
  
  # Get information for this field from the schema
  field_info <- ukbrapR:::ukb_schema[["field"]] |> dplyr::filter(field_id == !!field_id)
  
  # Initialize the basic field ID and valid fields list
  p_field_id <- stringr::str_c("p", field_id)
  valid_fields <- NULL
  
  # Check if the field is instanced and generate instances if true
  if (field_info$instanced == 1)  {
    instances <- seq(field_info$instance_min, field_info$instance_max, 1)
  }
  
  # Check if the field is arrayed and generate arrays if true
  if (field_info$arrayed == 1)  {
    arrays <- seq(field_info$array_min, field_info$array_max, 1)
    
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
  }
  
  # Return the list of valid fields for this field ID
  return(valid_fields)
  
}
