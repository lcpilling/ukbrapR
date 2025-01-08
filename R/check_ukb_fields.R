


valid_ukb_fields <- function(
  field,
  verbose=FALSE
)  {
  
  # check it's a character string of length 1
  if (class(field)[1] == "character")  {
    if (length(field)>1)  cli::cli_abort("field needs to be length 1")
  } else {
    cli::cli_abort("field needs to be a character string")
  }

  # get field ID -- in case passed as p4080_i0_a0 or similar
  field_id <- field |> stringr::str_remove("p") |> stringr::str_split_i("_", 1)
  if (verbose) cli::cli_alert(field_id)
  
  # get info for this field
  field_info <- ukbrapR:::ukb_schema[["field"]] |> dplyr::filter(field_id==!!field_id)
  
  # basic field
  p_field_id <- stringr::str_c("p", field_id)
  valid_fields <- NULL
  
  # instanced?
  if (field_info$instanced == 1)  instances <- seq(field_info$instance_min, field_info$instance_max, 1)
  
  # arrayed?
  if (field_info$arrayed == 1)  {
    arrays <- seq(field_info$array_min, field_info$array_max, 1)
    if (field_info$instanced == 0)  for (aa in 1:length(arrays))  valid_fields <- c(valid_fields, stringr::str_c(p_field_id, "_a", arrays[aa]))
    if (field_info$instanced == 1)  {
      for (ii in 1:length(instances))  {
        for (aa in 1:length(arrays))  {
          valid_fields <- c(valid_fields, stringr::str_c(p_field_id, "_i", instances[ii], "_a", arrays[aa]))
        }
      }
    }
  }
  
  # return valid fields for this field ID
  return(valid_fields)
  
}
