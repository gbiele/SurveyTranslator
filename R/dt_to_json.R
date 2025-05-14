#' Convert a data.table to JSON format
#'
#' This function converts a data.table with 'Source' and 'Target' columns
#' into a specific JSON format with 'source_item' and 'target_item' keys.
#'
#' @param dt A data.table object containing 'Source' and 'Target' columns
#'
#' @return A JSON string with the converted data
#'
#' @examples
#' dt <- data.table(
#'   Source = c("How satisfied are you with your current job?"),
#'   Target = c("Quel est votre degré de satisfaction à l'égard de votre emploi actuel ?")
#' )
#' json_result <- dt_to_json(dt)
#'
#' @importFrom data.table is.data.table
#' @importFrom jsonlite toJSON
#'
#' @export
dt_to_json <- function(dt) {
  # Check if the input is a data.table
  if (!is.data.table(dt)) {
    stop("Input must be a data.table object")
  }

  # Check if the required columns exist
  if (!all(c("Source", "Target") %in% names(dt))) {
    stop("Input data.table must contain 'Source' and 'Target' columns")
  }

  # Create a list with renamed columns
  json_list <- lapply(1:nrow(dt), function(i) {
    list(
      source_item = dt[i, Source],
      target_item = dt[i, Target]
    )
  })

  # Convert to JSON with pretty formatting
  json_string <- toJSON(json_list, pretty = TRUE, auto_unbox = TRUE)

  return(json_string)
}
