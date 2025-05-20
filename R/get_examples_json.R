#' @title Load and Validate Example Translation Data
#' @description Loads a file or object and returns a JSON string of valid example data.
#' If `reverse_transl = TRUE`, swaps the names "source_item" and "target_item".
#' @param input Either a character path to an Excel or RDS file, or a `data.frame`/`data.table`.
#' @param reverse_transl Logical. If TRUE, swap "source_item" and "target_item".
#' @return A JSON string containing source-target item pairs.
#' @export
get_examples_json <- function(input, reverse_transl = FALSE) {
  if (is.character(input)) {
    stopifnot(file.exists(input))
    ext <- tools::file_ext(input)
    if (ext %in% c("xlsx", "xls")) {
      dt <- data.table::data.table(readxl::read_xlsx(input))
    } else if (ext == "rds") {
      dt <- data.table::as.data.table(readRDS(input))
    } else {
      stop("Unsupported file type. Only .xlsx, .xls, and .rds are supported.")
    }
  } else if (is.data.frame(input)) {
    dt <- data.table::as.data.table(input)
  } else {
    stop("Input must be a file path or a data.frame/data.table.")
  }

  required_cols <- c("source_item", "target_item")
  if (!all(required_cols %in% names(dt))) {
    stop("Input must contain the columns: source_item and target_item.")
  }

  if (reverse_transl) {
    setnames(dt, old = c("source_item", "target_item"), new = c("target_item", "source_item"))
  }

  jsonlite::toJSON(dt[, .(source_item, target_item)], pretty = TRUE)
}
