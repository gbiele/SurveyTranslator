#' Extract Example Translations from Excel
#'
#' @description
#' Reads an Excel file containing survey translations and returns source–target pairs.
#' Filters by instrument and allows optional source/target reversal.
#'
#' @param file_path A string. Path to the Excel file (.xlsx) with a tabular format.
#'   Required columns: \code{Type}, \code{Instrument}, \code{Source}, and \code{Target}.
#' @param rev_ex Logical. If \code{TRUE}, reverses source and target columns.
#' @param instrument Optional string. Filters rows where \code{Instrument} matches.
#'
#' @returns
#' A \code{data.table} with two columns:
#' \itemize{
#'   \item \code{source_item}: Source language text.
#'   \item \code{target_item}: Corresponding translation.
#' }
#'
#' @importFrom readxl read_xlsx
#' @export
example_translations = function(file_path, instrument = NULL, rev_ex = FALSE) {

  examples = data.table(read_xlsx(file_path))

  if (!is.null(instrument))
    examples = examples[Instrument == instrument]

  if (rev_ex == TRUE)
    data.table::setnames(examples,c("Source","Target"),c("Target","Source"))

  data.table::setnames(examples,c("Source","Target"),c("source_item","target_item"))

  return(examples[, .(source_item, target_item)])

}
