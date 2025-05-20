# Define the TranslationItems S3 class and constructor
#' @title Create a TranslationItems object
#' @description Encapsulates survey items and translation parameters for use in LLM-based translation.
#' @param data A `data.table`, `data.frame`, a path to a text file (1 item per line), or an Excel file. Must result in a table with a `Text` column.
#' @param source_language Source language (default: "English").
#' @param target_language Target language (default: "Norwegian").
#' @param examples Either a `data.frame` with columns `source_item`, `target_item`, or a file path to an Excel file.
#' @param reverse_transl A logical determining if the source and target designationof the examples should be reversed (for back translation).
#' @param domain Domain or topic context (default: "Youth mental health").
#' @param guidelines Optional character vector of translation guidelines.
#' @param batch_size Optional integer for fixed-size batching.
#' @param batch_vars Optional character vector for variable-based batching.
#' @param get_instr Logical. Include instructions/responses (default: TRUE).
#' @return A `TranslationItems` object.
#' @export
prep_TranslationItems <- function(data,
                                  source_language = "English",
                                  target_language = "Norwegian",
                                  examples = NULL,
                                  reverse_transl = FALSE,
                                  domain = "Youth mental health",
                                  guidelines = NULL,
                                  batch_size = NULL,
                                  batch_vars = NULL,
                                  get_instr = TRUE) {
  if (is.character(data) && file.exists(data)) {
    ext <- tools::file_ext(data)
    if (ext %in% c("xlsx", "xls")) {
      data <- data.table::data.table(readxl::read_xlsx(data))
    } else {
      # Assume plain text file: one item per line
      lines <- readLines(data, warn = FALSE)
      data <- data.table::data.table(Text = trimws(lines))
    }
  } else if (is.data.frame(data)) {
    data <- data.table::as.data.table(data)
  }

  stopifnot("Text" %in% names(data))

  example_translations_json = get_examples_json(examples, reverse_transl = reverse_transl)

  structure(
    list(
      data = data,
      source_language = source_language,
      target_language = target_language,
      domain = domain,
      guidelines = guidelines,
      batch_size = batch_size,
      batch_vars = batch_vars,
      example_translations = example_translations_json,
      get_instr = get_instr
    ),
    class = "TranslationItems"
  )
}
