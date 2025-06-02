# Define the TranslationItems S3 class and constructor
#' @title Create a TranslationItems object
#' @description Encapsulates survey items and translation parameters for use in LLM-based translation.
#' @param data A `data.table`, `data.frame`, a path to a text file (1 item per line), or an Excel file. Must result in a table with a `Text` column.
#' @param source_language Source language (default: "English").
#' @param target_language Target language (default: "Norwegian").
#' @param example_trans Example translations. Either a `data.frame` with columns `source_item`, `target_item`, or a file path to an Excel file.
#' @param example_txt Example text from the domain in the target language.
#' @param reverse_transl A logical determining if the source and target designation of the examples should be reversed (for back translation).
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
                                  example_trans = NULL,
                                  example_txt = NULL,
                                  reverse_transl = FALSE,
                                  domain = "Youth mental health",
                                  guidelines = NULL,
                                  batch_size = NULL,
                                  batch_vars = NULL,
                                  topic_var = NULL,
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

  if (!is.null(example_trans)) {
    example_translations_json = get_examples_json(example_trans, reverse_transl = reverse_transl)
  } else {
    example_translations_json = NULL
  }

  if (!is.null(batch_vars)) {
    if (is.null(topic_var)) {
      stop("When the batch_vars argument is used, one of the batch_vars needs to specified as the topic_var")
    } else if (!(topic_var %in% batch_vars)){
      stop("When the batch_vars argument is used, one of the batch_vars needs to specified as the topic_var")
    }
  }



  structure(
    list(
      data = data,
      source_language = source_language,
      target_language = target_language,
      domain = domain,
      guidelines = guidelines,
      batch_size = batch_size,
      batch_vars = batch_vars,
      topic_var = topic_var,
      example_translations = example_translations_json,
      example_target_text = example_txt,
      get_instr = get_instr
    ),
    class = "TranslationItems"
  )
}
