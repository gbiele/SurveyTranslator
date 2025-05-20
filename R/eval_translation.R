#' @title Evaluate Survey Translations with an LLM
#'
#' @description Evaluates translation and back-translation equivalence (semantic and tonal) using an LLM.
#'   Handles data preparation, prompt generation, API call, and response parsing.
#'
#' @param translated_items A `data.table` or `data.frame` with original ('Text')
#'   and translated ('translated_item') survey texts.
#' @param back_translated_items A `data.table` or `data.frame` with back-translated
#'   texts ('translated_item').
#' @param guidelines a string with the description of the evaluation is performed.
#' Default is `default_backtrans_guidelines()`.by setting own guidelines one can
#' influence emphasis (e.g. semantics, tone) or strictness of the evaluation.
#' @param chat LLM chat client object. If `NULL`, initialized using `llm_model` and `api_key`.
#' @param api_key Your LLM API key. Required if `chat` is `NULL`.
#' @param llm_model The LLM model name.
#'
#' @return A `data.table` of evaluation results from the LLM, or `NULL` on error.
#'
#' @details Requires `data.table` and `jsonlite`. Assumes `translated_items` and
#'   `back_translated_items` are row-aligned. Assumes `prompt_evaluation()` is
#'   defined and correctly generates an LLM prompt from the round-trip JSON data.
#'   The LLM response should be JSON, optionally markdown-wrapped (e.g., ```json...```).
#'
#' @importFrom data.table data.table
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
eval_translations <- function(translated_items, back_translated_items, guidelines = default_backtrans_guidelines(), chat = NULL, api_key = NULL, llm_model = NULL) {
  if (!inherits(translated_items, "data.frame")) {
    stop("`translated_items` must be a data.frame or data.table.")
  }
  if (!inherits(back_translated_items, "data.frame")) {
    stop("`back_translated_items` must be a data.frame or data.table.")
  }

  required_translated_cols <- c("Text", "translated_item")
  if (!all(required_translated_cols %in% names(translated_items))) {
    stop(paste0("`translated_items` must contain columns: '", paste(required_translated_cols, collapse = "', '"), "'."))
  }

  required_back_translated_col <- "translated_item"
  if (!(required_back_translated_col %in% names(back_translated_items))) {
    stop(paste0("`back_translated_items` must contain column: '", required_back_translated_col, "'."))
  }

  if (nrow(translated_items) != nrow(back_translated_items)) {
    stop("Number of rows in `translated_items` (", nrow(translated_items), ") and `back_translated_items` (", nrow(back_translated_items), ") do not match. They must be aligned row-wise.")
  }

  round_trip <- data.table::data.table(
    original = translated_items$Text,
    translation = translated_items$translated_item,
    back_translation = back_translated_items$translated_item
  )

  round_trip_json <- jsonlite::toJSON(round_trip, pretty = TRUE, auto_unbox = TRUE)

  p2 <- prompt_evaluation(round_trip_json, guidelines = guidelines)

  if (is.null(chat)) chat = .get_chat(model = llm_model, api_key = api_key)
  eval_response <- chat$chat(p2)

  clean_response_json <- gsub("```json|```", "", eval_response)

  parsed_response <- tryCatch({
    data.table::data.table(jsonlite::fromJSON(clean_response_json))
  }, error = function(e) {
    warning("Failed to parse LLM's JSON response: ", e$message)
    warning("Raw LLM response was:\n", eval_response)
    invisible(NULL) # Return NULL invisibly on error
  })

  return(parsed_response)
}
