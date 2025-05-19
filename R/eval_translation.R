#' @title Evaluate Survey Translations with an LLM
#'
#' @description This function orchestrates the process of creating a "round trip"
#'   translation data structure, converting it to JSON, generating an LLM prompt,
#'   sending the prompt to a large language model (e.g., Gemini), and parsing
#'   the LLM's JSON response to evaluate translation equivalence.
#'
#' @param translated_items A `data.table` (or `data.frame`) containing the original
#'   survey items and their initial translations. It is expected to have a column
#'   named 'Text' (for original items) and 'translated_item' (for the first translation).
#' @param back_translated_items A `data.table` (or `data.frame`) containing the
#'   back-translated survey items. It is expected to have a column named
#'   'translated_item' (for the back-translated items).
#' @param prompt_generator_fn A function that takes a JSON string of the round-trip
#'   data and generates an LLM-specific prompt object suitable for your `chat_client`.
#'   This would typically be your custom `prompt_back_trans` function.
#' @param chat_client An object representing the LLM client (e.g., a configured
#'   `gemini` client object) that has a `$chat()` method. This method should
#'   accept the output of `prompt_generator_fn` and return the LLM's raw response.
#'
#' @return A list or data.frame resulting from parsing the LLM's JSON response,
#'   which should contain the evaluation of the translation roundtrip for each item.
#'   Returns `NULL` invisibly if any required input columns are missing or if
#'   JSON parsing fails.
#'
#' @details
#'   It's important that `translated_items` and `back_translated_items` are aligned
#'   row-wise such that `translated_items$Text[i]` corresponds to `translated_items$translated_item[i]`
#'   which in turn corresponds to `back_translated_items$translated_item[i]`.
#'
#' @importFrom data.table data.table
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
eval_translations <- function(translated_items, back_translated_items, chat = NULL, api_key = NULL, llm_model = NULL) {
  # --- Input Validation ---
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

  # --- Create Round-Trip Data ---
  round_trip <- data.table::data.table(
    original = translated_items$Text,
    translation = translated_items$translated_item,
    back_translation = back_translated_items$translated_item
  )

  # --- Convert to JSON ---
  # auto_unbox = TRUE helps with single-element vectors if any column was just one value
  round_trip_json <- jsonlite::toJSON(round_trip, pretty = TRUE, auto_unbox = TRUE)

  # --- Generate Prompt ---
  p2 <- prompt_back_trans(round_trip_json)

  # --- Call LLM ---
  if (is.null(chat)) chat = .get_chat(model = llm_model, api_key = api_key)
  eval_response <- chat$chat(p2)

  # --- Parse LLM Response ---
  # Remove markdown code block delimiters if present
  # The gsub function is robust enough to handle cases where '```json' or '```' might not be present.
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
