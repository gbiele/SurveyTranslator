#' @title Evaluate Survey Translations with an LLM
#'
#' @description Evaluates translation and back-translation equivalence (semantic and tonal) using an LLM. Handles data preparation, prompt generation, API call, and response parsing.
#'
#' @param translated_items A `data.table` or `data.frame` with original ('Text') and translated ('translated_item') survey texts.
#' @param back_translated_items A `data.table` or `data.frame` with back-translated texts ('translated_item').
#' @param guidelines A string describing the evaluation. Defaults to `default_eval_guidelines()`. Custom guidelines influence evaluation emphasis (e.g., semantics, tone) or strictness.
#' @param batch_size Numeric. Number of items to process in each batch. Defaults to 15.
#' @param chat An `ellmer` chat client object. If `NULL`, initialized using `llm_model` and `api_key`.
#' @param api_key Your LLM API key. Required if `chat` is `NULL`.
#' @param llm_model The LLM model name.
#' @param sleep Numeric. Seconds to pause between batches. Defaults to 1.
#' @param tmp_path Character. Path to a temporary file for saving progress. Defaults to "tmp.Rdata".
#' @param restart Logical. If `TRUE`, attempts to restart from progress saved in `tmp_path`. Defaults to `FALSE`.
#'
#' @return A `data.table` of evaluation results from the LLM, or an empty `data.table` if no data was successfully parsed.
#'
#' @details Requires `data.table` and `jsonlite`. Assumes `translated_items` and `back_translated_items` are row-aligned. Assumes `prompt_evaluation()` is defined and correctly generates an LLM prompt from the round-trip JSON data. The LLM response should be JSON, optionally markdown-wrapped (e.g., ```json...```).
#'
#' @importFrom data.table data.table setnames
#' @importFrom jsonlite toJSON fromJSON
#'
#' @export
eval_translations <- function(translated_items, back_translated_items, guidelines = default_eval_guidelines(), batch_size = 15, chat = NULL, api_key = NULL, llm_model = NULL, sleep = 1, tmp_path = "tmp.Rdata", restart = FALSE) {
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

  round_trip <-
    merge(setnames(translated_items[, .(Text,translated_item,id)],
                   c("Text","translated_item"),c("original","translation")),
          setnames(back_translated_items[, .(translated_item,id)],
                   c("translated_item"),c("back_translation")),
          by = "id")

  num_rows <- nrow(round_trip)
  batch_size = min(batch_size,num_rows)
  all_parsed_responses <- list()
  num_batches <- ceiling(num_rows / batch_size)

  i = first_idx = 1
  if (restart == TRUE) {
    load(tmp_path)
    first_idx = i
  }

  for (i in first_idx:num_batches) {
    start_index <- (i - 1) * batch_size + 1
    end_index <- min(i * batch_size, num_rows) # Ensure we don't go out of bounds

    current_batch_dt <- round_trip[start_index:end_index, ]
    message(paste0("Processing batch ", i, "/", num_batches, " (rows ", start_index, "-", end_index, ")"))

    round_trip_json_batch <- jsonlite::toJSON(current_batch_dt, pretty = TRUE, auto_unbox = TRUE)
    chat <- .get_chat(model = llm_model, api_key = api_key)
    p2_batch <- prompt_evaluation(round_trip_json_batch, guidelines = guidelines) # Assuming prompt_evaluation is defined

    parsed_response_batch <- .llm_response(p2_batch, chat = chat)
    parsed_response_batch = parsed_response_batch[id %in% current_batch_dt$id]
    if (nrow(parsed_response_batch) != nrow(round_trip[start_index:end_index])) {
      x = 3
      warning("Wrong number of items returned. Trying again with gemini-2.5-pro-preview-03-25")
      chat = .get_chat(model = "gemini-2.5-pro-preview-03-25")
      parsed_response_batch <- .llm_response(p2_batch, chat = chat)
      parsed_response_batch = parsed_response_batch[id %in% current_batch_dt$id]
    }

    if (!is.null(parsed_response_batch) && nrow(parsed_response_batch) > 0) {
      all_parsed_responses[[length(all_parsed_responses) + 1]] <- parsed_response_batch
    } else if (!is.null(parsed_response_batch) && nrow(parsed_response_batch) == 0) {
      warning(paste0("Batch ", i, ": Parsed response was an empty data.table."))
    }


    if (i < num_batches) {
      Sys.sleep(sleep) # Sleep for 1 second
    }

    save(all_parsed_responses,i, file = tmp_path)
  }

  # Combine all successfully parsed batch results
  if (length(all_parsed_responses) > 0) {
    parsed_response <- data.table::rbindlist(all_parsed_responses, use.names = TRUE, fill = TRUE)
  } else {
    warning("No data was successfully parsed from any batch.")
    parsed_response <- data.table::data.table() # Return an empty data.table if no batches succeeded
  }

  return(parsed_response)
}
