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
eval_translations <- function(translated_items, back_translated_items, guidelines = default_backtrans_guidelines(), batch_size = 15, chat = NULL, api_key = NULL, llm_model = NULL, sleep = 1, tmp_path = NULL, restart = FALSE) {
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

  num_rows <- nrow(round_trip)
  batch_size = min(batch_size,num_rows)
  all_parsed_responses <- list()
  num_batches <- ceiling(num_rows / batch_size)

  if (is.null(chat)) chat <- .get_chat(model = llm_model, api_key = api_key)

  first_idx = 1
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
    p2_batch <- prompt_evaluation(round_trip_json_batch, guidelines = guidelines) # Assuming prompt_evaluation is defined

    eval_response_batch <- chat$chat(p2_batch)
    clean_response_json_batch <- gsub("```json|```", "", eval_response_batch)

    parsed_response_batch <- tryCatch({
      data.table::data.table(jsonlite::fromJSON(clean_response_json_batch))
    }, error = function(e) {
      warning(paste0("Batch ", i, ": Failed to parse LLM's JSON response: ", e$message))
      warning(paste0("Batch ", i, ": Raw LLM response was:\n", eval_response_batch))
      NULL # Return NULL for this batch on error, it won't be added to the list
    })

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
