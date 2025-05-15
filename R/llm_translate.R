#' Translate survey items using a language model
#'
#' Uses a large language model (LLM) to translate survey items from a plain text file.
#' Translation is guided by example translations extracted from an Excel file and shaped by a customizable prompt.
#' Supports batching for large item sets and automatic model selection.
#'
#' @param source_file Character. Path to a text file containing survey items (one item per line) to be translated.
#' @param example_file Character. Path to an Excel file containing example translations.
#' @param source_language Character. Source language used in the survey items. Default is `"English"`.
#' @param target_language Character. Target language for the translations. Default is `"Norwegian"`.
#' @param domain Character. Domain context for the prompt. Helps tailor the tone and terminology. Default is `"Youth mental health"`.
#' @param guidelines Character string or vector. Optional custom translation guidelines. If `NULL`, a default set of instructions is used.
#' @param example_type Character. Type of examples to extract from the Excel file. Passed to `example_translations()`. Default is `"Item"`, can also be `"Instructions"` or `"Response options"`,
#' @param chat Optional. An `ellmer` LLM chat object (e.g., from `ellmer::chat_google_gemini()`). If `NULL`, an appropriate chat object will be auto-detected via `.get_chat()`.
#' @param batch_size Optional integer. If provided, items are translated in batches of this size. If `NULL`, all items are translated in a single request.
#'
#' @return A `data.table` containing the translated survey items.
#'
#' @details
#' The function constructs a structured prompt using `base_prompt()`, filling in language, domain, and guideline parameters,
#' along with example translations (via `example_translations()`) and target items.
#'
#' If no `chat` object is supplied, the function auto-detects the available backend by checking for API keys in `.Renviron`
#' in the following order:
#' 1. Google Gemini (`GOOGLE_API_KEY`)
#' 2. OpenAI (`OPENAI_API_KEY`)
#' 3. Anthropic (`ANTHROPIC_API_KEY`)
#'
#' At least one of these keys must be set in your environment.
#'
#' Example translations are serialized to JSON using `dt_to_json()` and included in the prompt under the `examples` field.
#' The expected model output is a JSON list of objects, each containing `original_item` and `translated_item`.
#'
#' @examples
#' \dontrun{
#' llm_translate("data/items.txt")
#' llm_translate("data/items.txt", batch_size = 10, target_language = "Spanish")
#' }
#'
#' @export
llm_translate = function(source_file, example_file ,
                         source_language = "English", target_language = "Norwegian", domain = "Youth mental health", guidelines = NULL,
                         example_type = "Item", chat = NULL, batch_size = NULL) {

  bp = base_prompt(source_language = source_language, target_language = target_language, domain = domain, guidelines = guidelines)

  EXAMPLES =
    dt_to_json(example_translations(example_file, type = example_type))

  items_engl = readLines(source_file)

  if (is.null(batch_size)) {
    ITEMS = paste0('"',paste(items_engl, collapse = '",\n "'),'"')

    prompt = glue::glue(bp)
    chat = ellmer::chat_google_gemini(params = list(temperature = .05))
    #chatb = ellmer::chat_openai(params = list(temperature = .05))
    translated_items = chat$chat(prompt)

    out =
      data.table::data.table(
        jsonlite::fromJSON(gsub("```json|```","",translated_items ))
      )
  } else {

    num_batches <- ceiling(length(items_engl) / batch_size)
    out <- data.table::data.table()
    for (batch_idx in 1:num_batches) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx <- min(batch_idx * batch_size, length(items_engl))

      batch_items <- items_engl[start_idx:end_idx]
      ITEMS <- paste0('"', paste(batch_items, collapse = '",\n "'), '"')

      prompt <- glue::glue(bp)

      if (is.null(chat)) chat <- .get_chat()

      translated_items <- chat$chat(prompt)

      batch_results <-
        data.table::data.table(
          jsonlite::fromJSON(gsub("```json|```", "", translated_items))
        )
      out <- rbind(out, batch_results)

      # Optional: Add a small delay between batches to avoid rate limiting
      Sys.sleep(0.5)

      # Optional: Print progress
      cat("Processed batch", batch_idx, "of", num_batches, "\n")
    }
  }
  return(out)
}

#' Auto-detect a language model chat backend
#'
#' Returns an appropriate `chat` function from the `ellmer` package based on available environment variables.
#' Priority: Google Gemini > OpenAI > Anthropic.
#'
#' @return A chat function suitable for use with `llm_translate()`.
#' @keywords internal
.get_chat = function() {
  if (Sys.getenv("GOOGLE_API_KEY") != "") {
    chat <- ellmer::chat_google_gemini(params = list(temperature = .05))
  } else if (Sys.getenv("OPENAI_API_KEY") != "") {
    chat <- ellmer::chat_openai(params = list(temperature = .05))
  } else if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
    chat <- ellmer::chat_anthropic(params = list(temperature = .05))
  }

  return(chat)
}
