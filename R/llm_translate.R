
#' Translate text using an LLM
#'
#' @description
#' A short description...
#'
#' @param source_file A single string specifying the path to the file containing the text to be translated.
#' @param example_file Optional. A single string specifying the path to an example translation file.
#' @param example_type Optional. A single string specifying the type of examples to use from the example file.
#' @param batch_size Optional. An integer specifying the number of items to translate in each batch, or `NULL` to translate all items at once.
#'
#' @returns
#' A data.table containing the translated text.
#'
#' @export
llm_translate = function(source_file, example_file = "Sample surveys translation.xlsx", example_type = "Item", batch_size = NULL) {

  bp = base_prompt()

  EXAMPLES =
    example_translations(example_file, type = example_type) %>%
    dt_to_json()

  items_engl = readLines(source_file)

  if (is.null(batch_size)) {
    ITEMS = paste0('"',paste(items_engl, collapse = '",\n "'),'"')

    prompt = glue::glue(bp)
    chat = ellmer::chat_google_gemini(params = list(temperature = .05))
    #chatb = ellmer::chat_openai(params = list(temperature = .05))
    translated_items = chat$chat(prompt)

    out =
      jsonlite::fromJSON(gsub("```json|```","",translated_items )) %>%
      data.table::data.table()
  } else {

    num_batches <- ceiling(length(items_engl) / batch_size)
    out <- data.table::data.table()
    for (batch_idx in 1:num_batches) {
      start_idx <- (batch_idx - 1) * batch_size + 1
      end_idx <- min(batch_idx * batch_size, length(items_engl))

      batch_items <- items_engl[start_idx:end_idx]
      ITEMS <- paste0('"', paste(batch_items, collapse = '",\n "'), '"')

      prompt <- glue::glue(bp)
      chat <- ellmer::chat_google_gemini(params = list(temperature = .05))
      translated_items <- chat$chat(prompt)

      batch_results <- jsonlite::fromJSON(gsub("```json|```", "", translated_items)) %>%
        data.table::data.table()
      out <- rbind(out, batch_results)

      # Optional: Add a small delay between batches to avoid rate limiting
      Sys.sleep(0.5)

      # Optional: Print progress
      cat("Processed batch", batch_idx, "of", num_batches, "\n")
    }
  }

  return(out)

}
