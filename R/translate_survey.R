#' Translate survey items using a language model
#'
#' Prepares prompts and manages the translation of survey items from a plain
#' text file, either in a single request or in batches. Uses `.translate_llm()`
#' for actual LLM interaction. Translation is guided by example translations
#' extracted from an Excel file and shaped by a customizable prompt.Supports
#' batching for large item sets and automatic model selection if no chat object
#' is provided.
#'
#' @param source_file Character. Path to a text file containing survey items
#' (one item per line) to be translated.
#' @param example_file Character. Path to an Excel file containing example translations.
#' @param source_language Character. Source language used in the survey items. Default is `"English"`.
#' @param target_language Character. Target language for the translations. Default is `"Norwegian"`.
#' @param domain Character. Domain context for the prompt. Helps tailor the tone
#' and terminology. Default is `"Youth mental health"`.
#' @param guidelines Character string or vector. Optional custom translation
#' guidelines. If `NULL`, a default set of instructions is used.
#' @param example_type Character. Type of examples to extract from the Excel file.
#' Passed to `example_translations()`. Default is `"Item"`, can also be
#' `"Instructions"` or `"Response options"`.
#' @param chat Optional. An `ellmer` LLM chat object (e.g., from `ellmer::chat_google_gemini()`).
#' If `NULL`, an appropriate chat object will be auto-detected via `.get_chat()`
#' (e.g. `ellmer::chat_google_gemini(params = list(temperature = .05))` if
#' `GOOGLE_API_KEY` is found).
#' @param batch_size Optional integer. If provided, items are translated in
#' batches of this size. If `NULL`, all items are translated in a single request.
#'
#' @return A `data.table` containing the translated survey items.
#'
#' @details
#' The function constructs a structured prompt using `base_prompt()`, filling in
#' language, domain, and guideline parameters, #' along with example translations
#' (via `example_translations()`) and target items.
#'
#' If no `chat` object is supplied, the function auto-detects the available backend by checking for API keys in `.Renviron`
#' in the following order (this logic should be implemented in a helper like `.get_chat()`):
#' 1. Google Gemini (`GOOGLE_API_KEY`)
#' 2. OpenAI (`OPENAI_API_KEY`)
#' 3. Anthropic (`ANTHROPIC_API_KEY`)
#'
#' At least one of these keys must be set in your environment for auto-detection.
#'
#' Example translations are serialized to JSON using `dt_to_json()` and included in the prompt under the `examples` field.
#' The expected model output (handled by `.translate_llm`) is a JSON list of objects, each containing `original_item` and `translated_item`.
#'
#' @examples
#' \dontrun{
#' # Assuming base_prompt, example_translations, dt_to_json, and .get_chat are defined
#' # and API keys are set up.
#'
#' # Example: Translate all items at once
#' # translated_data_single <- translate_survey("data/items.txt", "data/examples.xlsx")
#'
#' # Example: Translate items in batches
#' # translated_data_batch <- translate_survey("data/items.txt", "data/examples.xlsx",
#' #                                          batch_size = 10, target_language = "Spanish")
#' }
#'
#' @export
translate_survey = function(source_file, example_file,
                            source_language = "English", target_language = "Norwegian",
                            domain = "Youth mental health", guidelines = NULL,
                            example_type = "Item", chat = NULL, batch_size = NULL,
                            batch_vars = c("Instrument","Topic"),
                            api_key = NULL, sleep = 7, llm_model = NULL) {

  if (all(!c(is.null(batch_vars),is.null(batch_size))) == TRUE)
    stop("You can only specify batch_vars or batch_size")

  # Prepare the base prompt structure
  bp = base_prompt(
    source_language = source_language,
    target_language = target_language,
    domain = domain,
    guidelines = guidelines
  )

  # Load and format example translations
  EXAMPLES =
    dt_to_json(example_translations(example_file, type = example_type))

  # Read survey items from the source file
  items_to_translate = data.table(read_xlsx(source_file))


  if (is.null(chat)) chat = .get_chat(model = llm_model, api_key = api_key)


  if (is.null(batch_size) & is.null(batch_vars)) {
    # Process all items in a single batch
    ITEMS = paste0('"', paste(items_to_translate[,Text], collapse = '",\n "'), '"')
    current_prompt = glue::glue(bp) # EXAMPLES and ITEMS are used by glue via bp

    out = .translate_llm(prompt = current_prompt, chat = chat)

  } else if (!is.null(batch_size)) {
    out = .batch_by_size(bp,items_to_translate, batch_size, chat, sleep)
  } else if (!is.null(batch_vars)) {
    out = .batch_by_vars(bp,items_to_translate,batch_vars, source_language, target_language, guidelines, domain, EXAMPLES, chat, sleep)
  }
  return(out)
}

#' Submit a prepared prompt to the LLM and parse the response
#'
#' @param prompt Character. The complete prompt string to send to the LLM.
#' @param chat An `ellmer` LLM chat object (e.g., from `ellmer::chat_google_gemini()`).
#'
#' @return A `data.table` containing `original_item` and `translated_item` pairs
#'         extracted from the LLM's JSON response.
#'
#' @noRd
.translate_llm = function(prompt, chat) {
  # Submit the prompt to the LLM
  translated_items_raw = chat$chat(prompt)

  # Parse the JSON response
  # Remove potential markdown code fences around JSON
  parsed_response = jsonlite::fromJSON(gsub("```json|```", "", translated_items_raw))

  out = data.table::data.table(parsed_response)
  return(out)
}

#' Auto-detect a language model chat backend
#'
#' Returns an appropriate `chat` function from the `ellmer` package based on available environment variables.
#' Priority: Google Gemini > OpenAI > Anthropic.
#'
#' @return An `ellmer` chat object suitable for use with `llm_translate()`.
#' @keywords internal
.get_chat = function(model = NULL, api_key = NULL) {
  if (Sys.getenv("GOOGLE_API_KEY") != "") {
    chat <- ellmer::chat_google_gemini(params = list(temperature = .05), model = model, api_key = api_key)
  } else if (Sys.getenv("OPENAI_API_KEY") != "") {
    chat <- ellmer::chat_openai(params = list(temperature = .05))
  } else if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
    chat <- ellmer::chat_anthropic(params = list(temperature = .05))
  }

  return(chat)
}



#' Batch by size
#'
#' @description
#' A short description...
#'
#' @param bp A string prompt template.
#' @param items_to_translate A data.table or similar with a column `Text`.
#' @param batch_size An integer to determine the number of items per batch
#' @param An `ellmer` chat object
#' @param sleep A numeric value specifying the number of seconds to pause between batches.
#'
#' @returns
#' A data.table containing results from batch processing. The function prints
#' progress messages and may pause between batches.
#' @keywords internal
.batch_by_size = function(bp,items_to_translate,batch_size,chat,sleep) {
  items_to_translate = items_to_translate[,Text]
  # Process items in multiple batches
  num_batches <- ceiling(length(items_to_translate) / batch_size)
  out <- data.table::data.table() # Initialize an empty data.table for results

  for (batch_idx in 1:num_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(items_to_translate))

    items_to_translate <- items_to_translate[start_idx:end_idx]
    ITEMS <- paste0('"', paste(items_to_translate, collapse = '",\n "'), '"')

    current_prompt <- glue::glue(bp) # EXAMPLES and ITEMS are used by glue via bp

    batch_results <- .translate_llm(prompt = current_prompt, chat = chat)
    out <- rbind(out, batch_results)

    # Optional: Add a small delay between batches to avoid rate limiting
    if (num_batches > 1 && batch_idx < num_batches) {
      Sys.sleep(sleep) # Only sleep if there are more batches to come
    }

    # Optional: Print progress
    cat("Processed batch", batch_idx, "of", num_batches, "\n")
  }
  return(out)
}

#' Batch items by variables
#'
#' @description
#' A short description...
#'
#' @param bp A base prompt.
#' @param items_to_translate A data.table containing items to translate,
#' expected to have columns "Text", "Instrument", "Topic", "Instruction", and "Response",
#' and also columns specified by `batch_vars` used in the `by` argument of `data.table::data.table()`.
#' @param batch_vars A character vector with variable names that determine groups for
#' batch processing,
#' @param source_language Character. Source language used in the survey items. Default is `"English"`.
#' @param target_language Character. Target language for the translations. Default is `"Norwegian"`.
#' @param guidelines Character string or vector. Optional custom translation
#' @param domain Character. Domain context for the prompt. Helps tailor the tone
#' @param An `ellmer` chat object
#' @param sleep A numeric value specifying the number of seconds to pause between batches.
#'
#' @returns
#' A data.table containing the translation results (`batch_results` from `.translate_llm`),
#' combined with the original batch items and a `batch_idx` column.
#'
#' @keywords internal
.batch_by_vars = function(bp,items_to_translate,batch_vars, source_language, target_language, guidelines, domain, EXAMPLES, chat, sleep) {
  # expecting columns "Text" "Instrument","Topic","Instruction","Response" in items_to_translate
  batches =
    items_to_translate[
      Type == "Item", .(N = .N),
      by = batch_vars]
  out <- data.table::data.table() # Initialize an empty data.table for results

  num_batches = nrow(batches)

  for (batch_idx in 1:num_batches) {
    bp = base_prompt(
      source_language = source_language,
      target_language = target_language,
      topic = batches[batch_idx,Topic],
      domain = domain,
      guidelines = guidelines
    )
    batch_items_to_translate =
      merge(batches[batch_idx], items_to_translate,
            by = batch_vars)[,.(Instrument,Topic,Type,Text,Instruction,Response)]

    Instructions = items_to_translate[Instruction == batch_items_to_translate[1,Instruction] & Type == "Instr", Text]
    Response_opts = items_to_translate[Response == batch_items_to_translate[1,Response] & Type == "RT", Text]

    if (length(Response_opts) > 0)
      batch_items_to_translate = rbind(batch_items_to_translate[1][, `:=`(Text = Response_opts, Type = "RT")],batch_items_to_translate)
    if (length(Instructions) > 0)
      batch_items_to_translate = rbind(batch_items_to_translate[1][, `:=`(Text = Instructions, Type = "Instr")],batch_items_to_translate)


    ITEMS <- paste0('"', paste(batch_items_to_translate$Text, collapse = '",\n "'), '"')
    current_prompt <- glue::glue(bp) # EXAMPLES and ITEMS are used by glue via bp
    batch_results <- .translate_llm(prompt = current_prompt, chat = chat)
    # Optional: Add a small delay between batches to avoid rate limiting
    if (num_batches > 1 && batch_idx < num_batches) {
      Sys.sleep(sleep) # Only sleep if there are more batches to come
    }
    batch_results = cbind(batch_results,batch_items_to_translate)
    batch_results[, batch_idx := batch_idx]
    out <- rbind(out, batch_results)

    # Optional: Print progress
    cat("Processed batch", batch_idx, "of", num_batches, "\n")
  }
  return(out)
}
