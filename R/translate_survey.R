#' @title Translate survey items using a language model
#'
#' @description Translates survey items (from file or `data.table`) to a target language.
#'   Manages prompts, example translations, and batching. Leverages `.translate_llm()`
#'   for LLM interaction and `.get_chat()` for LLM client auto-detection.
#'
#' @param source_items Path to file or `data.table` of survey items for translation.
#' @param example_file Path to Excel file with example translations.
#' @param rev_ex Logical. If `TRUE`, swaps source/target languages for examples.
#' @param source_language Source language. Defaults to 'English'.
#' @param target_language Target language. Defaults to 'Norwegian'.
#' @param domain Domain context for translation tone/terminology. Defaults to 'Youth mental health'.
#' @param guidelines Optional custom translation guidelines (character vector).
#' @param example_type Type of examples ('Item', 'Instructions', 'Response options'). Defaults to 'Item'.
#' @param chat `ellmer` chat object. If `NULL`, auto-detected by `.get_chat()`.
#' @param batch_size Integer. Batch size for translation. `NULL` for single request.
#' @param get_instr Logical. If `TRUE`, incorporates instructions/response options into batches.
#' @param batch_vars Character vector. Variables to group items for batching via `.batch_by_vars()`.
#' @param api_key Your LLM API key. Passed to `.get_chat()` if `chat` is `NULL`.
#' @param sleep Numeric. Seconds to pause between batches.
#' @param llm_model LLM model name. Passed to `.get_chat()` if `chat` is `NULL`.
#'
#' @return A `data.table` containing the translated survey items.
#'
#' @details Uses `base_prompt()` to construct LLM prompts, `example_translations()` for examples,
#'   and `dt_to_json()` for serialization. Batching is handled by `.batch_by_size()` or `.batch_by_vars()`.
#'   Relies on `.get_chat()` for LLM client setup. Expected LLM output is JSON with
#'   `original_item` and `translated_item`.
#'
#' @examples
#' \dontrun{
#' # Example: Translate all items at once
#' # translated_data_single <- translate_survey("data/items.txt", "data/examples.xlsx")
#'
#' # Example: Translate items in batches
#' # translated_data_batch <- translate_survey("data/items.txt", "data/examples.xlsx",
#' #                                          batch_size = 10, target_language = "Spanish")
#' }
#'
#' @importFrom data.table data.table
#' @importFrom jsonlite toJSON
#' @export
translate_survey = function(source_items, example_file,rev_ex = FALSE,
                            source_language = "English", target_language = "Norwegian",
                            domain = "Youth mental health", guidelines = NULL,
                            example_type = "Item", chat = NULL, batch_size = NULL,
                            get_instr = TRUE, batch_vars = c("Instrument","Topic"),
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
    toJSON(
      example_translations(example_file, rev_ex = rev_ex),
      pretty = TRUE)

  # Read survey items from the source file
  if (is.character(source_items)) {
    items_to_translate = data.table(read_xlsx(source_items))
  } else {
    items_to_translate = source_items
  }


  if (is.null(chat)) chat = .get_chat(model = llm_model, api_key = api_key)


  if (is.null(batch_size) & is.null(batch_vars)) {
    # Process all items in a single batch
    ITEMS = paste0('"', paste(items_to_translate[,Text], collapse = '",\n "'), '"')
    current_prompt = glue::glue(bp) # EXAMPLES and ITEMS are used by glue via bp

    out = .translate_llm(prompt = current_prompt, chat = chat)

  } else if (!is.null(batch_size)) {
    out = .batch_by_size(bp,items_to_translate, batch_size, chat, sleep)
  } else if (!is.null(batch_vars)) {
    out = .batch_by_vars(bp, items_to_translate, batch_vars, get_instr, source_language, target_language, guidelines, domain, EXAMPLES, chat, sleep)
  }
  return(out)
}

#' @title Submit prepared prompt to LLM and parse response
#'
#' @description Submits a prompt to the LLM via `chat` object and parses its JSON response.
#'
#' @param prompt The LLM prompt string.
#' @param chat An `ellmer` chat object.
#'
#' @return A `data.table` containing `original_item` and `translated_item` pairs
#'   extracted from the LLM's JSON response.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
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

#' @title Auto-detect language model chat backend
#'
#' @description Auto-detects and returns an `ellmer` chat object based on available
#'   API keys (`GOOGLE_API_KEY`, `OPENAI_API_KEY`, `ANTHROPIC_API_KEY`), prioritizing Google Gemini.
#'
#' @param model LLM model name. Optional.
#' @param api_key Your LLM API key. Optional.
#'
#' @return An `ellmer` chat object.
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



#' @title Batch by size
#'
#' @description Processes items in batches of fixed size, sending each batch to the LLM via `.translate_llm()`.
#'
#' @param bp Base prompt template.
#' @param items_to_translate Data with `Text` column for translation.
#' @param batch_size Number of items per batch.
#' @param chat An `ellmer` chat object.
#' @param sleep Seconds to pause between batches.
#'
#' @return A `data.table` of translation results.
#' @importFrom data.table data.table
#' @keywords internal pause between batches.
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

#' @title Batch items by variables
#'
#' @description Groups and processes items by specified variables, sending each batch to the LLM via `.translate_llm()`.
#'
#' @param bp Base prompt template.
#' @param items_to_translate `data.table` with items, `Text` and `batch_vars` columns.
#' @param batch_vars Variables for grouping items into batches.
#' @param get_instr Logical. If `TRUE`, incorporates instructions/response options.
#' @param source_language Source language.
#' @param target_language Target language.
#' @param guidelines Optional custom translation guidelines.
#' @param domain Domain context.
#' @param EXAMPLES JSON string of example translations.
#' @param chat An `ellmer` chat object.
#' @param sleep Seconds to pause between batches.
#'
#' @return A `data.table` of translation results with original items and batch index.
#' @importFrom data.table data.table
#' @keywords internal
.batch_by_vars = function(bp, items_to_translate, batch_vars, get_instr, source_language, target_language, guidelines, domain, EXAMPLES, chat, sleep) {
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

    if (get_instr == TRUE) {
      batch_items_to_translate =
        merge(batches[batch_idx], items_to_translate,
              by = batch_vars)[,.(Instrument,Topic,Type,Text, Instruction, Response)]

      Instructions = items_to_translate[Instruction == batch_items_to_translate[1,Instruction] & Type == "Instr", Text]
      Response_opts = items_to_translate[Response == batch_items_to_translate[1,Response] & Type == "RT", Text]

      if (length(Response_opts) > 0)
        batch_items_to_translate = rbind(batch_items_to_translate[1][, `:=`(Text = Response_opts, Type = "RT")],batch_items_to_translate)
      if (length(Instructions) > 0)
        batch_items_to_translate = rbind(batch_items_to_translate[1][, `:=`(Text = Instructions, Type = "Instr")],batch_items_to_translate)
    } else {
      batch_items_to_translate =
        merge(batches[batch_idx], items_to_translate,
              by = batch_vars)[,.(Instrument,Topic,Type,Text)]
    }

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
