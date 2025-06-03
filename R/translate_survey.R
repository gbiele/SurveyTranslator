#' @title Translate survey items using a TranslationItems object
#' @description Translates items using either batch size or batch vars logic with LLM.
#' @param items_obj A `TranslationItems` object.
#' @param chat Optional. An `ellmer` chat object.
#' @param llm_model Optional. LLM model name.
#' @param api_key Optional. API key.
#' @param sleep Seconds to pause between batches.
#' @param tmp_path Path to a temporary file for saving progress. Defaults to "tmp.RDS".
#' @param restart Logical. If TRUE, the function attempts to restart from a previously saved temporary file. Defaults to FALSE.
#' @return A `data.table` of translated items.
#' @export
translate_survey <- function(items_obj, chat = NULL, llm_model = NULL, api_key = NULL, sleep = 7, tmp_path = "tmp.RDS", restart = FALSE) {
  stopifnot(inherits(items_obj, "TranslationItems"))

  if (!is.null(items_obj$batch_size) && !is.null(items_obj$batch_vars)) {
    stop("Specify only one of batch_size or batch_vars.")
  }

  if (is.null(chat)) chat <- .get_chat(model = llm_model, api_key = api_key)

  bp <- base_prompt(
    source_language = items_obj$source_language,
    target_language = items_obj$target_language,
    example_translation = items_obj$example_translations,
    example_target_text  = items_obj$example_target_text,
    domain = items_obj$domain,
    guidelines = items_obj$guidelines
  )

  if (is.null(items_obj$batch_size) && is.null(items_obj$batch_vars)) {
    ITEMS <- paste0('"', paste(items_obj$data$Text, collapse = '",\n "'), '"')
    return(.translate_llm(prompt, chat))
  }

  if (!is.null(items_obj$batch_size)) {
    return(.batch_by_size(items_obj, bp, chat, sleep, tmp_path, restart))
  }

  .batch_by_vars(items_obj, bp, topic_var, EXAMPLES, chat, sleep, tmp_path, restart)
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

  #quick fix
  translated_items_raw = gsub(" translated_item", "translated_item", translated_items_raw)

  # Parse the JSON response
  # Remove potential markdown code fences around JSON
  parsed_response = jsonlite::fromJSON(gsub("```json|```", "", translated_items_raw))

  out = data.table::data.table(parsed_response)
  return(out)
}


#' Batch process items by size
#'
#' @description
#' Processes survey items in batches based on a specified batch size, communicating with an LLM for translation.
#'
#' @param items_obj An object containing data to process and the batch size. Must have a `data` element with a `Text` column and a `batch_size` element.
#' @param bp A string template for the prompt.
#' @param chat A chat object or argument passed to `.translate_llm`.
#' @param sleep A single number representing the time to sleep between batches.
#' @param tmp_path Path to a temporary file for saving progress.
#' @param restart Logical. If TRUE, the function attempts to restart from a previously saved temporary file.
#'
#' @returns
#' A `data.table` containing the results of processing items in batches.
.batch_by_size <- function(items_obj, bp, chat, sleep, tmp_path, restart) {
  items_to_translate <- items_obj$data$Text
  batch_size <- items_obj$batch_size
  num_batches <- ceiling(length(items_to_translate) / batch_size)
  out <- data.table::data.table()

  first_idx = 1
  if (restart == TRUE) {
    load(tmp_path)
    first_idx = batch_idx
  }

  for (batch_idx in first_idx:num_batches) {
    start_idx <- (batch_idx - 1) * batch_size + 1
    end_idx <- min(batch_idx * batch_size, length(items_to_translate))
    batch_items <- items_to_translate[start_idx:end_idx]
    ITEMS <- paste0('"', paste(batch_items, collapse = '",\n "'), '"')
    current_prompt <- gsub("\\{ITEMS\\}",ITEMS,bp)
    batch_results <- .translate_llm(prompt = current_prompt, chat = chat)
    out <- rbind(out, batch_results)
    if (num_batches > 1 && batch_idx < num_batches) Sys.sleep(sleep)
    cat("Processed batch", batch_idx, "of", num_batches, "\n")
    save(out,batch_idx,file = tmp_path)
  }
  return(out)
}

#' Batch process items by variables
#'
#' @description
#' Processes survey items in batches based on specified grouping variables, communicating with an LLM for translation.
#'
#' @param items_obj An object containing data and batching information (e.g., `batch_vars`, `topic_var`, `get_instr`, source/target languages, examples, guidelines, domain).
#' @param bp A base prompt string.
#' @param topic_var The variable in `items_obj$data` that represents the topic for dynamic prompt generation.
#' @param example_translation The example translation text to be included in the prompt.
#' @param chat Chat context for the LLM.
#' @param sleep A numeric value for sleeping between batches.
#' @param tmp_path Path to a temporary file for saving progress.
#' @param restart Logical. If TRUE, the function attempts to restart from a previously saved temporary file.
#'
#' @returns
#' A `data.table` containing the results for each batch, including the original item details.
#'
#' @export
.batch_by_vars <- function(items_obj, bp, topic_var, example_translation, chat, sleep, tmp_path, restart) {
  items_to_translate <- items_obj$data
  batch_vars <- items_obj$batch_vars
  topic_var <- items_obj$topic_var
  get_instr <- items_obj$get_instr
  source_language <- items_obj$source_language
  target_language <- items_obj$target_language
  example_translation <- items_obj$example_translations
  example_target_text <- items_obj$example_target_text
  guidelines <- items_obj$guidelines
  domain <- items_obj$domain

  batches <- items_to_translate[Type == "Item", .(N = .N), by = batch_vars]
  out <- data.table::data.table()
  num_batches <- nrow(batches)

  first_idx = 1
  if (restart == TRUE) {
    load(tmp_path)
    first_idx = batch_idx
  }

  for (batch_idx in first_idx:num_batches) {
    topic = batches[batch_idx, get(topic_var)]
    bp <- base_prompt(
      source_language = source_language,
      target_language = target_language,
      example_translation = example_translation,
      example_target_text = example_target_text,
      topic = topic,
      domain = domain,
      guidelines = guidelines
    )

    if (get_instr) {
      batch_items_to_translate <- merge(batches[batch_idx], items_to_translate, by = batch_vars)[, .(Instrument, Topic, Type, Text, Instruction, Response)]
      Instructions <- items_to_translate[Instruction == batch_items_to_translate[1, Instruction] & Type == "Instr", Text]
      Response_opts <- items_to_translate[Response %in% na.omit(unique(batch_items_to_translate[, Response])) & Type == "RT", Text]
      if (length(Response_opts) > 0 & !any(batch_items_to_translate$Type == "RT"))
        batch_items_to_translate <- rbind(batch_items_to_translate[1:length(Response_opts)][, `:=`(Text = Response_opts, Type = "RT")], batch_items_to_translate)
      if (length(Instructions) > 0 & !any(batch_items_to_translate$Type == "Instr"))
        batch_items_to_translate <- rbind(batch_items_to_translate[1:length(Instructions)][, `:=`(Text = Instructions, Type = "Instr")], batch_items_to_translate)
    } else {
      batch_items_to_translate <- merge(batches[batch_idx], items_to_translate, by = batch_vars)[, .(Instrument, Topic, Type, Text)]
    }
    chat <- .get_chat()
    ITEMS <- paste0('"', paste(batch_items_to_translate$Text, collapse = '",\n "'), '"')
    current_prompt <- gsub("\\{ITEMS\\}",ITEMS,bp)
    batch_results <- .translate_llm(prompt = current_prompt, chat = chat)
    if (num_batches > 1 && batch_idx < num_batches) Sys.sleep(sleep)
    batch_results <- cbind(batch_results, batch_items_to_translate)
    batch_results[, batch_idx := batch_idx]
    out <- rbind(out, batch_results)
    cat("Processed batch", batch_idx, "of", num_batches, "\n")
    save(out,batch_idx,file = tmp_path)
  }
  return(out)
}
