#' Get ellmer chat object
#'
#' @description
#' Initializes a chat object using the `ellmer` package, automatically detecting and
#' prioritizing available API keys for Google Gemini, OpenAI, or Anthropic.
#'
#' @param model Optional. A model identifier.
#' @param api_key Optional. An API key.
#' @param params Optional. A list of parameters for the LLM API. See \code{\link[ellmer]{params}}
#'
#' @returns
#' A chat object from the `ellmer` package. Will error if no API key environment
#' variable (GOOGLE_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY) is found.
#' @seealso
#' \code{\link[ellmer]{chat_google_gemini}}, \code{\link[ellmer]{chat_openai}}, \code{\link[ellmer]{chat_anthropic}}
#'
.get_chat = function(model = NULL, api_key = NULL, params = list(temperature = .05, max_tokens = 10000)) {
  if (Sys.getenv("GOOGLE_API_KEY") != "") {
    chat <- ellmer::chat_google_gemini(params = params, model = model, api_key = api_key)
  } else if (Sys.getenv("OPENAI_API_KEY") != "") {
    chat <- ellmer::chat_openai(params = params, model = model, api_key = api_key)
  } else if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
    chat <- ellmer::chat_anthropic(params = params, model = model, api_key = api_key)
  }

  return(chat)
}

#' @title Submit prepared prompt to LLM and parse response
#'
#' @description Submits a prompt to the LLM via `chat` object and parses its JSON response.
#'
#' @param prompt The LLM prompt string.
#' @param chat An `ellmer` chat object.
#' @param timeout An integer, seconds before curls returns a time out error
#'
#' @return A `data.table` containing `original_item` and `translated_item` pairs
#'   extracted from the LLM's JSON response.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom data.table data.table
#' @noRd
.llm_response = function(prompt, chat, timout = 120) {
  options(ellmer_timeout_s = timout)
  # Submit the prompt to the LLM
  llm_response_raw = chat$chat(prompt)

  if (llm_response_raw == "") {
    warning("Trying different LLM.")
    chat = .get_chat(model = "gemini-1.5-pro-latest")
    llm_response_raw = chat$chat(prompt)
  }

  #quick fix
  llm_response_raw = gsub(" translated_item", "translated_item", llm_response_raw)

  # Parse the JSON response
  # Remove potential markdown code fences around JSON
  parsed_response <- tryCatch({
    jsonlite::fromJSON(gsub("```json|```", "", llm_response_raw))
  }, error = function(e) {
    message("Error parsing JSON: ", e$message)
    return(NULL) # Or some other indicator of failure, like an empty list or NA
  })

  if (!is.null(parsed_response)) {
    out = data.table::data.table(parsed_response)
  } else {
    stopt("Could not parsed LLM produced JSON.")
  }
  return(out)
}
