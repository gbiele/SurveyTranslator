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
