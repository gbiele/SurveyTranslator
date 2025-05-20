
#' Get chat object
#'
#' @description
#' A short description...
#'
#' @param model Optional. A model name.
#' @param api_key Optional. An API key.
#' @param temperature Optional. A single numeric value for temperature.
#'
#' @returns
#' A chat object from the `ellmer` package. Will error if no API key environment
#' variable (GOOGLE_API_KEY, OPENAI_API_KEY, or ANTHROPIC_API_KEY) is found.
.get_chat = function(model = NULL, api_key = NULL, temperature = 0.05) {
  if (Sys.getenv("GOOGLE_API_KEY") != "") {
    chat <- ellmer::chat_google_gemini(params = list(temperature = temperature), model = model, api_key = api_key)
  } else if (Sys.getenv("OPENAI_API_KEY") != "") {
    chat <- ellmer::chat_openai(params = list(temperature = temperature))
  } else if (Sys.getenv("ANTHROPIC_API_KEY") != "") {
    chat <- ellmer::chat_anthropic(params = list(temperature = temperature))
  }

  return(chat)
}
