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
