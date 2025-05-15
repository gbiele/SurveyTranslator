#' Construct prompt for questionnaire translation
#'
#' Builds a JSON-formatted prompt string for use with a language model, including structured translation instructions,
#' language and domain context, and formatting expectations.
#'
#' @param source_language Character. Source language (e.g., `"English"`).
#' @param target_language Character. Target language (e.g., `"Norwegian"`).
#' @param domain Character. Subject domain of the questionnaire (e.g., `"Youth mental health"`). Helps tailor the style and terminology.
#' @param guidelines Character vector or string. Optional additional translation guidelines. If `NULL`, a default set of instructions is used.
#' If a custom value is provided, it is appended to the defaults.
#'
#' @return A character string containing a complete JSON prompt with placeholders `{EXAMPLES}` and `{ITEMS}` to be filled in later.
#'
#' @details
#' The returned prompt is designed to instruct the LLM to return a list of objects, each containing
#' `original_item` and `translated_item` keys. Guidelines include formatting rules and translation consistency requirements.
#' If custom `guidelines` are provided, they are appended after the default ones.
#'
#' @keywords internal
#' @export
base_prompt = function(source_language = NULL, target_language = NULL, domain = NULL, guidelines = NULL)  {

  default_guidelines =
'
      "Ensure each item is translated accurately and retains its original meaning.",
      "Maintain consistency in terminology and phrasing across all items.",
      "Translate questions as questions, and statements as statements.",
      "If an item contains a placeholder like \'[Variable]\', keep the placeholder as is."
'


  if (is.null(guidelines)){
    guidelines = default_guidelines
  } else {
    guidelines = paste0(default_guidelines,"\n",guidelines)
  }

  print("### Guideline for translation:")
  print(cat(guidelines))

  out = paste0(
'
{{
  "task": "Translate a questionnaire",
  "instructions": {{
    "role": "You are an expert translator specializing in questionnaires and surveys for mental health and related fields.",
    "source_language": "', source_language, '",
    "target_language": "', target_language, '",
    "tone": "neutral and clear",
    "domain": "', domain, '",
    "guidelines": [',guidelines ,'],
    "output_format_instruction": "Your output should be a list of objects,
    where each object contains two keys: \'original_item\' holding the English text,
    and \'translated_item\' holding the Norwegian translation. Mirror the structure shown in the \'examples\'."
  }},
  "examples": {EXAMPLES},
  "items_to_translate": [
    {ITEMS}
  ]
}}
')
  return(out)
}
