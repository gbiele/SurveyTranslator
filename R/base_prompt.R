
#' Base prompt for translation task
#'
#' @description
#' A short description...
#'
#' @param source_language Optional. A single string specifying the source language.
#' @param target_language Optional. A single string specifying the target language.
#' @param domain Optional. A single string specifying the domain of the content.
#'
#' @returns
#' A single string formatted as a JSON structure for the translation task prompt.
#'
#' @export
base_prompt = function(source_language = "English", target_language = "Norwegian", domain = "Youth mental health")  {
  out = paste0(
'
{{
  "task": "Translate a questionnaire",
  "instructions": {{
    "role": "You are an expert translator specializing in questionnaires and surveys.",
    "source_language": "', source_language, '",
    "target_language": "', target_language, '",
    "tone": "neutral and clear",
    "domain": "', domain, '",
    "guidelines": [
      "Ensure each item is translated accurately and retains its original meaning.",
      "Maintain consistency in terminology and phrasing across all items.",
      "Translate questions as questions, and statements as statements.",
      "If an item contains a placeholder like \'[Variable]\', keep the placeholder as is."
    ],
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
