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
base_prompt = function(source_language = NULL, target_language = NULL, guidelines = NULL,
                       domain = NULL, topic = NULL, instructions = NULL, responses = NULL)  {


  if (is.null(guidelines)){
    guidelines = default_guidelines()
  } else {
    guidelines = paste0(default_guidelines,"\n",guidelines)
  }

  out = paste0(
'
{{
  "task": "Translate a questionnaire",
  "instructions": {{
    "Role": "You are an expert translator specializing in questionnaires and surveys for mental health and related fields.",
    "Source language": "', source_language, '",
    "Target language": "', target_language, '",
    "Tone": "neutral and clear",
    "Domain": "', domain, '",
    ',ifelse(!is.null(topic),paste0("Item-topic: ", topic,","),""),'
    ',ifelse(!is.null(instructions),paste0("Instructions: ", instructions,","),""),'
    ',ifelse(!is.null(responses),paste0("Response options: ", responses,","),""),'
    "Guidelines": [',guidelines ,'],
    "Output format instruction": "Your output should be a list of objects,
    where each object contains two keys: \'original_item\' holding the original text,
    and \'translated_item\' holding the translation. Mirror the structure shown in the \'examples\'."
  }},
  "examples": {EXAMPLES},
  "Items to translate": [
    {ITEMS}
  ]
}}
')
  return(out)
}



#' Prompt evaluation
#'
#' @description
#' A short description...
#'
#' @param JSON_DATA A string containing JSON data.
#' @param guidelines Optional. A string containing guidelines.
#'
#' @returns
#' A string representing a JSON structure, likely for a language model prompt.
#'
#' @export
prompt_evaluation = function(JSON_DATA, guidelines = default_backtrans_guidelines()) {
  prompt =
  paste('
{
  "contents": [
    {
      "role": "user",
      "parts": [
        {
          "text":
          ', guidelines,'

          You will receive a JSON array, where each object contains an \'original\' statement, its \'translation\', and its \'back_translation\'.

          For each item, perform the following steps:
            1.  **Compare:** Analyze the \'original\' statement against its \'back_translation\'
            2.  **Evaluate Meaning:** Determine if their core meanings are identical.
            3.  **Generate Output:** Create a new JSON array. Each object in this new
                array should include all original fields (\'original\', \'translation\', \'back_translation\')
                plus a new field titled \'evaluation\'.
                The \'evaluation\' field must follow these rules:
                  * Set to \'OK\' if the meanings of the \'original\' and \'back_translation\' are identical.
                  * Set to \'deviation:[justification]\' if the meanings are not identical.
                  The \'[justification]\' must be a concise and clear explanation of the semantic discrepancy.
                Ensure the final output is a valid JSON array of objects.

                Here is the JSON data you need to process, which will follow this introductory text:
                ',JSON_DATA,'"
        }
      ]
    }
  ]
}
')
  return(prompt)
}


#' Default guidelines for translation.
#'
#' @description
#' Translation guidelines that are added to the LLM prompt
#'
#' @returns
#' A character string containing default translation guidelines.
#'
#' @export
default_guidelines = function() {
  guidelines =
    '
      "Ensure each item is translated accurately and retains its original meaning.",
      "When an item topic, instructions, or response options are given, translate these first."
      "Maintain consistency in terminology and phrasing across all items.",
      "Translate questions as questions, and statements as statements.",
      "If an item contains a placeholder like \'[Variable]\', keep the placeholder as is."
'
  return(guidelines)
}


#' Default guidelines for translation.
#'
#' @description
#' Evaluation guidelines that are added to the LLM prompt
#'
#' @returns
#' A character string containing default evaluation guidelines.
#'
#' @export
default_backtrans_guidelines = function() {
  guidelines =
'
      "You are an expert linguistic evaluator specializing in survey translation quality."
       "Your task is to evaluate the semantic and tonal equivalence between original survey statements and their back-translated versions."
       "Be relatively strict in your evaluation, nuances can matter."
'
  return(guidelines)
}
