api_key = "AIzaSyBuh-UYHocYhpqF06ks5LBWqu-eAAzZRpk"
m = "gemini-2.0-flash-lite"
m = NULL

library(data.table)
library(readxl)
source_items = data.table(read_xlsx(here::here("zdata/Translation data MENTOR.xlsx")))[Instrument == "BCFPI"]

translated_items =
  translate_survey(source_items,
                example_file = here::here("zdata/Sample surveys translation.xlsx"),
                batch_vars = c("Instrument","Topic"),
                sleep = 1, llm_model = m, api_key = NULL)



source_back_translation =
  translated_items[, .(Instrument,Topic,translated_item,Type)] %>%
  setnames("translated_item","Text")


back_translated_items =
  translate_survey(source_back_translation,
                 example_file = here::here("zdata/Sample surveys translation.xlsx"),
                 rev_ex = TRUE,
                 batch_vars = c("Instrument","Topic"),
                 get_instr = FALSE,
                 source_language = "Norwegian",
                 target_language = "English",
                 sleep = 1, llm_model = m, api_key = NULL)

round_trip = data.table(
  original = translated_items$Text,
  translation = translated_items$translated_item,
  back_translation = back_translated_items$translated_item
)


round_trip_json = toJSON(round_trip, pretty = TRUE)

p2 = prompt_back_trans(round_trip_json)

eval = chat$chat(p2)
parsed_response = jsonlite::fromJSON(gsub("```json|```", "", eval))
