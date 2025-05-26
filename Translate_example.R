api_key = NULL
m = NULL


library(data.table)
library(readxl)
source_items = data.table(read_xlsx(here::here("zdata/Translation data MENTOR.xlsx")))[grepl("ACE",Instrument)]

items = prep_TranslationItems(
   data = source_items,
   examples = here::here("zdata/Sample surveys translation.xlsx"),
   source_language = "English",
   target_language = "Norwegian",
   domain = "Youth mental health",
   batch_vars =  c("Instrument","Topic")
)


translated_items =
  translate_survey(items, sleep = 1, llm_model = m, api_key = api_key)


data_back_translation =
  translated_items[, .(Instrument,Topic,translated_item,Type)] %>%
  setnames("translated_item","Text")


items_back_translation = prep_TranslationItems(
  data = data_back_translation,
  examples = here::here("zdata/Sample surveys translation.xlsx"),
  reverse_transl = TRUE,
  source_language = "Norwegian",
  target_language = "English",
  domain = "Youth mental health",
  batch_vars =  c("Instrument","Topic")
)


back_translated_items =
  translate_survey(items, sleep = 1, llm_model = m, api_key = api_key)


evaluation =
  eval_translations(translated_items, back_translated_items)

evaluation[evaluation != "OK"]
