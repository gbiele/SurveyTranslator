api_key = "AIzaSyBuh-UYHocYhpqF06ks5LBWqu-eAAzZRpk"
m = "gemini-2.0-flash-lite"

api_key = NULL
m = NULL

library(SurveyTranslator)
library(data.table)
library(readxl)
library(magrittr)
library(jsonlite)
source_items = data.table(read_xlsx(here::here("zdata/Translation data_happ_app_MENTOR.xlsx")))
#source_items = source_items[grepl("Terms",Instrument)]
source_items = source_items[!grepl("Don|don",Comments_from_Helga)]
source_items = source_items[, .(Text,Instrument, Topic, Type)] %>% unique()
source("zdata/mindfullness_excersises.R")

items = prep_TranslationItems(
   data = source_items,
   example_txt = ex_txt_json,
   source_language = "English",
   target_language = "Norwegian",
   domain = "Happ App, a self help Mindfulness App",
   batch_vars =  c("Instrument","Topic"),
   topic_var = "Topic",
   get_instr = FALSE
)


translated_items =
  translate_survey(items, sleep = 1, llm_model = m, api_key = api_key, tmp_path = "tmp_en_no.RDS")

saveRDS(translated_items,file = "happapp_en_no.RDS")

data_back_translation =
  translated_items[, .(Instrument,Topic,translated_item,Type)] %>%
  setnames("translated_item","Text")

items_back_translation = prep_TranslationItems(
  data = data_back_translation,
  reverse_transl = TRUE,
  source_language = "Norwegian",
  target_language = "English",
  domain = "Happ App, a self help Mindfulness App",
  batch_vars =  c("Instrument","Topic"),
  topic_var = "Topic",
  get_instr = FALSE
)


back_translated_items =
  translate_survey(items_back_translation, sleep = 1, llm_model = m, api_key = api_key, tmp_path = "tmp_back_no_en.Rdata")

saveRDS(back_translated_items,file = "happapp_back_no_en.RDS")

evaluation =
  eval_translations(translated_items, back_translated_items)

saveRDS(evaluation,file = "happapp_evaluation.RDS")


evaluation[evaluation != "OK"]
