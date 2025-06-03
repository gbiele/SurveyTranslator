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
   task = "Translate essential App text (e.g., instructions, terms, UI, error messages) for a mindfulness self-help application. Maintain precision for technical and legal text",
   role = "You are a highly experienced and meticulous translator specializing in digital mental health interventions and technical application content",
   guidelines = "Ensure all translations are accurate and retain original meaning, especially for technical or legal phrases.",
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

data_back_translation[grep('"', Text), Text := gsub('"','',Text)]
data_back_translation[grep('“|”', Text), Text := gsub('“|”','',Text)]

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
  translate_survey(items_back_translation, sleep = 1, llm_model = m, api_key = api_key, tmp_path = "tmp_back_no_en.Rdata", restart = FALSE)

saveRDS(back_translated_items,file = "happapp_back_no_en.RDS")

evaluation =
  eval_translations(translated_items, back_translated_items, tmp_path = "tmp_eval.Rdata")

saveRDS(evaluation,file = "happapp_evaluation.RDS")


evaluation[evaluation != "OK"]


ft <- flextable(evaluation) |>
  width(j = "original",         width = 3) |>
  width(j = "translation",      width = 3) |>
  width(j = "back_translation", width = 3) |>
  width(j = "evaluation",       width = 1.5) |>
  set_table_properties(layout = "fixed", width = 1)

default_sect_properties <- prop_section(
  page_size = page_size(orient = "landscape"), type = "continuous",
  page_margins = page_mar(bottom = .75, top = .75, right = .75, left = .75)
)

doc <- read_docx() |>
  body_set_default_section(default_sect_properties) |>
  body_add_flextable(ft)
print(doc, target = here("translated_items.docx"))
