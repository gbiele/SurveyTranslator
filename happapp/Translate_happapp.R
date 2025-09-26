api_key = NULL
m = NULL

library(SurveyTranslator)
library(data.table)
library(readxl)
library(magrittr)
library(jsonlite)
library(flextable)
library(officer)
library(here)

all_items = data.table(read_excel("zdata/Translation data_happ_app_MENTOR.xlsx"))
all_items[, id := 1:nrow(all_items)]
all_items = all_items[!grepl("Don|don",Comments_from_Helga)]
source_items = all_items[, .(id = paste(id,collapse = ", ")), by = .(Text,Instrument, Topic, Type)]
source("zdata/mindfullness_excersises.R")

translation_task = "Translate essential App text (e.g., instructions, terms, UI, error messages) for a mindfulness self-help application. Maintain precision for technical and legal text"
translation_role = "You are a highly experienced and meticulous translator specializing in digital mental health interventions and technical application content"
extra_guidelines =
"Ensure all translations are accurate and retain original meaning, especially for technical or legal phrases.
Translate the text into natural, idiomatic Norwegian – not word-for-word. Avoid literal translations, and write in a way that sounds like fluent, original Norwegian. The tone should reflect how a Norwegian mindfulness instructor, educator, or guide would express these ideas, whether in writing or speech.

Do not mirror English phrasing. Avoid translated-sounding expressions such as 'din oppgave,' 'utføre oppgaver', 'øke velvære', or 'original tenker'. Use Norwegian expressions like 'oppgaven din,' 'gjøre oppgaver,' 'fremme trivsel', 'tenke nytt'

As a general rule, place possessive pronouns after the noun (e.g. 'oppgaven din' instead of 'din oppgave'). Omit the pronoun entirely when ownership is obvious from context.

Simplify or rephrase overly formal or abstract expressions like 'det innebærer evnen til å…' or 'det går utover…' into more natural alternatives like 'det betyr at du kan…' or 'det handler ikke bare om…'

Prioritize flow, readability, and a native sense of tone over direct accuracy. The result should feel like it was originally written in Norwegian, not translated. Rewrite rather than preserve awkward structures or academic formulations.

Use the following recurring translations consistently:
Well being = Trivsel
Practice = Øve på / jobbe med
Identify = Tenk på / finn ut av
Strengths = Styrker
Love of Learning = Lærelyst
Perspective = Perspektiv
Bravery = Tapperhet
Honesty = Ærlighet
Perseverance = Utholdenhet
Zest = Livsgnist
Kindness = Vennlighet
Love = Kjærlighet
Prudence = Klokskap
Appreciation of beauty and excellence = Verdsettelse av skjønnhet og kvalitet

Make sure these terms are naturally integrated into the sentence, and feel like a part of living language, not fixed labels.
"

items = prep_TranslationItems(
   data = source_items,
   task = translation_task,
   role = translation_role,
   guidelines = extra_guidelines,
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
  translated_items[, .(Instrument,Topic,translated_item,Type, id)] %>%
  setnames("translated_item","Text")

data_back_translation[grep('"', Text), Text := gsub('"','',Text)]
data_back_translation[grep('“|”', Text), Text := gsub('“|”','',Text)]
extra_guidelines =
  "Ensure all translations are accurate and retain original meaning, especially for technical or legal phrases."
items_back_translation = prep_TranslationItems(
  data = data_back_translation,
  task = translation_task,
  role = translation_role,
  guidelines = extra_guidelines,
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

setnames(translated_items,"original_item","Text")
options(ellmer_timeout_s = 120)
evaluation =
  eval_translations(translated_items, back_translated_items, tmp_path = "tmp_eval.Rdata")

saveRDS(evaluation,file = "happapp_evaluation.RDS")


evaluation[evaluation != "OK"]

small_ids = source_items[grepl("Terms|Account|UI|Day|Info|Error",Instrument),id]
evaluation[, Context := ifelse(id %in% small_ids,"Tech","Psych")]
evaluation = evaluation[order(Context)][, Context := NULL]
small_rows = which(evaluation$id %in% source_items[grepl("Terms|Account|UI|Day|Info|Error",Instrument),id])


ft <- flextable(evaluation) |>
  width(j = "id",         width = .6) |>
  width(j = "original",         width = 2.9) |>
  width(j = "translation",      width = 2.9) |>
  width(j = "back_translation", width = 2.9) |>
  width(j = "evaluation",       width = 1.5) |>
  fontsize(i = small_rows, j = NULL, size = 8, part = "body") |>
  set_table_properties(layout = "fixed", width = 1)

default_sect_properties <- prop_section(
  page_size = page_size(orient = "landscape"), type = "continuous",
  page_margins = page_mar(bottom = .5, top = .5, right = .5, left = .5)
)

doc <- read_docx() |>
  body_set_default_section(default_sect_properties) |>
  body_add_flextable(ft)
print(doc, target = here("happ_app_translation_evaluation.docx"))


writexl::write_xlsx(evaluation, path = here("happ_app_translation_evaluation.xlsx"))
