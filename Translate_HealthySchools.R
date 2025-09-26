api_key = NULL
m = NULL
m = "gemini-2.5-pro"

library(SurveyTranslator)
library(data.table)
library(readxl)
library(magrittr)
library(jsonlite)
library(flextable)
library(officer)
library(here)

source_items = data.table(read_excel("HealthySchools/Iceland_schools_criteria_items.xlsx"))
source_items[, topic := ifelse(is.na(criterion),"Background",criterion)]
source_items[, .(N = .N), by = .(School, topic)]
source_items = source_items[, .(Text, Topic = topic, Instrument = School, Type = "Item")]
example_items = data.table(read_excel("HealthySchools/examples_health_promotig_schools_questionnaire.xlsx"))
setnames(example_items,c("source","target"),c("source_item","target_item"))
translation_task = "Translate school health-promotion criteria and items from English to Norwegian (Bokmål) with precise, consistent terminology."
translation_role = "You are a highly experienced and meticulous translator specializing on physical and mental health in the school sector."
extra_guidelines =
"Goal:
Translate school health-promotion criteria and items from English to Norwegian (Bokmål) with precise, consistent terminology.

# Translation brief

Register & style:
 - Use Norwegian Bokmål.
 - Plain, professional, public-sector tone.
 - Prefer neutral present tense and declarative wording.
 - Avoid Anglicisms; use established public-health and education terms.
 - Use en dash (–) for “Criterion 1 – …” and Norwegian punctuation spacing.
 - Numbers/units: keep numerals and time units (min, timer) as in source; use Norwegian conventions (f.eks. 3–4 timer).

Glossary (preferred Norwegian terms)
 - health-promoting → helsefremmende
 - psychosocial → psykososial
 - mental health → psykisk helse
 - criterion / criteria → kriterium / kriterier
 - item → påstand / punkt (keep “item” semantics as a statement; if the style is “I …”, translate as “Jeg …”)
 - guideline(s) → retningslinje / retningslinjer
 - physical activity → fysisk aktivitet
 - school meal (guidelines) → skolemåltidet (Nasjonale retningslinjer for skolemåltidet)
 - tobacco-free → tobakksfri
 - substance use → rusmidler / rusbruk (context-dependent)
 - school health service → skolehelsetjenesten
 - public health nurse → helsesykepleier
 - classroom management → klasseledelse
 - student council → elevråd
 - after-school program → SFO (skolefritidsordningen)
 - guardians/parents → foresatte / foreldre (use foresatte when formal/neutral)
 - local community / surroundings → nærmiljø
 - traffic education → trafikkopplæring
 - internal control → internkontroll
 - netiquette / digital safety → nettvett / digital sikkerhet

Style rules for common patterns
 - Headings: Sentence case in Norwegian, e.g., “Skolepraksis og skolemiljø”.
 - “I …” statements: translate as “Jeg …” (present tense).
 - Use å/ø/æ correctly; avoid English hyphenation.
 - Use “skal” for requirements, “bør” for recommendations, mirror source strength.

Follow the Translation brief below exactly. Translate from English to Norwegian (Bokmål). Preserve structure and numbering.
"

items = prep_TranslationItems(
   data = source_items,
   task = translation_task,
   role = translation_role,
   guidelines = extra_guidelines,
   example_trans = example_items,
   source_language = "English",
   target_language = "Norwegian",
   domain = "Criteria and items for evaluation of health promoting schools.",
   batch_vars =  c("Instrument","Topic"),
   topic_var = "Topic",
   get_instr = FALSE
)


translated_items =
  translate_survey(items, sleep = 1, llm_model = m, api_key = api_key, tmp_path = "tmp_en_no.RDS")

saveRDS(translated_items,file = "HealthySchools/en_no_p.RDS")

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
  domain = "Criteria and items for evaluation of health promoting schools.",
  batch_vars =  c("Instrument","Topic"),
  topic_var = "Topic",
  get_instr = FALSE
)


back_translated_items =
  translate_survey(items_back_translation, sleep = 1, llm_model = m, api_key = api_key, tmp_path = "tmp_back_no_en.Rdata")

saveRDS(back_translated_items,file = "HealthySchools/back_no_en_p.RDS")

setnames(translated_items,"original_item","Text")
options(ellmer_timeout_s = 120)
evaluation =
  eval_translations(translated_items, back_translated_items, tmp_path = "tmp_eval.Rdata", llm_model = m)

saveRDS(evaluation,file = "HealthySchools/evaluation.RDS")


evaluation[evaluation != "OK"]


ft <- flextable(evaluation) |>
  width(j = "id",         width = .6) |>
  width(j = "original",         width = 2.9) |>
  width(j = "translation",      width = 2.9) |>
  width(j = "back_translation", width = 2.9) |>
  width(j = "evaluation",       width = 1.5) |>
  #fontsize(i = small_rows, j = NULL, size = 8, part = "body") |>
  set_table_properties(layout = "fixed", width = 1)

default_sect_properties <- prop_section(
  page_size = page_size(orient = "landscape"), type = "continuous",
  page_margins = page_mar(bottom = .5, top = .5, right = .5, left = .5)
)

doc <- read_docx() |>
  body_set_default_section(default_sect_properties) |>
  body_add_flextable(ft)
print(doc, target = here("HealthySchools/translation_evaluation.docx"))


writexl::write_xlsx(evaluation, path = here("HealthySchools/translation_evaluation.xlsx"))
