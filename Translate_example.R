api_key = "AIzaSyBuh-UYHocYhpqF06ks5LBWqu-eAAzZRpk"
m = "gemini-2.0-flash-lite"
m = NULL
translated_items =
  translate_survey(source_file = here::here("zdata/Translation data MENTOR.xlsx"),
                example_file = here::here("zdata/Sample surveys translation.xlsx"),
                sleep = 1, llm_model = m, api_key = api_key)

translated_items[, c("Text","Instruction","Response","batch_idx") := NULL]

library(magrittr)

translated_items %>%
  .[, idx := 1:nrow(translated_items)] %>%
  .[, batch := min(idx), by = .(Instrument,Topic)] %>%
  .[, batch := as.numeric(idx)]

translated_items[, .(N = .N), by = .(Instrument,Topic, batch_idx)]
