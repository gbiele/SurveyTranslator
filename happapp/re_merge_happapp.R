library(data.table)
library(readxl)
library(writexl)
library(magrittr)

all_items = data.table(read_excel("zdata/Translation data_happ_app_MENTOR.xlsx"))
all_items[, id := 1:nrow(all_items)]
all_items[, Norwegian_translation := NULL]

translated_items = data.table(read_excel("zdata/happ_app_translation_evaluation_olav.xlsx"))
setnames(translated_items, c("translation"),"Norwegian_translation")
translated_items[, id := as.integer(id)]

merged = merge(all_items, translated_items, by = "id", all = TRUE)

tmp = translated_items[is.na(id)]

new_id = max(all_items$id)

for (j in tmp$original) {
  nid = all_items[Text == j, id]
  repl = cbind(data.table(id = nid),tmp[original == j & is.na(id),][, id := NULL])
  translated_items = rbind(translated_items[original != j], repl)
}


merged = merge(all_items, translated_items, by = "id", all = TRUE)
tmp = translated_items[is.na(id)]

write_xlsx(merged,"zdata/HappApp_Norwegian_translation_no.xlsx")
