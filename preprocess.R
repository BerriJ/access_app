library(readxl)
library(dplyr)
library(tibble)

dataset <- read_excel(file.choose(),skip = 4)

students <- dataset %>% as_tibble() %>% 
  mutate_at(vars("Matrikelnummer"), .funs = as.numeric) %>%
  dplyr::select("Vorname", "Nachname", "Matrikelnummer") %>%
  rename("name" = "Nachname", "forename" = "Vorname", "matrnumber" = "Matrikelnummer") %>%
  add_column("accepted" = FALSE, "note" = NA, "log" = NA, "modified" = NA, "shift" = NA) %>%
  filter_all(any_vars(!is.na(.)))

dir.create("db", showWarnings = F)
con <- dbConnect(RSQLite::SQLite(), "db/students_db")
dbWriteTable(con, "students", students)
