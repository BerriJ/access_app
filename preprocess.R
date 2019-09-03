library(readxl)
library(dplyr)
library(tibble)

dataset <- read_excel("Z:/App/ZEA10021-Induktive_Statistik-SoSe_2019-1.xls",skip = 4)
dataset <- as_tibble(dataset) %>% 
             mutate_at(vars("Matrikelnummer"), .funs = as.numeric) %>%
             dplyr::select("Nachname", "Vorname", "Matrikelnummer") %>%
             rename("Name" = "Nachname", "Forename" = "Vorname", "Matr.Number" = "Matrikelnummer") %>%
             add_column("Accepted" = FALSE, "Note" = NA, "Log" = NA, "Modified" = NA, "Shift" = NA)

write.csv2(dataset, "students.csv", row.names = F)
             