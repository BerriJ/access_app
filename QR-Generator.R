#install.packages('qrcode')
library(qrcode)
library(dplyr)
library(tibble)
library(readxl)

# Read in the list from Prüfungsamt:

students_accepted <- read_excel(file.choose(),skip = 4)

students_accepted <- students_accepted %>% dplyr::select(Matrikelnummer, Nachname, Vorname) %>%
  tidyr::drop_na()

# Read the list from moodle:

students_moodle <- read.csv(file.choose(), stringsAsFactors = F)

students_moodle <- students_moodle %>% dplyr::select(Nutzer, Matrikelnummer, E.Mail.Adresse) %>%
  tidyr::drop_na()

students_accepted$Matrikelnummer <- as.numeric(students_accepted$Matrikelnummer)

Full <- right_join(students_moodle, students_accepted, by = "Matrikelnummer")

file_path <- rstudioapi::selectDirectory()

Full$path <- paste(file_path, "/", Full$Matrikelnummer, ".jpeg", sep = "")

dir.create(file_path, recursive = T, showWarnings = F)

for (i in 1:nrow(Full)){
  jpeg(file = Full$path[i], width = 500, height = 500)
  qrcode_gen(paste(Full$Matrikelnummer[i]))
  dev.off()
}

colnames(Full) <- c("user", "number", "mail", "name", "forename", "path")

write.csv2(file = paste(file_path, "/table.csv", sep = ""), Full, row.names = F)
