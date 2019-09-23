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

Mails_and_Numbers <- semi_join(students_moodle, students_accepted, by = "Matrikelnummer")
Missings <- anti_join(students_accepted,students_moodle, by = "Matrikelnummer")

Full <- full_join(Mails_and_Numbers, Missings)


before <- Sys.time()
for (i in 1:length(mat_num)) {
  jpeg(file = paste('qr/', mat_num[i], '.jpeg', sep = ''), width = 500, height = 500)
  qrcode_gen(paste(mat_num[i]))
  dev.off()
}
after <- Sys.time()

after - before
