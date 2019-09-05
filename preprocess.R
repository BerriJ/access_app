library(readxl)
library(dplyr)
library(tibble)
library(RSQLite)

dataset <- read_excel(file.choose(),skip = 4)

students <- dataset %>% as_tibble() %>% 
  mutate_at(vars("Matrikelnummer"), .funs = as.numeric) %>%
  dplyr::select("Vorname", "Nachname", "Matrikelnummer") %>%
  rename("name" = "Nachname", "forename" = "Vorname", "matrnumber" = "Matrikelnummer") %>%
  add_column("accepted" = NA, "note" = NA, "log" = NA, "modified" = NA, "shift" = NA) %>%
  filter_all(any_vars(!is.na(.)))

seats <-  150    # set No. of seats available
overbook <- 0.05    # factor for overbooking (input as decimal)

booking <- seats*(1+overbook)
shift_no <- ceiling(nrow(students)/booking)
students$shift <- rep(1:shift_no, each = booking)[1:nrow(students)]


dir.create("db", showWarnings = F)
con <- dbConnect(RSQLite::SQLite(), "db/students_db")
dbWriteTable(con, "students", students)

### Example:
read.csv2("example_students.csv")

dir.create("db", showWarnings = F)
con <- dbConnect(RSQLite::SQLite(), "db/students_db")
dbWriteTable(con, "students", students)