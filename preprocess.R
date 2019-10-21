library(readxl)
library(dplyr)
library(tibble)
library(RSQLite)
library(pool)

dataset <- read_excel(file.choose(),skip = 4)

students <- dataset %>% as_tibble() %>% 
  mutate_at(vars("Matrikelnummer"), .funs = as.numeric) %>%
  dplyr::select("Vorname", "Nachname", "Matrikelnummer") %>%
  rename("name" = "Nachname", "forename" = "Vorname", "matrnumber" = "Matrikelnummer") %>%
  add_column("accepted" = NA, "note" = NA, "log" = NA, "modified" = NA, "shift" = NA, "overbooked" = NA) %>%
  filter_all(any_vars(!is.na(.)))

students$shift <- 1

# Identification of first overbooked student

overbooked_start <- (students$name == "Selik") %>% which()

students$overbooked[overbooked_start:nrow(students)] <- TRUE
students$overbooked[-c(overbooked_start:nrow(students))] <- FALSE

# create summary table:

stats <- data.frame(shift = c(1,2,3,4), 
                    sumstudents = c(0,0,0,0))
shift <- data.frame(shift = c(1))
dir.create("db", showWarnings = F)
con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")
dbWriteTable(con, "students", students)
dbWriteTable(con, "stats", stats, overwrite = T)
dbWriteTable(con, "shift", shift, overwrite = T)

# Use the example:
# 
# load(file = "students_example.Rda")
# stats <- data.frame(shift = c(1,2,3,4), sumstudents = c(0,0,0,0))
# dir.create("db", showWarnings = F)
# con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")
# dbWriteTable(con, "students", students)
# dbWriteTable(con, "stats", stats)
