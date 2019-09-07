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

seats <-  150    # set No. of seats available
overbook <- 0.05    # factor for overbooking (input as decimal)

booking <- seats*(1+overbook)
shift_no <- ceiling(nrow(students)/booking)
students$shift <- rep(1:shift_no, each = booking)[1:nrow(students)]

write.csv2(file = "example_students2.csv", students, row.names = F)

# students$overbooked[(rep(1:2, each = (booking-seats))*(seats+1):booking)] <- "OVERBOOKED"

# create summary table:

stats <- data.frame(shift = c(1,2,3,4), sumstudents = c(0,0,0,0))
dir.create("db", showWarnings = F)
con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")
dbWriteTable(con, "students", students)
dbWriteTable(con, "stats", stats, overwrite = T)
 
# Use the example:
# 
# load(file = "students_example.Rda")
# stats <- data.frame(shift = c(1,2,3,4), sumstudents = c(0,0,0,0))
# dir.create("db", showWarnings = F)
# con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")
# dbWriteTable(con, "students", students)
# dbWriteTable(con, "stats", stats)
