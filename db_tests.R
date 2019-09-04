# remotes::install_github("cwida/duckdb/tools/rpkg", build = FALSE)

library(DBI)

con <- dbConnect(MonetDBLite::MonetDBLite(), "db/")

colnames(students) <- c("name", "matrnumber", "accepted", "note", "log", "motified")

dbWriteTable(con, "students", students)

dbReadTable(con, "students")

students_db <- tbl(con, "students")

students_db <- students_db %>% dplyr::filter(name == "James") %>% mutate(accepted = TRUE)

request <- paste("UPDATE students",
                 "SET accepted = 'TRUE' ",
                 "WHERE name = 'James' ")

con %>% dbExecute(request)
