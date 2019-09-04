# remotes::install_github("cwida/duckdb/tools/rpkg", build = FALSE)

library(DBI)

con <- dbConnect(MonetDBLite::MonetDBLite(), "db/")

colnames(students) <- c("name", "matrnumber", "accepted", "note", "log", "motified")

dbWriteTable(con, "students", students)
