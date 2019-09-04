# remotes::install_github("hannesmuehleisen/MonetDBLite-R")
# install.packages("DBI")
# library(DBI)

con <- dbConnect(MonetDBLite::MonetDBLite(), "db/")

colnames(students) <- c("name", "matrnumber", "accepted", "note", "log", "motified")

dbWriteTable(con, "students", students)
