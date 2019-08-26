library(rstudioapi)

jobRunScript("shiny-run.R")

rstudioapi::viewer("http://127.0.0.1:6018")