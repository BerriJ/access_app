log_backup_reset <- function(sid, event, note = NA, backup_path, session, data){
  # update log:
  cat(paste(na.omit(
    c(as.character(Sys.time()),data[sid, "matrnumber"], event, note)), collapse = " "),
    file= paste("backup_log/log_", session$clientData$url_hostname, ".txt", sep = ""),
    append=TRUE, sep="\n")
  # update backup log:
  cat(paste(na.omit(
    c(as.character(Sys.time()),data[sid, "matrnumber"], event, note)), collapse = " "),
    file= paste(backup_path, "/log_", session$clientData$url_hostname, ".txt", sep = ""),
    append=TRUE, sep="\n")
  # Update csv
  write.csv2(file = "backup_log/students.csv", x = data, row.names = FALSE)
  # update backup csv
  write.csv2(file = paste(backup_path, "/", "students.csv", sep = ""), x = data, row.names = FALSE)
  # Clear search field and refocus
  updateSearchInput(session, "search", value = "", trigger = TRUE)
  session$sendCustomMessage("focus_search", "focus")
}