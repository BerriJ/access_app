log_backup_reset <- function(sid, event, note = NA, backup_path = backup_path, 
                             session = session, data, stats){
  # update log:
  cat(paste(na.omit(
    c(as.character(Sys.time()),data[sid, "matrnumber"], event, note)), collapse = " "),
    file= paste("backup/activity_log.txt", sep = ""),
    append=TRUE, sep="\n")
  # update backup log:
  cat(paste(na.omit(
    c(as.character(Sys.time()),data[sid, "matrnumber"], event, note)), collapse = " "),
    file= paste(backup_path, "/activity_log.txt", sep = ""),
    append=TRUE, sep="\n")
  # Update csv
  write.csv2(file = "backup/students.csv", x = data, row.names = FALSE)
  write.csv2(file = "backup/stats.csv", x = stats, row.names = FALSE)
  # update backup csv
  write.csv2(file = paste(backup_path, "/", "students.csv", sep = ""), x = data, row.names = FALSE)
  write.csv2(file = paste(backup_path, "/", "stats.csv", sep = ""), x = stats, row.names = FALSE)
  # Clear search field and refocus
  updateSearchInput(session, "search", value = "", trigger = TRUE)
  session$sendCustomMessage("focus_search", "focus")
}
