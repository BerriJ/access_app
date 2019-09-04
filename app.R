library(shinydashboard)
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(shinyWidgets)
library(RSQLite)
library(DBI)

# # Ask for backup path from user
# rstudioapi::showDialog("Backup Path", message = "The next step asks you to select
#                        a folder for backups. Consider using an external Device
#                        for this. A backup is created for every change and named
#                        by date and time.")
# backup_path <- rstudioapi::selectDirectory(caption = "Backup Folder")
# 
# while(is.null(backup_path)){
#   rstudioapi::showDialog("Backup Path", message = "Seriously: select a backup folder!")
#   backup_path <- rstudioapi::selectDirectory(caption = "Backup Folder")
# }

################################################################################
###################################  UI  #######################################
################################################################################

ui <- dashboardPage(skin = "green",
    
  dashboardHeader(title = "Students ID Check", disable = FALSE),
  
  dashboardSidebar(width = 300,
                   
    fluidRow(align = "center", # Row for search text input
             searchInput(
               inputId = "search", 
               label = h3("Search by Name or Number"), 
               placeholder = "Press Enter to search.", 
               btnSearch = icon("search"), 
               btnReset = icon("remove"), 
               width = "95%")),
    fluidRow(column(6, actionButton("decline", # Row for accept decline buttons
                                 "",
                                 style = "color: rgba(0, 0, 0, 0.5);
                         background-color: #dd4b39;
                         width: 100px;
                         height: 60px;
                         border-color:#dd4b39;
                                 font-size: 30px",
                                 icon = icon("user-times"))),
             column(6, actionButton("accept", "",
                                 style = "color: rgba(0, 0, 0, 0.5);
                         background-color: #00a65a;
                         width: 100px;
                         height: 60px;
                         border-color:#00a65a;
                         font-size: 30px",
                                 icon = icon("user-check")))),
    fluidRow(align = "center", # Row for note text input
             searchInput(
               inputId = "note", 
               label = h3("Add a Note"), 
               value = NULL,
               placeholder = "Press Enter to save.", 
               btnSearch = icon("save"), 
               btnReset = icon("remove"), 
               width = "95%")),
    # Info box for sum of accepted students
    fluidRow(align = "center",
             column(10, offset = 1,valueBoxOutput("progressBox", width = NULL))),
    
    # Info box for sum of students with a note
    fluidRow(align = "center",style = "position:fixed, bottom:0",
             column(10, offset = 1,valueBoxOutput("progressBox2", width = NULL))),
    # Backup path
    htmlOutput("backup"),
    # Include footer
    includeCSS("www/footer.css"), includeHTML("www/footer.html")),
  
  dashboardBody( # Main Panel
    
     # Refocus search bar after action
     tags$head(includeScript("www/refocus_search.js")),
     # Include Github corner
     includeHTML("www/github.html"),
    # Box with search result: 
    box(title = "Search Result:",
      collapsible = FALSE, width = NULL,
      h2(textOutput("nme"), align = "center")
    ),
    # Box with various tabs that show subsets of students dataframe
    tabBox(
      width = NULL, title = "Overview",side = "right", selected = "Checked In",
      tabPanel("Checked In", DT::dataTableOutput("studtable_accept")),
      tabPanel("Open", DT::dataTableOutput("studtable_open")),
      tabPanel("With Note", DT::dataTableOutput("studtable_note"))
    )
  )
)

################################################################################
#################################  Server  #####################################
################################################################################

server <- function(input, output, session) {
  con <- dbConnect(RSQLite::SQLite(), "db/students_db")
  # rv will store reactive values like students dataframe
  rv <- reactiveValues(students = tbl(con, "students"))
  # Open the connection to database
  s <- 0
  observe({
    studentsorig <- reactivePoll(intervalMillis = 50, session = session, 
                             checkFunc = function() {
                               if(all_equal(as.data.frame(rv$students),tbl(con, "students")) %>% isTRUE()){
                                 ""
                               }else{
                                 s <- s+1
                                 print(s)
                                 s
                               }},
                             valueFunc = function(){
                               dbReadTable(con, "students")})
    rv$students <- studentsorig()
  })

  output$nme <- renderText({rv$students %>% dplyr::filter(
    str_detect(input$search, as.character(rv$students$matrnumber)) |
          name == input$search) %>% dplyr::select(name) %>% unlist()})

  output$progressBox <- renderValueBox({
    valueBox(paste0(sum(rv$students %>% dplyr::filter(accepted == TRUE) %>%
                          dplyr::count())), "students checked in.",
             icon = icon("user-check"), color = "green")})

  output$progressBox2 <- renderValueBox({valueBox(paste0(sum(rv$students %>%
                         dplyr::filter(!is.na(note)) %>%
                           dplyr::count())), "students with note.",
                         icon = icon("clipboard"), color = "yellow")})

  output$backup <- renderUI({
    HTML(paste("Saving Backup to:<br/>", backup_path, "/", sep= ""))})

  output$studtable_accept <- DT::renderDataTable(rv$students %>%
                                                   dplyr::filter(accepted == TRUE) %>%
                                                   arrange(desc(modified)))
  output$studtable_open <- DT::renderDataTable(rv$students)
  output$studtable_note <- DT::renderDataTable(rv$students %>%
                                                 dplyr::filter(!is.na(note)) %>%
                                                   arrange(desc(modified)))

  # Accept Event
  observeEvent(input$accept, {
    
    sid_a <- which(str_detect(input$search, 
                              as.character(rv$students$matrnumber)) |
                     rv$students$name == input$search)
    
    # Check if (only) one student is selected
    if(length(sid_a) == 1){
      
      if(!as.logical(rv$students[sid_a, "accepted"])){
        # Accept the student
        rv$students[sid_a, "accepted"] <- TRUE
        rv$students[sid_a, "log"] <- paste(na.omit(c(rv$students[sid_a, "log"],as.character(Sys.time()), "[A]")), collapse = " ")
        rv$students[sid_a, "modified"] <- Sys.time()
        # Save log
        cat(paste(
          as.character(Sys.time()),rv$students[sid_a, "matrnumber"],"[A]"), 
          file= paste("log/log_", session$clientData$url_hostname, ".txt", sep = ""), 
        append=TRUE, sep="\n")
        # Save backup log
        cat(paste(
          as.character(Sys.time()),rv$students[sid_a, "matrnumber"],"[A]"), 
          file= paste(backup_path, "/log_", session$clientData$url_hostname, ".txt", sep = ""), 
          append=TRUE, sep="\n")
        
        # Clear search field and refocus
        updateSearchInput(session, "search", value = "", trigger = TRUE)
        session$sendCustomMessage("focus_search", "focus")
      } else {
        sendSweetAlert(session, title = "Already Accepted", 
                       text = "This student is already checked in! Consider taking a note!")
      }
    } else {
      sendSweetAlert(session, title = "Selection", 
                     text = "Please select one student.")
    }
  })
  
  # Decline Event
  observeEvent(input$decline, {
    
    sid_d <- which(str_detect(input$search, 
                              as.character(rv$students$matrnumber)) |
                     rv$students$name == input$search)
    
    if(length(sid_d) == 1){
      if(rv$students[sid_d, "accepted"] == TRUE){
        confirmSweetAlert(
          session = session,
          inputId = "decline_confirm",
          type = "warning",
          title = "Want to check out?",
          text = "Do you really want to check out? This should rarely be the case.",
          danger_mode = TRUE)
      } else {
        sendSweetAlert(session, title = "Can't Check Out!", 
                       text = "Can't check out! This student is still marked as checked out. Consider taking a note!")
      }}})
  
  observeEvent(input$decline_confirm, {
    if (isTRUE(input$decline_confirm)) {
      
      sid_d <- which(str_detect(input$search, 
                                as.character(rv$students$matrnumber)) |
                       rv$students$name == input$search)
      
      rv$students[sid_d, "accepted"] <- FALSE
      rv$students[sid_d, "log"] <- paste(na.omit(c(rv$students[sid_d, "log"], as.character(Sys.time()), "[D]")), collapse = " ")
      rv$students[sid_d, "modified"] <- Sys.time()
      
      # Save log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_d, "matrnumber"],"[D]"), 
        file= paste("log/log_", session$clientData$url_hostname, ".txt", sep = ""), 
      append=TRUE, sep="\n")
      # Save backup log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_d, "matrnumber"],"[D]"), 
        file= paste(backup_path, "/log_", session$clientData$url_hostname, ".txt", sep = ""), 
        append=TRUE, sep="\n")
      
      # Clear search field and refocus
      updateSearchInput(session, "search", value = "", trigger = TRUE)
      session$sendCustomMessage("focus_search", "focus")
    }})
  
  # Note event
  observeEvent(input$note, {
    
    sid_n <- which(str_detect(input$search, 
                              as.character(rv$students$matrnumber)) |
                     rv$students$name == input$search)
    
    # Take Note if single student is selected else notify
    if(length(sid_n) == 1){
      
      rv$students[sid_n, "note"] <- paste(na.omit(c(rv$students[sid_n, "note"], input$note)), collapse = " ")
      rv$students[sid_n, "log"] <- paste(na.omit(c(rv$students[sid_n, "log"],as.character(Sys.time()), "[N]")), collapse = " ")
      rv$students[sid_n, "modified"] <- Sys.time()
      
      # Save log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_n, "matrnumber"],"[N]",input$note), 
        file= paste("log/log_", session$clientData$url_hostname, ".txt", sep = ""), 
      append=TRUE, sep="\n")
      # Save backup log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_n, "matrnumber"],"[N]",input$note), 
        file= paste(backup_path, "/log_", session$clientData$url_hostname, ".txt", sep = ""), 
        append=TRUE, sep="\n")
      
      # Clear search field and refocus
      updateSearchInput(session, "search", value = "", trigger = TRUE)
      updateSearchInput(session, "note", value = "", trigger = FALSE)
      session$sendCustomMessage("focus_search", "focus")
    } else {
      if(input$note != ""){
        sendSweetAlert(session, title = "Selection", 
                       text = "Please select one student.")   
      }
    }
  })
  
  # observeEvent(rv$students, {
  #   # Update the CSV File
  #   write.csv2(file = "students.csv", x = rv$students, row.names = FALSE)
  #   # Save external Backup
  #   write.csv2(file = paste(backup_path, "/", "students.csv", sep = ""), x = rv$students, row.names = FALSE)
  # })

}

shinyApp(ui, server)