library(shinydashboard)
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(shinyWidgets)

# Ask for backup path from user
rstudioapi::showDialog("Backup Path", message = "The next step asks you to select
                       a folder for backups. Consider using an external Device
                       for this. A backup is created for every change and named
                       by date and time.")
backup_path <- rstudioapi::selectDirectory(caption = "Backup Folder")

while(is.null(backup_path)){
  rstudioapi::showDialog("Backup Path", message = "Seriously: select a backup folder!")
  backup_path <- rstudioapi::selectDirectory(caption = "Backup Folder")
}

ui <- dashboardPage(skin = "green",
    
  dashboardHeader(title = "Students ID Check", disable = FALSE),
  
  dashboardSidebar(width = 300,
                   
    fluidRow(align = "center",
             # textInput("search", label = h3("Search by Name or Number"), value = "")
             
             searchInput(
               inputId = "search", 
               label = h3("Search by Name or Number"), 
               placeholder = "Press Enter to search.", 
               btnSearch = icon("search"), 
               btnReset = icon("remove"), 
               width = "95%"
             ) 
    ),
     
    fluidRow(
             column(6,
                    actionButton("decline",
                                 "",
                                 style = "color: rgba(0, 0, 0, 0.5);
                         background-color: #dd4b39;
                         width: 100px;
                         height: 60px;
                         border-color:#dd4b39;
                                 font-size: 30px",
                                 icon = icon("user-times"))),
             column(6,
                    actionButton("accept",
                                 "",
                                 style = "color: rgba(0, 0, 0, 0.5);
                         background-color: #00a65a;
                         width: 100px;
                         height: 60px;
                         border-color:#00a65a;
                         font-size: 30px",
                                 icon = icon("user-check")))),
    fluidRow(align = "center",
             searchInput(
               inputId = "note", 
               label = h3("Add a Note"), 
               value = NULL,
               placeholder = "Press Enter to save.", 
               btnSearch = icon("save"), 
               btnReset = icon("remove"), 
               width = "95%"
             )),
    # Add info box for sum of accepted students
    fluidRow(align = "center",
             column(10, offset = 1,valueBoxOutput("progressBox", width = NULL))),
    
    # Add info box for sum of students with a note
    fluidRow(align = "center",style = "position:fixed, bottom:0",
             column(10, offset = 1,valueBoxOutput("progressBox2", width = NULL))),
    
    htmlOutput("backup"),
    
    includeCSS("www/footer.css"), 
    includeHTML("www/footer.html")

  ),
  dashboardBody(
      
     # Refocus search bar after action
     tags$head(includeScript("www/refocus_search.js")),
     includeHTML("www/github.html"),
     
    box(title = "Search Result:",
      collapsible = FALSE, width = NULL,
      h2(textOutput("nme"), align = "center")
    ),
    
    tabBox(
      width = NULL, title = "Overview",side = "right",
      selected = "Checked In",
      tabPanel("Checked In", DT::dataTableOutput("studtable_accept")),
      
      tabPanel("Open", DT::dataTableOutput("studtable_open")),
      
      tabPanel("With Note", DT::dataTableOutput("studtable_note"))
    )
  )
)

server <- function(input, output, session) {
  
  # rv <- reactiveValues(students = read.csv2("students.csv", 
  #                                           stringsAsFactors = FALSE))
  
  rv <- reactiveValues()
  
  observe({
    studentsorig <- reactiveFileReader(100, session = session, filePath = "students.csv", readFunc = read.csv2, stringsAsFactors = FALSE)
    rv$students <- studentsorig()
    print(session$clientData$url_hostname)
  })
  
  output$nme <- renderText({
    rv$students %>% 
      dplyr::filter(
        str_detect(input$search, 
                   as.character(rv$students$Matr.Number)) | 
          Name == input$search) %>% 
      dplyr::select(Name) %>% unlist()
  })
  
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(sum(rv$students %>%
                   dplyr::filter(Accepted == TRUE) %>%
                   dplyr::count())), "students checked in.", icon = icon("user-check"),
      color = "green"
    )
  })
  
  output$progressBox2 <- renderValueBox({
      valueBox(
          paste0(sum(rv$students %>%
                         dplyr::filter(!is.na(Note)) %>%
                         dplyr::count())), "students with note.", icon = icon("clipboard"),
          color = "yellow"
      )
  })
  
  output$backup <- renderUI({
    HTML(paste("Saving Backup to:<br/>", backup_path, "/", sep= ""))
  })
  
  output$studtable_accept <- DT::renderDataTable(rv$students %>%
                                                   dplyr::filter(Accepted == TRUE) %>%
                                                   arrange(desc(Modified)))
  
  output$studtable_open <- DT::renderDataTable(rv$students %>%
                                                 dplyr::filter(Accepted == FALSE) %>% 
                                                   dplyr::arrange(desc(Modified), Name))
  
  output$studtable_note <- DT::renderDataTable(rv$students %>%
                                                 dplyr::filter(!is.na(Note)) %>%
                                                   arrange(desc(Modified)))
  
  # Accept Event
  observeEvent(input$accept, {
    
    sid_a <- which(str_detect(input$search, 
                              as.character(rv$students$Matr.Number)) |
                     rv$students$Name == input$search)
    
    # Check if (only) one student is selected
    if(length(sid_a) == 1){
      
      if(!as.logical(rv$students[sid_a, "Accepted"])){
        rv$students[sid_a, "Accepted"] <- TRUE
        rv$students[sid_a, "Log"] <- paste(na.omit(c(rv$students[sid_a, "Log"],as.character(Sys.time()), "[A]")), collapse = " ")
        rv$students[sid_a, "Modified"] <- Sys.time()
        # Save log
        cat(paste(
          as.character(Sys.time()),rv$students[sid_a, "Matr.Number"],"[A]"), 
          file= paste("log/log_", session$clientData$url_hostname, ".txt", sep = ""), 
        append=TRUE, sep="\n")
        # Save backup log
        cat(paste(
          as.character(Sys.time()),rv$students[sid_a, "Matr.Number"],"[A]"), 
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
                              as.character(rv$students$Matr.Number)) |
                     rv$students$Name == input$search)
    
    if(length(sid_d) == 1){
      if(rv$students[sid_d, "Accepted"] == TRUE){
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
      }
      
    } else {
      #sendSweetAlert(session, title = "Selection", 
      #               text = "Please select one student.")
    }
  })
  
  observeEvent(input$decline_confirm, {
    if (isTRUE(input$decline_confirm)) {
      
      sid_d <- which(str_detect(input$search, 
                                as.character(rv$students$Matr.Number)) |
                       rv$students$Name == input$search)
      
      rv$students[sid_d, "Accepted"] <- FALSE
      rv$students[sid_d, "Log"] <- paste(na.omit(c(rv$students[sid_d, "Log"], as.character(Sys.time()), "[D]")), collapse = " ")
      rv$students[sid_d, "Modified"] <- Sys.time()
      
      # Save log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_d, "Matr.Number"],"[D]"), 
        file= paste("log/log_", session$clientData$url_hostname, ".txt", sep = ""), 
      append=TRUE, sep="\n")
      # Save backup log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_d, "Matr.Number"],"[D]"), 
        file= paste(backup_path, "/log_", session$clientData$url_hostname, ".txt", sep = ""), 
        append=TRUE, sep="\n")
      
      # Clear search field and refocus
      updateSearchInput(session, "search", value = "", trigger = TRUE)
      session$sendCustomMessage("focus_search", "focus")
    } 
  })
  
  # Note event
  observeEvent(input$note, {
    
    sid_n <- which(str_detect(input$search, 
                              as.character(rv$students$Matr.Number)) |
                     rv$students$Name == input$search)
    
    # Take Note if single student is selected else notify
    if(length(sid_n) == 1){
      
      rv$students[sid_n, "Note"] <- paste(na.omit(c(rv$students[sid_n, "Note"], input$note)), collapse = " ")
      rv$students[sid_n, "Log"] <- paste(na.omit(c(rv$students[sid_n, "Log"],as.character(Sys.time()), "[N]")), collapse = " ")
      rv$students[sid_n, "Modified"] <- Sys.time()
      
      # Save log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_n, "Matr.Number"],"[N]",input$note), 
        file= paste("log/log_", session$clientData$url_hostname, ".txt", sep = ""), 
      append=TRUE, sep="\n")
      # Save backup log:
      cat(paste(
        as.character(Sys.time()),rv$students[sid_n, "Matr.Number"],"[N]",input$note), 
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
  
  observeEvent(rv$students, {
    # Update the CSV File
    write.csv2(file = "students.csv", x = rv$students, row.names = FALSE)
    # Save external Backup
    write.csv2(file = paste(backup_path, "/", "students.csv", sep = ""), x = rv$students, row.names = FALSE)
  })

}

shinyApp(ui, server)