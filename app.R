source("www/packages.R")
source("www/functions.R")
source("www/styling.R")

# Set options for data tables:
options(DT.options = list(pageLength = 10, lengthMenu = c(10, 25, 50, 100,250)))

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

# Create log folder if not existent
dir.create("backup_log", showWarnings = F)

con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")

# Connect to database:
onStop(function() {
  poolClose(con)
})

################################################################################
###################################  UI  #######################################
################################################################################

ui <- dashboardPage(skin = "green",
                    
                    dashboardHeader(title = "Students ID Check", disable = FALSE),
                    
                    dashboardSidebar(width = "350",
                                     
                                     fluidRow(align = "center",
                                              searchInput(
                                                inputId = "search", 
                                                label = h3("Search by Name or Number"), 
                                                placeholder = "Press Enter to search.", 
                                                btnSearch = icon("search"), 
                                                btnReset = icon("remove"), 
                                                width = "84%")),
                                     fluidRow(align = "center", column(10, offset = 1,
                                              actionButton("accept", "",
                                                                     style = button_accept,
                                                                     icon = icon("user-check")))),
                                     fluidRow(align = "center", style = "padding-top:20px",
                                              column(10, offset = 1, box( width = "NULL", collapsible = T, collapsed = T, title = "Take Note or Decline", background = "red",
                                                                          searchInput(
                                                                            inputId = "note", 
                                                                            label = NULL, 
                                                                            value = NULL,
                                                                            placeholder = "Press Enter to save.", 
                                                                            btnSearch = icon("save"), 
                                                                            btnReset = icon("remove"), 
                                                                            width = "100%"),
                                                                          actionButton("decline", # Row for accept decline buttons
                                                                                       "",
                                                                                       style = button_decline,
                                                                                       icon = icon("user-times"))
                                                
                                              ))),
                                     # Info box for sum of accepted students
                                     fluidRow(align = "center",
                                              column(10, offset = 1,valueBoxOutput("progressBox", width = NULL))),
                                     
                                     # Info box for sum of students with a note
                                     fluidRow(align = "center",style = "position:fixed, bottom:0",
                                              column(10, offset = 1,valueBoxOutput("progressBox2", width = NULL))),
                                     
                                     fluidRow(align = "center", style = "position:fixed, bottom:0",
                                              column(10, offset = 1, box(
                                                width = "NULL", collapsible = T, collapsed = T, title = "Shift options",background = "maroon",
                                                tags$style(slider_maxnumshift),
                                                sliderInput("maxnumshift", "Notify me at:", 50, 220, 65, 1, 
                                                            width = "100%", post = " Students"),
                                                actionButton("finish_shift", # Row for accept decline buttons
                                                             "Close Shift",
                                                             style = button_finish_shift)
                                              ))),
                                     # Backup path
                                     fluidRow(style = "padding-bottom:20px",
                                              htmlOutput("backup")),
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
                          h2(htmlOutput("results"), align = "center")
                      ),
                      # Box with various tabs that show subsets of students dataframe
                      fluidRow(
                      tabBox(
                        width = NULL, title = "Overview",side = "right", selected = "Checked In",
                        tabPanel("Checked In", DT::dataTableOutput("studtable_accept")),
                        tabPanel("Open", DT::dataTableOutput("studtable_open")),
                        tabPanel("With Note", DT::dataTableOutput("studtable_note")),
                        tabPanel("Declined", DT::dataTableOutput("studtable_decline")),
                        tabPanel("All", DT::dataTableOutput("studtable_all")),
                        tabPanel("Stats", DT::dataTableOutput("stats"))
                      ))
                    )
)

################################################################################
#################################  Server  #####################################
################################################################################

server <- function(input, output, session) {
  
  students <- function(){}
  stats <- function(){}
  
  students <- reactivePoll(intervalMillis = 50, session = session, 
                           checkFunc = function() {
                             if(all_equal(as.data.frame(students()),dbReadTable(con, "students")) %>% isTRUE()){0}else{1}},
                           valueFunc = function(){dbReadTable(con, "students")})
  
  stats <- reactivePoll(intervalMillis = 50, session = session, 
                           checkFunc = function() {
                             if(all_equal(as.data.frame(stats()),dbReadTable(con, "stats")) %>% isTRUE()){0}else{1}},
                           valueFunc = function(){dbReadTable(con, "stats")})
  
  # Render the search results:
  
  output$results <- renderUI({
    forename <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(forename) %>% unlist()
    name <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(name) %>% unlist()
    shift <- students() %>% dplyr::filter(
      str_detect(input$search, as.character(students()$matrnumber)) |
        name == input$search) %>% dplyr::select(shift) %>% unlist()
    if(length(name > 0)){
      HTML(paste(forename, name, "<br/>", "Shift:", shift )) 
    } else {HTML("No student selected.")}
  })
  
  # Render sum of accepted students
  
  output$progressBox <- renderValueBox({
    valueBox(paste0(sum(students() %>% dplyr::filter(accepted == TRUE) %>%
                          dplyr::count() - sum(stats()[,"sumstudents"]))), "students checked in.",
             icon = icon("user-check"), color = "green")})
  
  # Render sum of students with note
  
  output$progressBox2 <- renderValueBox({valueBox(paste0(sum(students() %>%
                                                               dplyr::filter(!is.na(note)) %>%
                                                               dplyr::count())), "students with note.",
                                                  icon = icon("user-edit"), color = "yellow")})
  
  # Render the backup path
  
  output$backup <- renderUI({
    HTML(paste("Saving Backup to:<br/>", backup_path, "/", sep= ""))})
  
  # Render all data tables
  
  output$studtable_accept <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(accepted == TRUE) %>%
                          dplyr::arrange(desc(modified)) %>%
                          dplyr::select(-modified), rownames = FALSE,
                        options = list(columnDefs = list(list(
                          className = 'dt-center', 
                          targets = 0:6))))
  output$studtable_decline <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(accepted == FALSE) %>%
                          dplyr::arrange(desc(modified)) %>%
                          dplyr::select(-modified), rownames = FALSE,
                        options = list(columnDefs = list(list(
                          className = 'dt-center', 
                          targets = 0:6))))
  output$studtable_open <- 
    DT::renderDataTable(students() %>% 
                          dplyr::filter(is.na(accepted)) %>% 
                          dplyr::arrange(desc(modified), name) %>%
                          dplyr::select(-modified), rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center', 
                            targets = 0:6))))
  output$studtable_note <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(!is.na(note)) %>%
                          arrange(desc(modified)) %>%
                          dplyr::select(-modified),rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center',
                            targets = 0:6))))
  output$studtable_all <- 
    DT::renderDataTable(students() %>%
                          arrange(desc(modified)) %>%
                          dplyr::select(-modified),rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center',
                            targets = 0:6))))
  output$stats <- 
    DT::renderDataTable(stats(),rownames = FALSE,options = list(
                          columnDefs = list(list(
                            className = 'dt-center',
                            targets = 0:1))))
  
  # Accept Event
  
  observeEvent(input$accept, {
    
    sid_a <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    # Check if (only) one student is selected
    if(length(sid_a) == 1){
      
      if(dbReadTable(con, "students")[sid_a, "accepted"] == FALSE | is.na(dbReadTable(con, "students")[sid_a, "accepted"])){
        # Accept the student, write log and write modification time
        con %>% dbExecute(paste("UPDATE students ",
                                "SET accepted = '1', log = '", paste(na.omit(c(students()[sid_a, "log"],as.character(Sys.time()), "[A]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                                "WHERE matrnumber = ",students()[sid_a, "matrnumber"], sep = ""))
        
        # Save a log, backup data, reset- and refocus search field
        log_backup_reset(sid = sid_a, 
                         event = "[A]", 
                         data = dbReadTable(con, "students"),
                         stats = dbReadTable(con, "stats"),
                         session = session,
                         backup_path = backup_path)
        
        if((sum(dbReadTable(con, "students") %>% dplyr::filter(accepted == TRUE) %>%
                nrow() - sum(stats()[,"sumstudents"]))) == input$maxnumshift){
          sendSweetAlert(session, title = "Limit Reached",
                         text = "You have reached the maximum number of Students.", type = "warning")
        }
        
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
  # Ask user to confirm or cancel
  observeEvent(input$decline, {
    
    sid_d <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    if(length(sid_d) == 1){
      if(is.na(students()[sid_d, "accepted"]) | students()[sid_d, "accepted"] == TRUE){
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
    if(isTRUE(input$decline_confirm)) {
      
      sid_d <- which(str_detect(input$search,
                                as.character(students()$matrnumber)) |
                       students()$name == input$search)
      
      con %>% dbExecute(paste("UPDATE students ",
                              "SET accepted = '0', log = '", paste(na.omit(c(students()[sid_d, "log"],as.character(Sys.time()), "[D]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                              "WHERE matrnumber = ",students()[sid_d, "matrnumber"], sep = ""))
      
      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_d, 
                       event = "[D]", 
                       data = dbReadTable(con, "students"),
                       stats = dbReadTable(con, "stats"),
                       session = session,
                       backup_path = backup_path)
    }})
  
  # Note event
  observeEvent(c(input$note, input$note_search), {
    
    sid_n <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)
    
    # Take Note if single student is selected else notify
    if(length(sid_n) == 1){
      
      con %>% dbExecute(paste("UPDATE students ",
                              "SET note = '", paste(na.omit(c(students()[sid_n, "note"], input$note)), collapse = " "),"', log = '", paste(na.omit(c(students()[sid_n, "log"],as.character(Sys.time()), "[N]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                              "WHERE matrnumber = ",students()[sid_n, "matrnumber"], sep = ""))
      
      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_n, 
                       event = "[N]", 
                       data = dbReadTable(con, "students"),
                       note = input$note,
                       stats = dbReadTable(con, "stats"),
                       session = session,
                       backup_path = backup_path)
      # Clear Note field
      updateSearchInput(session, "note", value = "", trigger = FALSE)
    } else {
      if(input$note != ""){
        sendSweetAlert(session, title = "Selection",
                       text = "Please select one student.")
      }
    }
  })
  
  # Close shift event:
  
  observeEvent(input$finish_shift, {
    confirmSweetAlert(
      session = session,
      inputId = "finish_confirm",
      type = "warning",
      title = "Want to finish this shift?",
      text = "This action will reset the sum of students. It can't be reverted.",
      danger_mode = TRUE)
  })
  
  observeEvent(input$finish_confirm, {
    
    con %>% dbExecute(paste("UPDATE stats ",
                            "SET sumstudents = ", (sum(students() %>% dplyr::filter(accepted == TRUE) %>% dplyr::count()) - sum(dbReadTable(con, "stats")[,"sumstudents"])),
                            " WHERE shift = ", min(which(dbReadTable(con, "stats")[,"sumstudents"] == 0)) , sep = ""))
    
  })
}

# options(shiny.host = '192.168.0.2')
options(shiny.host = '0.0.0.0')
options(shiny.port = 8888)

shinyApp(ui, server)