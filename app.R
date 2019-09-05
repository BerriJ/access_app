source("packages.R")
source("functions.R")

# Create log folder if not existent
dir.create("backup_log", showWarnings = F)

# Set options for data tables:
options(DT.options = list(pageLength = 5, lengthMenu = c(5, 25, 50, 100,250)))

# Connect to database:

onStop(function() {
  poolClose(con)
})

con <- dbPool(drv = RSQLite::SQLite(), dbname = "db/students_db")

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
      h2(htmlOutput("nme"), align = "center")
    ),
    # Box with various tabs that show subsets of students dataframe
    tabBox(
      width = NULL, title = "Overview",side = "right", selected = "Checked In",
      tabPanel("Checked In", DT::dataTableOutput("studtable_accept")),
      tabPanel("Open", DT::dataTableOutput("studtable_open")),
      tabPanel("With Note", DT::dataTableOutput("studtable_note")),
      tabPanel("Declined", DT::dataTableOutput("studtable_decline"))
    )
  )
)

################################################################################
#################################  Server  #####################################
################################################################################

server <- function(input, output, session) {
  # con <- dbConnect(RSQLite::SQLite(), "db/students_db")
  # rv will store reactive values like students dataframe
  students <- function(){dbReadTable(con, "students")}
  # Open the connection to database
  s <- 0
  
  students <- reactivePoll(intervalMillis = 50, session = session, 
                               checkFunc = function() {
                                 if(all_equal(as.data.frame(students()),dbReadTable(con, "students")) %>% isTRUE()){0}else{1}
                                 },
                               valueFunc = function(){
                                 dbReadTable(con, "students")})
  

  output$nme <- renderUI({
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

  output$progressBox <- renderValueBox({
    valueBox(paste0(sum(students() %>% dplyr::filter(accepted == TRUE) %>%
                          dplyr::count())), "students checked in.",
             icon = icon("user-check"), color = "green")})

  output$progressBox2 <- renderValueBox({valueBox(paste0(sum(students() %>%
                         dplyr::filter(!is.na(note)) %>%
                           dplyr::count())), "students with note.",
                         icon = icon("clipboard"), color = "yellow")})

  output$backup <- renderUI({
    HTML(paste("Saving Backup to:<br/>", backup_path, "/", sep= ""))})

  output$studtable_accept <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(accepted == TRUE) %>%
                          dplyr::arrange(desc(modified)) %>%
                          dplyr::select(-modified), 
                        rownames = FALSE,
                        options = list(columnDefs = list(list(
                          className = 'dt-center', 
                          targets = 0:4))))
  output$studtable_decline <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(accepted == FALSE) %>%
                          dplyr::arrange(desc(modified)) %>%
                          dplyr::select(-modified), 
                        rownames = FALSE,
                        options = list(columnDefs = list(list(
                          className = 'dt-center', 
                          targets = 0:4))))
  output$studtable_open <- 
    DT::renderDataTable(students() %>% 
                          dplyr::filter(is.na(accepted)) %>% 
                          dplyr::arrange(desc(modified), name) %>%
                          dplyr::select(-modified),
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center', 
                            targets = 0:4))))
  output$studtable_note <- 
    DT::renderDataTable(students() %>%
                          dplyr::filter(!is.na(note)) %>%
                          arrange(desc(modified)) %>%
                          dplyr::select(-modified),
                        rownames = FALSE,
                        options = list(
                          columnDefs = list(list(
                            className = 'dt-center',
                            targets = 0:4))))

  # Accept Event
  observeEvent(input$accept, {

    sid_a <- which(str_detect(input$search,
                              as.character(students()$matrnumber)) |
                     students()$name == input$search)

    # Check if (only) one student is selected
    if(length(sid_a) == 1){

      if(students()[sid_a, "accepted"] == FALSE | is.na(students()[sid_a, "accepted"])){
        # Accept the student, write log and write modification time
        con %>% dbExecute(paste("UPDATE students ",
                                "SET accepted = '1', log = '", paste(na.omit(c(students()[sid_a, "log"],as.character(Sys.time()), "[A]")), collapse = " "),"', modified = '", Sys.time(), "' ",
                                "WHERE '",input$search, "' LIKE ('%' || matrnumber || '%') OR '",input$search,"' = name", sep = ""))

        # Save a log, backup data, reset- and refocus search field
        log_backup_reset(sid = sid_a, 
                         event = "[A]", 
                         backup_path = backup_path, 
                         session = session,
                         data = students())
        
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
                              "WHERE '",input$search, "' LIKE ('%' || matrnumber || '%') OR '",input$search,"' = name", sep = ""))

      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_d, 
                       event = "[D]", 
                       backup_path = backup_path, 
                       session = session,
                       data = students())
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
                              "WHERE '",input$search, "' LIKE ('%' || matrnumber || '%') OR '",input$search,"' = name", sep = ""))

      # Save a log, backup data, reset- and refocus search field
      log_backup_reset(sid = sid_n, 
                       event = "[N]", 
                       backup_path = backup_path, 
                       session = session, 
                       data = students(),
                       note = input$note)
      # Clear Note field
      updateSearchInput(session, "note", value = "", trigger = FALSE)
    } else {
      if(input$note != ""){
        sendSweetAlert(session, title = "Selection",
                       text = "Please select one student.")
      }
    }
  })
}

options(shiny.host = '192.168.0.2')
options(shiny.port = 8888)

shinyApp(ui, server)