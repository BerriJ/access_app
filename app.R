library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(shinyWidgets)


# Ask for backup path from user
backup_path <- rstudioapi::selectDirectory()

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # Hit enter to accept students
    tags$head(includeScript("refocus_search.js")),
    
    # App title ----
    titlePanel(
        h1(textOutput("nme"), align = "center")
    ),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            fluidRow(align = "center",
                     # textInput("search", label = h3("Search by Name or Number"), value = "")
                     
                     searchInput(
                         inputId = "search", 
                         label = h3("Search by Name or Number"), 
                         placeholder = "Press Enter to search.", 
                         btnSearch = icon("search"), 
                         btnReset = icon("remove"), 
                         width = "100%"
                     )
            ),
            
            fluidRow(align = "center",
                     
                     actionButton("accept",
                                  "Check In",
                                  style = "color: black;
                         background-color: #209400;
                         width: 100px;
                         height: 50px"),
                     
                     actionButton("decline",
                                  "Check Out",
                                  style = "color: black;
                         background-color: #940000;
                         width: 100px;
                         height: 50px")),
            
            fluidRow(align = "center",

                     searchInput(
                         inputId = "note", 
                         label = h3("Add a Note"), 
                         placeholder = "Press Enter to save.", 
                         btnSearch = icon("save"), 
                         btnReset = icon("remove"), 
                         width = "100%"
                     )
            ),
            
            h3(textOutput("sum")),
            htmlOutput("backup")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            tabsetPanel(
                tabPanel("Checked In", DT::dataTableOutput("studtable_accept")),
                
                tabPanel("Open", DT::dataTableOutput("studtable_open")),
                
                tabPanel("With Note", DT::dataTableOutput("studtable_note"))
            )
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    rv <- reactiveValues(students = read.csv2("students.csv", 
                                              stringsAsFactors = FALSE))
    
    output$nme <- renderText({
            rv$students %>% 
                dplyr::filter(
                    str_detect(input$search, 
                          as.character(rv$students$Matr.Number)) | 
                        Name == input$search) %>% 
                dplyr::select(Name) %>% unlist()
    })
    
    output$sum <- renderText({
        paste("Accepted:", sum(rv$students %>%
                                              dplyr::filter(Accepted == TRUE) %>%
                                              dplyr::count()))
    })
    
    output$backup <- renderUI({
        HTML(paste("Saving Backup to:<br/>", backup_path, "/", sep= ""))
    })
    
    output$studtable_accept <- DT::renderDataTable(rv$students %>%
                                        dplyr::filter(Accepted == TRUE) %>%
                                            arrange(desc(Modified)))
    
    output$studtable_open <- DT::renderDataTable(rv$students %>%
                                             dplyr::filter(Accepted == FALSE))
    
    output$studtable_note <- DT::renderDataTable(rv$students %>%
                                        dplyr::filter(!is.na(Note)))
    
    # Accept Event
    observeEvent(input$accept, {
        
        sid_a <- which(str_detect(input$search, 
                                  as.character(rv$students$Matr.Number)) |
                           rv$students$Name == input$search)
        
        # Check if (only) one student is selected
        if(length(sid_a) == 1){
            
            if(!as.logical(rv$students[sid_a, "Accepted"])){
                rv$students[sid_a, "Accepted"] <- TRUE
                rv$students[sid_a, "Log"] <- paste(na.omit(c(rv$students[sid_a, "Log"],Sys.time(), "[A]")), collapse = " ")
                rv$students[sid_a, "Modified"] <- Sys.time()
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
            sendSweetAlert(session, title = "Selection", 
                           text = "Please select one student.")
        }
    })
    
    observeEvent(input$decline_confirm, {
        if (isTRUE(input$decline_confirm)) {
            
            sid_d <- which(str_detect(input$search, 
                                      as.character(rv$students$Matr.Number)) |
                               rv$students$Name == input$search)
            
                rv$students[sid_d, "Accepted"] <- FALSE
                rv$students[sid_d, "Log"] <- paste(na.omit(c(rv$students[sid_d, "Log"],Sys.time(), "[D]")), collapse = " ")
                rv$students[sid_d, "Modified"] <- Sys.time()
                
                # Clear search field and refocus
                updateSearchInput(session, "search", value = "", trigger = TRUE)
                session$sendCustomMessage("selectText", "focus")
        } 
    })
    
    # Note event
    observeEvent(input$note, {
        
        sid_n <- which(str_detect(input$search, 
                                  as.character(rv$students$Matr.Number)) |
                           rv$students$Name == input$search)
        
        # Take Note if single student is selected
        if(length(sid_n) == 1){
            
            rv$students[sid_n, "Note"] <- paste(na.omit(c(rv$students[sid_n, "Note"], input$note)), collapse = " ")
            rv$students[sid_n, "Log"] <- paste(na.omit(c(rv$students[sid_n, "Log"],Sys.time(), "[N]")), collapse = " ")
            rv$students[sid_n, "Modified"] <- Sys.time()
            # Clear search field and refocus
            updateSearchInput(session, "search", value = "", trigger = TRUE)
            updateSearchInput(session, "note", value = "", trigger = FALSE)
            session$sendCustomMessage("focus_search", "focus")
        } else {
            sendSweetAlert(session, title = "Selection", 
                           text = "Please select one student.")
        }
    })
    
    observeEvent(rv$students, {
        # Update the CSV File
        write.csv2(file = "students.csv", x = rv$students, row.names = FALSE)
        # Save internal Backup
        write.csv2(file = paste("log/students ", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".csv", sep = ""), x = rv$students, row.names = FALSE)
        # Save external Backup
        write.csv2(file = paste(backup_path, "/", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), ".csv", sep = ""), x = rv$students, row.names = FALSE)
    })
    
}

shinyApp(ui = ui, server = server, options = c(port = 1337, launch.browser = "chrome"))