library(shiny)
library(DT)
library(dplyr)
library(shinyFiles)

# Ask for backup path from user
backup_path <- rstudioapi::selectDirectory()

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # Hit enter to accept students
    # tags$head(includeScript("returnClick.js")),
    
    # App title ----
    titlePanel(
        h1(textOutput("nme"), align = "center")
    ),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            fluidRow(align = "center",
                textInput("search", label = h3("Search by Name or Number"), value = "")
            ),
            
            fluidRow(align = "center",
            
            actionButton("accept",
                         "Accept",
                         style = "color: black;
                         background-color: #209400;
                         width: 100px;
                         height: 50px"),
            
            actionButton("decline",
                         "Decline",
                         style = "color: black;
                         background-color: #940000;
                         width: 100px;
                         height: 50px")),
            
            fluidRow(align = "center",
                     textInput("note_text", label = h3("Add a Note"), value = "")
            ),
            
            fluidRow(align = "center",
                     
                     actionButton("note",
                                  "Note",
                                  style = "color: black;
                         background-color: #e06500;
                         width: 100px;
                         height: 50px")
                     ),
            
            h3(textOutput("sum")),
            htmlOutput("backup")
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            tabsetPanel(
                tabPanel("Accepted", tableOutput("studtable_accept")),
                
                tabPanel("Open", tableOutput("studtable_open")),
                
                tabPanel("With Note", tableOutput("studtable_note"))
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
            dplyr::filter(Matr.Number == input$search | Name == input$search) %>% 
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
    
    output$studtable_accept <- renderTable(rv$students %>%
                                        dplyr::filter(Accepted == TRUE))
    
    output$studtable_open <- renderTable(rv$students %>%
                                             dplyr::filter(Accepted == FALSE))
    
    output$studtable_note <- renderTable(rv$students %>%
                                        dplyr::filter(!is.na(Note)))
    
    # Accept Event
    observeEvent(input$accept, {
        
        # Accept if Searched by name:
        
        sid_a <- which(rv$students$Matr.Number == input$search |
                  rv$students$Name == input$search)
        
        rv$students[sid_a, "Accepted"] <- TRUE
        rv$students[sid_a, "Timestamp"] <- paste(
            rv$students[sid_a, "Timestamp"],
            Sys.time(), "[A]"
            )
        
        # Clear search field after accepting
        updateTextInput(session, "search", value = "")

    })
    
    # Decline Event
    observeEvent(input$decline, {
        
        sid_d <- which(rv$students$Matr.Number == input$search |
                           rv$students$Name == input$search)
        
        # Accept if Searched by name:
        rv$students[sid_d, "Accepted"] <- FALSE
        rv$students[sid_d, "Timestamp"] <- paste(
            rv$students[sid_d, "Timestamp"],
            Sys.time(), "[D]")
        
        # Clear search field after accepting
        updateTextInput(session, "search", value = "")
        
    })
    
    # Note event
    observeEvent(input$note, {
        
        sid_n <- which(rv$students$Matr.Number == input$search |
                           rv$students$Name == input$search)
        
        # Take Note by Number
        rv$students[sid_n, "Note"] <- paste(
            rv$students[sid_n, "Note"], 
            input$note_text
            )
        
        # Clear Note field after saving the note
        updateTextInput(session, "note", value = "")
        
    })
    
    observeEvent(input$goButton,{
        output$session <- renderText(function(){
            list.files(choose.dir())})
    })
    
    observeEvent(rv$students, {
        # Update the CSV File
        write.csv2(file = "students.csv", x = rv$students, row.names = FALSE)
        # Save internal Backup
        write.csv2(file = paste("log/students ", format(Sys.time(), "%d%b%Y_%H_%M_%S"), ".csv", sep = ""), x = rv$students, row.names = FALSE)
        # Save external Backup
        write.csv2(file = paste(backup_path, "/", format(Sys.time(), "%d%b%Y_%H_%M_%S"), ".csv", sep = ""), x = rv$students, row.names = FALSE)
    })
    
}

shinyApp(ui = ui, server = server, options = c(port = 1337, launch.browser = "chrome"))
