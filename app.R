library(shiny)
library(DT)
library(dplyr)
library(shinyFiles)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # Hit enter to accept students
    tags$head(includeScript("returnClick.js")),
    
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
                     )
            
        ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            tabsetPanel(
                tabPanel("Accepted", tableOutput("studtable_accept")),
                
                tabPanel("Open", tableOutput("studtable_open")),
                
                tabPanel("WNote", tableOutput("studtable_note"))
            )
        )
    )
)

# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
    
    rv <- reactiveValues(students = read.csv2("students.csv", 
                                              stringsAsFactors = FALSE))

    output$nme <- renderText({
        paste(rv$students[rv$students$Matr.Number == input$search, "Name"],
              rv$students[rv$students$Name == input$search, "Name"])
    })
    
    output$sum <- renderText({
        paste("Accepted:", sum(rv$students %>%
                                              dplyr::filter(Accepted == TRUE) %>%
                                              nrow()))
    })
    
    output$studtable_accept <- renderTable(rv$students %>%
                                        dplyr::filter(Accepted == TRUE))
    
    output$studtable_open <- renderTable(rv$students %>%
                                             dplyr::filter(Accepted == FALSE))
    
    output$studtable_note <- renderTable(rv$students %>%
                                        dplyr::filter(!is.na(Note)))
    
    # Accept Event
    
    observeEvent(input$accept, {
        print("accept")
        
        # Accept if Searched by name:
        rv$students[rv$students$Matr.Number == input$search, "Accepted"] <- TRUE
        rv$students[rv$students$Name == input$search, "Timestamp"] <- paste(
            rv$students[rv$students$Name == input$search, "Timestamp"],
            Sys.time(), "[D]")
        
        # Accept if searched by number
        rv$students[rv$students$Name == input$search, "Accepted"] <- TRUE
        rv$students[rv$students$Matr.Number == input$search, "Timestamp"] <- paste(
            rv$students[rv$students$Matr.Number == input$search, "Timestamp"],
            Sys.time(), "[D]")
        
        
        # Clear search field after accepting
        updateTextInput(session, "search", value = "")

    })
    
    # Decline Event
    observeEvent(input$decline, {
        print("decline")
        
        # Accept if Searched by name:
        rv$students[rv$students$Matr.Number == input$search, "Accepted"] <- FALSE
        rv$students[rv$students$Name == input$search, "Timestamp"] <- paste(
            rv$students[rv$students$Name == input$search, "Timestamp"],
            Sys.time(), "[D]")
        
        # Accept if searched by number
        rv$students[rv$students$Name == input$search, "Accepted"] <- FALSE
        rv$students[rv$students$Matr.Number == input$search, "Timestamp"] <- paste(
            rv$students[rv$students$Matr.Number == input$search, "Timestamp"],
            Sys.time(), "[D]")
        
        
        # Clear search field after accepting
        updateTextInput(session, "search", value = "")
        
    })
    
    # Add a note
    observeEvent(input$note, {
        print("note")
        
        # Take Note by Name
        rv$students[rv$students$Name == input$search, "Note"] <- paste(
            rv$students[rv$students$Name == input$search, "Note"], input$note_text)
        rv$students[rv$students$Name == input$search, "Timestamp"] <- paste(
            rv$students[rv$students$Name == input$search, "Timestamp"],
            Sys.time(), "[N]")
        
        # Take Note by Number
        rv$students[rv$students$Matr.Number == input$search, "note"] <- paste(
            rv$students[rv$students$Matr.Number == input$search, "Note"], input$note_text)
        rv$students[rv$students$Matr.Number == input$search, "Timestamp"] <- paste(
            rv$students[rv$students$Matr.Number == input$search, "Timestamp"],
            Sys.time(), "[N]")
        
        
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
        # Save a backup
        write.csv2(file = paste("log/students ", Sys.time(), ".csv", sep = ""), x = rv$students, row.names = FALSE)
    })
    
}

shinyApp(ui = ui, server = server)
