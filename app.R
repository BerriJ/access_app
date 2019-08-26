library(shiny)
library(DT)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
    
    # App title ----
    titlePanel(
        h1(textOutput("nme"), align = "center")
    ),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            
            fluidRow(align = "center",
                textInput("nme", label = h3("Name"), value = ""),
                
                textInput("mtrklnr", label = h3("Matrikel Nr."), value = "")
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
            
            actionButton("revert",
                         "Revert",
                         style = "color: black;
                         background-color: #424242;
                         width: 100px;
                         height: 50px"),
            actionButton("load",
                         "Load file",
                         style = "color: black;
                         background-color: #424242;
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
server <- function(input, output) {
    
    load("students.rda", envir = .GlobalEnv)
    
    output$nme <- renderText({ 
        paste(students[students$Matr.Number == input$mtrklnr, "Name"])
    })
    
    rv <- reactiveValues(students = students)
    
    output$studtable_accept <- renderTable(rv$students %>%
                                        dplyr::filter(Accepted == TRUE))
    
    output$studtable_open <- renderTable(rv$students %>%
                                             dplyr::filter(Accepted == FALSE))
    
    output$studtable_note <- renderTable(rv$students %>%
                                        dplyr::filter(!is.na(note)))
    
    observeEvent(input$accept, {
        print("accept")
        
        rv$students[rv$students$Matr.Number == input$mtrklnr, "Accepted"] <- TRUE

    })  
}

shinyApp(ui = ui, server = server)
