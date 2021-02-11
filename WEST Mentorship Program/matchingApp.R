library(shiny)

ui <- bootstrapPage(
    navbarPage(strong("WEST MENTORSHIP PROGRAM"), id="InOut",
               tabPanel("Inputs",
                        fluidPage(
                            fluidRow(column(2, "Load Survey Data",
                                            br(), 
                                            fileInput('file1', 'Choose Input File',
                                                      accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                                            tags$hr(),
                                            actionButton('runScript', 'Generate Pods')
                                            
                            ),  #end column
                            
                            column(10, "View Input",  
                                   tags$hr(),
                                   tabPanel('Input Data', dataTableOutput('surveyResults') )
                                   
                            )   #end column
                            )#end fluidrow
                        )#end fluidpage
               ),#end tabpanel
               
               tabPanel("Output",
                        fluidPage(
                            fluidRow(
                                column(2,
                                       # selectInput("dataout", "Choose Output:", choices=c("Results")),
                                       downloadButton('downloadData', 'Download')
                                ),    #end column
                                column(10, "View Output", 
                                       tags$hr(),
                                       tabPanel('Results', tableOutput('matchedOutput'))
                                )  #end column
                            )   #end fluidrow
                        )   #end fluidpage       
               )#end tabsetpanel
    )#end navbarpage 
) #end bootstrapPage ui


# Define server logic
server <- function(input, output) {
    
    inputData <- reactive({
        
        inFile=input$file1
        print(inFile)
        
        if(is.null(input$file1)){
            return(NULL)}
        else {
            x <- read.csv(inFile$datapath) 
            print(inFile$name)
            print(inFile$datapath)
            return(x)  } 
    })
    
    output$surveyResults <- renderDataTable({
        inputData()                      
    })
    
    d <- eventReactive(input$runScript, {
                             source("print-hello.R")
                             return(list(df=df)) 
        })
    
    output$matchedOutput <- renderTable({  d() 
        print(head(d()))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
