library(shiny)
library(DT)

ui <- bootstrapPage(
    navbarPage(strong("WEST MENTORSHIP PROGRAM"), id="InOut",
               tabPanel("Inputs",
                    fluidPage(
                        
                        fluidRow(
                            h4("Load Survey Data"), 
                            column(4, fileInput('menteeFile', 'Mentee Survey File',
                                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                            column(4, fileInput('mentorFile', 'Mentor Survey File',
                                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                            tags$hr()
                        ), # end fluirow
                        
                        fluidRow(
                            h4("Make Selections to generate Pods"),
                            column(3, dateInput("week", "Select Start date for week",value = '2021-03-02', format = "yyyy-mm-dd")),
                            column(3, numericInput("cnt_topics","How many Topics will be covered this week", value = 9)),
                            br(),
                            column(3, actionButton('runScript', 'Generate Pods')),
                            tags$hr(),
                        ), # end fluirow
                        
                        fluidRow(
                            h4("View Input Data"),  
                            tags$hr(),
                            dataTableOutput('menteeTable') ,
                            br(),
                            dataTableOutput('mentorTable') 
                            # , column(5, textOutput('x'))
                            ) #end fluidrow
                        
                        )#end fluidpage
               ),#end tabpanel
               
               tabPanel("Output",
                        fluidPage(
                            fluidRow(
                                column(2,
                                       # selectInput("dataout", "Choose Output:", choices=c("Results")),
                                       downloadButton('downloadData', 'Download')
                                ),    #end column
                                column(10, h4("View Output"), 
                                       tags$hr(),
                                       tabPanel('Results',        
                                                # textOutput('x'),
                                                tableOutput('matchedOutput'))
                                )  #end column
                            )   #end fluidrow
                        )   #end fluidpage       
               )#end tabpanel
    )#end navbarpage 
) #end bootstrapPage ui


# Define server logic
server <- function(input, output) {
    
    # output$x <- renderText(getwd())
    mentee <- reactive({
        
        inFile=input$menteeFile
        print(inFile)
        
        if(is.null(input$menteeFile)){
            return(NULL)}
        else {
            x <- read.csv(inFile$datapath) 
            print(inFile$name)
            print(inFile$datapath)
            return(x)  } 
    })
    
    mentor <- reactive({
        
        inFile=input$mentorFile
        print(inFile)
        
        if(is.null(input$mentorFile)){
            return(NULL)}
        else {
            x <- read.csv(inFile$datapath) 
            print(inFile$name)
            print(inFile$datapath)
            return(x)  } 
    })
    
    output$menteeTable <- renderDataTable({
        
        datatable(head(mentee()), 
                  caption = htmltools::tags$caption("Mentee Survey Data", style="font-weight: bold"),
                  rownames=FALSE)
    })
    
    output$mentorTable <- renderDataTable({
        datatable(head(mentor()), 
                  caption = htmltools::tags$caption("Mentor Survey Data", style="font-weight: bold"),
                  rownames=FALSE)
    })
    
    results <- eventReactive(input$runScript, {
                            # source("print-hello.R")
                            # return(list(df=outputdf)) 
                            # source("main_matching.R", local = TRUE)
        source("algorithm.R", local = TRUE)
                             # print("R script ran fine")
        return(list(df=assignments))
        })
    
    output$matchedOutput <- renderTable({  
        results() 
        print(head(results()))
        })
    
    # output$x <- renderText({  
    #     print(paste(input$cnt_topics, "\n",input$week))
    # })
    
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("Mentorship_pod_", Sys.Date(), ".csv", sep="")
            
        },
        content = function(file) {
            write.csv(results(), file)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
