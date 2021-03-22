library(shiny)
library(DT)
library(here)

here::here()

subskill_map = read.csv(file = "./../Data/subskill_map.csv", stringsAsFactors = F)

ui <- bootstrapPage(
    navbarPage(strong("WEST MENTORSHIP PROGRAM"), id="InOut",
               tabsetPanel(id = "inTabset",
               tabPanel("Inputs",
                    fluidPage(
                        
                        fluidRow(
                            h4("Load Survey Data"), 
                            column(3, fileInput('menteeSurvey', 'Mentee Survey File',
                                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                            column(3, fileInput('mentorSurvey', 'Mentor Survey File',
                                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                            column(3, fileInput('mentorAvailability', 'Mentor Availability File',
                                                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv'))),
                            
                            tags$hr()
                        ), # end fluirow
                        
                        fluidRow(
                            h4("Make Selections to generate Pods"),
                            column(3, dateInput("week", "Select Start date for week",value = '2021-03-02', format = "yyyy-mm-dd")),
                            # column(3, numericInput("cnt_topics","How many Topics will be covered this week", value = 9)),
                            column(3, numericInput("max_mentees","Maximum Mentees to be assigned", value = 3)),
                            column(3, numericInput("min_mentees","Minimum Mentees to be assigned", value = 1)),
                            
                            checkboxInput("opt_mentors","Should we optimize the mentors", value = FALSE),
                            br(),
                            column(3, actionButton('runScript', 'Generate Pods')),
                            br(),
                            tags$hr(),
                        ), # end fluirow
                        
                        fluidRow(
                            h4("View Input Data"),  
                            tags$hr(),
                            dataTableOutput('menteeTable') ,
                            br(),
                            dataTableOutput('mentorSurvTable') ,
                            br(),
                            dataTableOutput('mentorAvailTable') ,
                            br(),
                            tableOutput('subskillTable') 
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
                                                dataTableOutput('matchedOutput'))
                                )  #end column
                            )   #end fluidrow
                        )   #end fluidpage       
               )#end tabpanel
               )
    )#end navbarpage 
) #end bootstrapPage ui


# Define server logic
server <- function(input, output, session) {
    
    # output$x <- renderText(getwd())
    menteeSurvey <- reactive({
        
        inFile=input$menteeSurvey
        print(inFile)
        
        if(is.null(input$menteeSurvey)){
            return(NULL)}
        else {
            x <- read.csv(inFile$datapath) 
            print(inFile$name)
            print(inFile$datapath)
            return(x)  } 
    })
    
    mentorSurvey <- reactive({
        
        inFile=input$mentorSurvey
        print(inFile)
        
        if(is.null(input$mentorSurvey)){
            return(NULL)}
        else {
            x <- read.csv(inFile$datapath) 
            print(inFile$name)
            print(inFile$datapath)
            return(x)  } 
    })
    
    mentorAvail <- reactive({
        
        inFile=input$mentorAvailability
        print(inFile)
        
        if(is.null(input$mentorAvailability)){
            return(NULL)}
        else {
            x <- read.csv(inFile$datapath) 
            print(inFile$name)
            print(inFile$datapath)
            return(x)  } 
    })
    
    output$menteeTable <- renderDataTable({
        
        datatable(head(menteeSurvey()), 
                  caption = htmltools::tags$caption("Mentee Survey Data", style="font-weight: bold"),
                  rownames=FALSE)
    })
    
    output$mentorSurvTable <- renderDataTable({
        datatable(head(mentorSurvey()), 
                  caption = htmltools::tags$caption("Mentor Survey Data", style="font-weight: bold"),
                  rownames=FALSE)
    })
    
    output$mentorAvailTable <- renderDataTable({
        datatable(head(mentorAvail()), 
                  caption = htmltools::tags$caption("Mentor Availability Data", style="font-weight: bold"),
                  rownames=FALSE)
    })
    
    observeEvent(input$runScript, {
        updateTabsetPanel(session, "inTabset", selected = "Output")
    })
    
    results <- eventReactive(input$runScript, {
                            # source("print-hello.R")
                            # return(list(df=outputdf)) 
                            # source("main_matching.R", local = TRUE)
       
        # withProgress(message = 'Running code to generate pods')
        source("algorithm.R", local = TRUE)

        # source("main_matching.opt.R", local = TRUE)
                             # print("R script ran fine")
        return(list(df=assignments))
        })
    
    output$matchedOutput <- renderDataTable({  
        progress <- shiny::Progress$new()  # style = "old"
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        
        progress$set(message = "Running code to generate pods", value = 0)
        
        codeFiles <- c("algorithm.R","helper_functions.R")
        
        linesOfCode <- codeFiles %>%
            sapply(function(x) x %>% readLines() %>% length()) %>%
            sum()
        
        for (i in 1:linesOfCode) {
            # Each time through the loop, add another row of data. This is
            # a stand-in for a long-running computation
            
            # Increment the progress bar, and update the detail text.
            progress$inc(1/linesOfCode)
            
            # # Pause for 0.1 seconds to simulate a long computation.
            Sys.sleep(0.1)
        }
        
        results() 
        print(head(results()))
        })
    
    output$subskillTable <- renderTable({  
        print(head(subskill_map)) 
    })
    
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
