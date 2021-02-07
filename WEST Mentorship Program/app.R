#
# Create your own survey from a template in csv/excel
# 

# devtools::install_github("jdtrat/shinysurveys")

library(shiny)
library(shinysurveys)

dfsurvey <- WEST_mentee_survey

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mentee Survey"),
    mainPanel(
            surveyOutput(df = dfsurvey) 
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {

    renderSurvey(df = dfsurvey)
    
    # Create a df and then allow for the responses to be downloaded.
    
    Data = eventReactive( input$submit , {
            df <- data.frame(name=input$name, 
                             age=input$age, 
                             education = input$education)
            return(list(df=df))
    })
    
    # output$responseResults <- renderTable({  print(Data()$df) })
}

# Run the application 
shinyApp(ui = ui, server = server)
