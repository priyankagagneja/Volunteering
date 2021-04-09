library(shiny)
library(shinysurveys)
dfsurvey <- data.frame(question = c("What's your name?",
                                    "What's your age?",
                                    "What's your education level?"),
                       option = NA,
                       input_type = "text",
                       input_id = c("name", "age", "education"),
                       dependence = NA,
                       dependence_value = NA, 
                       required = F)
# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Mentee Survey"),
    mainPanel(
        surveyOutput(df = dfsurvey),
        tableOutput("responseResults")
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
    output$responseResults <- renderTable({  print(Data()$df) })
}
shinyApp(ui, server)