###
### This shiny app generates the sample survey & is able to save the results in a googlesheet.
###

library(shiny)
library(shinysurveys)
library(googlesheets4)

dfsurvey <- read.csv("./WEST-sample survey-mentee.csv")
dfsurvey$option <- ifelse(dfsurvey$input_type == "numeric",20,dfsurvey$option)

gs4_auth(
    email = gargle::gargle_oauth_email(),
    path = NULL,
    scopes = "https://www.googleapis.com/auth/spreadsheets",
    cache = gargle::gargle_oauth_cache(),
    use_oob = gargle::gargle_oob_default(),
    token = NULL
)

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
        
        # Submit & Save the response on the googlesheets file
        sheet_append("https://docs.google.com/spreadsheets/d/13CNixY838ZjryizUiGp_O9boox_8KqrF0REpSNmCKH4/edit#gid=0", df, sheet = 1)
        
        return(list(df=df))
    })
    output$responseResults <- renderTable({  
        # Display the current values on the web browser
        print(Data()$df) 
        
        })
}
shinyApp(ui, server)
