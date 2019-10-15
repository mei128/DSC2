#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI

ui <- fluidPage(
    # theme = "bootstrap.min.css",
    style = "width: 95%",
    fluidRow(h3("So Predictable...")),
    fluidRow(column(11,textInput("inStream",label=NULL, value="", width='100%')),
             column(1,actionButton("clrButton","Clear",class = "btn btn-warning", width='100%'))),
    fluidRow(id="predRow",""),
    fluidRow(hr(id="sEparator")),
    fluidRow(
        tabsetPanel(
            tabPanel("Checking", textOutput("tickerCount")), 
            tabPanel("Summary", h4("Nothing")), 
            tabPanel("Instructions", helpText("Let's see how this looks")))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$tickerCount <- renderText({
        n <- nchar(input$inStream)
        if (n>8) {
            insertUI("#predictRowr",where = "beforeEnd", ui = actionButton("goButton", input$inStream, class ="btn btn-secondary"))
            }
        paste0(n," chars so far.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
