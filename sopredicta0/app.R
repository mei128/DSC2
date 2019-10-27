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
    fluidRow(textInput("inText", label = NULL, value = "", width="90%"),
             actionButton("clrButton","Doit",class = "btn btn-warning")),
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
server <- function(input, output, session) {
    
    pega <- function(x,y) {
        paste(x," -- ",y)
    }
    
    trigger <- reactiveValues()
    trigger$flag <- FALSE
    counter <- 0
    observeEvent(input$clrButton, {
        insertUI("#predRow",where="afterBegin",div(id="btnList"))
        for (i in 1:5){
            insertUI("#btnList",where="afterBegin",actionButton(paste0("B",i),paste0("Botón ",i)))
        }
    })
    observeEvent(input$B1, {print("uno")})
    observeEvent(input$B2, {trigger$flag<-!trigger$flag
        print("dos")})
    observeEvent(input$B3, {updateTextInput(session,"B1",label=pega("Bton","UNO"))})
    observeEvent(input$B4, {
        counter <<- counter + 1
        print(counter)})
    observeEvent(input$B5, {removeUI("#btnList")})
    observe({
        t<-trigger
        print(paste0("VALE ",t))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
