#
# SoPredictable
#
# Capstone project for JHU's Data Science Specialization course
#
# Requires previously generatend n-grams look-up table and co-occurrence matrix
#

source("./Predict.R")
require(shiny)

# addPredButtons: Create DIV with buttons for each predicted word and insert in UI

addPredButtons <- function(prediction) {
    insertUI("#predRow",where="afterBegin",div(id="predButtons"))
    for(i in length(prediction):1)
        insertUI("#predButtons", where="afterBegin",
                 actionButton(paste0("BTN",i),
                 prediction[i], class="btn btn-success"))
}


### UI ########################################################################

ui <- fluidPage(
    #theme = "bootstrap.css",
    style = "width: 95%",
    fluidRow(column(10, h2("So Predictable...")),
             column( 2, checkboxInput("cntxtToggle", "Context Boost", FALSE))),
    fluidRow(column(10, textInput("inText", label = NULL, value = "", width="100%")),
             column( 2, actionButton("clrButton", label = "Clear", class = "btn btn-danger"))),
    fluidRow(id="predRow"),
    fluidRow(hr(id="sep")),
    fluidRow(tabsetPanel(
        tabPanel("Words", plotOutput("wordPlot")), 
        tabPanel("Inner working", tableOutput("predTable")), 
        tabPanel("Instructions", helpText("Let's see how this looks"))))
)

### Server function ###########################################################

server <- function(input, output, session) {
    
    # local: update input text after prediction button press
    updateInput <- function(btn) { 
        s <- ifelse(str_sub(inText(),-1,-1)==" ",""," ")
        updateTextInput(session,"inText",value=paste(inText(),predset$ahead[btn],sep=s))
    }
    
    inText  <- reactive({input$inText})         # Typed text
    context <- reactive({input$cntxtToggle})    # Context boost toggle
    inTkns  <- character()
    predset <- tibble()
    
    observeEvent(input$inText, {
        inTkns <<- inTokenize(inText())
        removeUI("#predButtons")
        if ((l<-length(inTkns))>0) {
            predset <<- nextfull(inTkns,l,context())
            addPredButtons(predset$ahead[1:min(predsetshow,length(predset$ahead))])
            output$wordPlot <- renderPlot({
#                ggplot(data=predset,aes(x=reorder(ahead,-psm),y=psm,stat=))+geom_col()
                textplot_wordcloud(dfm(tokens(rep(predset$ahead,predset$psm/min(predset$psm)))))
            })
        }
    })
    
    # clrButton event handler
    observeEvent(input$clrButton, {
        updateTextInput(session,"inText",value="")
        removeUI("#predButtons")
        output$wordPlot <- NULL
    })
    
    observeEvent(input$BTN1,  { updateInput(1) })
    observeEvent(input$BTN2,  { updateInput(2) })
    observeEvent(input$BTN3,  { updateInput(3) })
    observeEvent(input$BTN4,  { updateInput(4) })
    observeEvent(input$BTN5,  { updateInput(5) })
    observeEvent(input$BTN6,  { updateInput(6) })
    observeEvent(input$BTN7,  { updateInput(7) })
    observeEvent(input$BTN8,  { updateInput(8) })
    observeEvent(input$BTN9,  { updateInput(9) })
    observeEvent(input$BTN10, { updateInput(10) })
}


### Run the application #######################################################

shinyApp(ui = ui, server = server)

