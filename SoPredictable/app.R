#
# SoPredictable
#
# Capstone project for JHU's Data Science Specialization course
#
# Requires previously generatend n-grams look-up table and co-occurrence matrix
#

source("./Predict.R")
require(shiny)


### addPredButtons: Create DIV with buttons for each predicted word ###########

addPredButtons <- function(prediction) {
    insertUI("#predRow",where="afterBegin",div(id="predButtons"))
    for(i in length(prediction):1)
        insertUI("#predButtons", where="afterBegin",
                 actionButton(paste0("BTN",i),
                 prediction[i], class="btn btn-success"))
}


### isPartial: check if token tkn partially matches a token in predictor ######

isPartial <- function(tkn) {
    any(grep(paste0("^",tkn),alltokens)) & !(tkn %in% alltokens)
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
        tabPanel("Words", div(radioButtons("chartMode",NULL,c("Cloud"="W","Probability"="P"), inline = TRUE)),
                          div(plotOutput("wordPlot"))), 
        tabPanel("Inner working", tableOutput("predTable")), 
        tabPanel("Help", includeHTML("./help.html"))))
)

### Server function ###########################################################

server <- function(input, output, session) {
    
    ### Glocals: local gobals within the server function (user session) #######
    #                                                                         #
    
    stream   <- ""                                  # Typed text
    inTokens <- ""                                  # Typed tokens : trigger
    inCount  <- 0                                   # Typed token count
    inLast   <- ""                                  # Last typed token (or partial)
    inPart   <- FALSE                               # Last token is partial match
    predset  <- tibble()                            # Empty predicted set
    wordle   <- dfm(tokens(paste(1:predsetsize)))   # Pre-packaged dfm for word cloud
    context  <- reactive({input$cntxtToggle})       # Context toggle : trigger
    typed    <- reactiveVal({FALSE})                # Typed trigger
    
    ### Local server functions to avoid passing parameters (speed & context) ##
    #                                                                         #
    
    # update input text after prediction button pressed #######################
    #    if partial==TRUE last (partial) token will be replaced by prediction

    updateInput <- function(btn, partial = FALSE, ltoken = "") {
        if (partial) {
            newstream <- stri_replace_last_fixed(stream,ltoken,predset$ahead[btn])
        } else {
            s <- ifelse(str_sub(stream,-1,-1)==" ",""," ")
            newstream <- paste(stream,predset$ahead[btn],sep=s)
        }
        updateTextInput(session,"inText",value=newstream)
    }
    
    # wordle chart # Hack a prebuilt dfm for speed: hard, but I got it! #######
    
    wordleChart <- function() {
        predlen                  <- length(predset$ahead)
        if (predlen==0) return(NULL)
        wordle@i                 <- as.integer(rep(0,predlen))
        wordle@p                 <- 0:predlen
        wordle@Dim[2]            <- predlen
        wordle@Dimnames$features <- predset$ahead
        wordle@x                 <- round(predset$psm*200,0)
        textplot_wordcloud(wordle, random_order = FALSE, color = "steelBlue4",
                           rotation = 0.25, min_size = 1, max_size     = 5)
        
    }
    
    # probabilty chart ########################################################
    
    probableChart <- function() {
        ggplot(data=predset,aes(x=reorder(ahead,-psm),y=psm, fill=psm))+
            geom_col()+ xlab("Prediction") + ylab("ML Probability") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    }
    
    
    ### Reactivity - Observers and event handlers #############################
    #                                                                         #
    

    # Input text event handler ################################################
    
    observeEvent(input$inText, {
        stream   <<- input$inText
        inTokens <<- tokenize(stream)
        inCount  <<- length(inTokens)
        if (inCount>0) {
            inLast <<- inTokens[inCount]
            inPart <<- isPartial(inLast)
        } else {
            inLast <<- ""
            inPart <<- FALSE
            output$wordPlot <- NULL
        }
        typed(!typed())
    })
    
    # clrButton event handler #################################################
    
    observeEvent(input$clrButton, {
        updateTextInput(session,"inText",value="")
        removeUI("#predButtons")
        output$wordPlot <- NULL
    })

    # prediction buttons event handler ########################################

    observeEvent(input$BTN1,  { updateInput( 1, inPart, inLast) })
    observeEvent(input$BTN2,  { updateInput( 2, inPart, inLast) })
    observeEvent(input$BTN3,  { updateInput( 3, inPart, inLast) })
    observeEvent(input$BTN4,  { updateInput( 4, inPart, inLast) })
    observeEvent(input$BTN5,  { updateInput( 5, inPart, inLast) })
    observeEvent(input$BTN6,  { updateInput( 6, inPart, inLast) })
    observeEvent(input$BTN7,  { updateInput( 7, inPart, inLast) })
    observeEvent(input$BTN8,  { updateInput( 8, inPart, inLast) })
    observeEvent(input$BTN9,  { updateInput( 9, inPart, inLast) })
    observeEvent(input$BTN10, { updateInput(10, inPart, inLast) })
    
    observe({
        context() | typed() 
        removeUI("#predButtons")
        if (inCount>0) {
            if (inPart) predset <<- nextpart(inTokens,inCount,context())
            else        predset <<- nextfull(inTokens,inCount,context())
            addPredButtons(predset$ahead[1:min(predsetshow,length(predset$ahead))])
            output$wordPlot <- renderPlot({
                if (input$chartMode == "W") wordleChart()
                else                        probableChart()
            })
        }
    })


}


### Run the application #######################################################

shinyApp(ui = ui, server = server)

