#
# So Predictable
#
# Capstone project for JHU's Data Science Specialization course
#
# Manuel Esteban-Infantes
#
# Requires previously generatend n-grams look-up table and co-occurrence matrix
#

require(shiny)

### Source common to all sessions #############################################

source("./common.R")

### UI ########################################################################

ui <- fluidPage(
    #theme = "bootstrap.css",
    style = "width: 95%",
    fluidRow(column(10, h2("So Predictable...")),
             column( 2, checkboxInput("cntxtToggle", "Context", FALSE))),
    fluidRow(column(10, textInput("inText", label = NULL, value = "", width="100%")),
             column( 2, actionButton("clrButton", label = "Clear", class = "btn btn-danger"))),
    fluidRow(id="predRow"),
    fluidRow(hr(id="sep")),
    fluidRow(tabsetPanel(
        tabPanel("Word Charts",
                 div(radioButtons("chartMode",NULL,c("Cloud"="W","Probability"="P"), inline = TRUE)),
                 div(plotOutput("wordPlot"))), 
        tabPanel("Inner Data",
                 fillRow(div(style=iwStyle,h4(textOutput("titleC1")),tableOutput("tableC1")),
                         div(style=iwStyle,h4(textOutput("titleC2")),tableOutput("tableC2")),
                         div(style=iwStyle,h4(textOutput("titleC3")),tableOutput("tableC3")),
                         div(style=iwStyle,h4(textOutput("titleC4")),tableOutput("tableC4")))), 
        tabPanel("Help", includeHTML("./help.html"))))
)

### Server function ###########################################################

server <- function(input, output, session) {
    
    ### Source core predicion within session context ##########################
    
    source("./core.R")
    
    ### Glocals: local gobals within the server function (user session) #######
    #                                                                         #
    
    stream   <- ""                                  # Typed text
    inTokens <- ""                                  # Typed tokens : trigger
    inCount  <- 0                                   # Typed token count
    inLast   <- ""                                  # Last typed token (or partial)
    inPart   <- FALSE                               # Last token is partial match
    wordle   <- dfm(tokens(paste(1:predsetsize)))   # Pre-packaged dfm for word cloud
    context  <- reactive({input$cntxtToggle})       # Context toggle : trigger
    typed    <- reactiveVal({FALSE})                # Typed trigger
    
    ### Local server functions to avoid passing parameters (speed & context) ##
    #                                                                         #
    
    # update input text after prediction button pressed #######################
    #    if partial==TRUE last (partial) token will be replaced by prediction

    updateInput <- function(btn, partial = FALSE, ltoken = "") {
        if (partial) {
            newstream <- stri_replace_last_fixed(stream,ltoken,ngpred$ahead[btn])
        } else {
            s <- ifelse(str_sub(stream,-1,-1)==" ",""," ")
            newstream <- paste(stream,ngpred$ahead[btn],sep=s)
        }
        updateTextInput(session,"inText",value=newstream)
    }
    
    # wordle chart # Hack a prebuilt dfm for speed: hard, but I got it! #######
    
    wordleChart <- function() {
        if (nglen==0) return(NULL)
        wordle@i                 <- as.integer(rep(0,nglen))
        wordle@p                 <- 0:nglen
        wordle@Dim[2]            <- as.integer(nglen)
        wordle@Dimnames$features <- ngpred$ahead
        wordle@x                 <- round(ngpred$psm*200,0)
        textplot_wordcloud(wordle, random_order = FALSE, color = "steelBlue4",
                           rotation = 0.25, min_size = 1, max_size     = 5)
        
    }
    
    # probabilty chart ########################################################
    
    probableChart <- function() {
        ggplot(data=ngpred,aes(x=reorder(ahead,-psm),y=psm, fill=psm))+
            geom_col()+ xlab("Prediction") + ylab("ML Probability") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
    }
    
    # inner data tables ##not nice code but faster ############################
    
    innerData <- function(content = FALSE) {
        if (!content) {
            output$titleC1 <- renderText({"no input"})
            output$tableC1 <- renderTable({NULL})
            output$titleC2 <- renderText({NULL})
            output$tableC2 <- renderTable({NULL})
            output$titleC3 <- renderText({NULL})
            output$tableC3 <- renderTable({NULL})
            output$titleC4 <- renderText({NULL})
            output$tableC4 <- renderTable({NULL})
        } else
        if (ngmode==2) {
            output$titleC1 <- renderText({"Best guess"})
            output$tableC1 <- renderTable({ngpred$ahead})
            output$titleC2 <- renderText({NULL})
            output$tableC2 <- renderTable({NULL})
            output$titleC3 <- renderText({NULL})
            output$tableC3 <- renderTable({NULL})
            output$titleC4 <- renderText({NULL})
            output$tableC4 <- renderTable({NULL})
        } else
        if (ngmode==1) {
            output$titleC1 <- renderText({"Context guess"})
            output$tableC1 <- renderTable({ngpred$ahead})
            output$titleC2 <- renderText({NULL})
            output$tableC2 <- renderTable({NULL})
            output$titleC3 <- renderText({NULL})
            output$tableC3 <- renderTable({NULL})
            output$titleC4 <- renderText({NULL})
            output$tableC4 <- renderTable({NULL})
        } else {
            output$titleC1 <- renderText({"N4"})
            output$tableC1 <- renderTable({ng4$ahead})
            output$titleC2 <- renderText({"N3"})
            output$tableC2 <- renderTable({ng3$ahead})
            output$titleC3 <- renderText({"N2"})
            output$tableC3 <- renderTable({ng2$ahead})
            if (ld1>0) {
                output$titleC4 <- renderText({"N1"})
                output$tableC4 <- renderTable({ng1$ahead})
            } else {
                output$titleC4 <- renderText({NULL})
                output$tableC4 <- renderTable({NULL})
            }
        }
    }
    
    ### Core Server: Output, reactivitym and event handlers ###################
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
        ngmode          <- 0
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

    # main observer: trigger prediction when input changes ####################    
    observe({
        context() | typed() 
        removeUI("#predButtons")
        if (inCount>0) {
            if (inPart) nextpart(inTokens,inCount,context())
            else        nextfull(inTokens,inCount,context())
            addPredButtons(ngpred$ahead[1:min(predsetshow,nglen)], inPart)
            output$wordPlot <- renderPlot({
                if (input$chartMode == "W") wordleChart()
                else                        probableChart()
            })
        }
        innerData(inCount>0)
    })


}


### Run the application #######################################################

shinyApp(ui = ui, server = server)

