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
require(shinyjs)

### Source common to all sessions #############################################

source("./common.R")

### UI ########################################################################

iwStyle <- "border-style: dotted; border-width: 0px 1px 0px 1px; border-color: #DDDDDD"

iwCol  <- function(c) div(# style=iwStyle,
                          h4(textOutput(paste0("title",c))),
                          htmlOutput(paste0("texto",c)),
                          tableOutput(paste0("table",c)))

panel1 <- tabPanel("Word Charts",
                   div(radioButtons("chartMode",NULL,c("Probability"="P","Cloud"="W"), inline = TRUE, selected = "P")),
                   div(plotOutput("wordPlot")))

panel2 <- tabPanel("Inner Data", fillRow(iwCol("C1"), iwCol("C2"), iwCol("C3"), iwCol("C4")))

panel3 <- tabPanel("Help", includeHTML("./help.html"))

ui <- fluidPage(
    useShinyjs(),
    #theme = "bootstrap.css",
    style = "width: 95%",
    fluidRow(column(10, h2("So Predictable...")),
             column( 2, checkboxInput("cntxtToggle", "Context", FALSE))),
    fluidRow(column(10, textInput("inText", label = NULL, value = "type what you", width="100%")),
             column( 2, actionButton("clrButton", label = "Clear", class = "btn btn-danger"))),
    fluidRow(id="predRow"),
    fluidRow(hr(id="sep")),
    fluidRow(tabsetPanel(panel1, panel2, panel3)))


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
        wordle@x                 <- round(ngpred$psm*200,0)  # check other scales
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
    
    innerData <- function(tiC1=NULL,teC1=NULL,taC1=NULL,
                          tiC2=NULL,teC2=NULL,taC2=NULL,
                          tiC3=NULL,teC3=NULL,taC3=NULL,
                          tiC4=NULL,teC4=NULL,taC4=NULL) {
        output$titleC1 <- renderText({tiC1})
        output$textoC1 <- renderText({teC1})
        output$tableC1 <- renderTable({taC1},striped = TRUE, digits = 4)
        output$titleC2 <- renderText({tiC2})
        output$textoC2 <- renderText({teC2})
        output$tableC2 <- renderTable({taC2},striped = TRUE, digits = 4)
        output$titleC3 <- renderText({tiC3})
        output$textoC3 <- renderText({teC3})
        output$tableC3 <- renderTable({taC3},striped = TRUE, digits = 4)
        output$titleC4 <- renderText({tiC4})
        output$textoC4 <- renderText({teC4})
        output$tableC4 <- renderTable({taC4},striped = TRUE, digits = 4)
    }

    innerTC <- function(t,w) paste0("Terms ",t,"<br>Weight ",round(100*w,2),"%")
    
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
            if (ngmode==1) innerData(tiC1 = "Context Guess", taC1 = ngpred[1:min(ld1,predsetshow),])
            if (ngmode==2) innerData(tiC1 = "Best Guess", taC1 = ngpred[1:min(ld1,predsetshow),])
            if (ngmode==0) innerData(tiC1="N1",teC1=innerTC(ld1,lw1),taC1=ng1[1:min(ld1,predsetshow),],
                                     tiC2="N2",teC2=innerTC(ld2,lw2),taC2=ng2[1:min(ld2,predsetshow),],
                                     tiC3="N3",teC3=innerTC(ld3,lw3),taC3=ng3[1:min(ld3,predsetshow),],
                                     tiC4="N4",teC4=innerTC(ld4,lw4),taC4=ng4[1:min(ld4,predsetshow),])
        } else
            innerData()
        runjs(HTML("document.getElementById('inText').focus()")) # return focus to inText
    })


}


### Run the application #######################################################

shinyApp(ui = ui, server = server)

