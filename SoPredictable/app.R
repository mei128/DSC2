#
# SoPredictable
#
# Capstone project for JHU's Data Science Specialization course
#
# Requires previously generatend n-grams look-up table and co-occurrence matrix
#
require(tidyverse)
require(readtext)
require(quanteda)
require(stringi)
require(data.table)
require(shiny)

library(lexicon)

### Global defines ############################################################

predsetsize     <- 50                       # max prediction set size: truncate prediction to this size
predsetshow     <- 10                       # number of terms to show from prediction set
dpath_fcm_p     <- "../data/fcm_p.rds"      # co-occurrence matrix - ML
dpath_lookahead <- "../data/lookahead.rds"  # Look ahead table

### Support functions #########################################################

# inTokenize - Convert input to character vector by tokens

inTokenize <- function(input) {
    t <- tokens(stri_trans_tolower(input),
                remove_punct   = TRUE,
                remove_symbols = TRUE,
                remove_numbers = TRUE,
                remove_twitter = TRUE,
                remove_url     = TRUE)[[1]]
    return(t)
}


# fake prediction funtion to test design - same signature as definitive

whatnext <- function(tkns, tknl, context = FALSE) {
    pl     <- round(runif(1,min=1,max=predsetsize-1),0)
    pl     <- tibble(ahead=sw_dolch[sample(1:220,pl)],psm = sample(pl))
    if (context)
        pl <- rbind.data.frame(pl,tibble(ahead="CONTEXTO", psm=0.0001))
    ps     <- sum(pl$psm)
    pl$psm <- pl$psm/ps+4*context
    return(arrange(pl,desc(psm)))
}


# addPredButtons: Create DIV with buttons for each predicted word and insert in UI

addPredButtons <- function(prediction) {
    insertUI("#predRow",where="afterBegin",div(id="predButtons"))
    for(i in length(prediction):1)
        insertUI("#predButtons", where="afterBegin",
                 actionButton(paste0("BTN",i),
                 prediction[i], class="btn"))
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
            predset <<- whatnext(inTkns,l,context())
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
    
    
    
    observeEvent(input$BTN1,  {updateInput(1)})
    observeEvent(input$BTN2,  {updateInput(2)})
    observeEvent(input$BTN3,  {updateInput(3)})
    observeEvent(input$BTN4,  {updateInput(4)})
    observeEvent(input$BTN5,  {updateInput(5)})
    observeEvent(input$BTN6,  {updateInput(6)})
    observeEvent(input$BTN7,  {updateInput(7)})
    observeEvent(input$BTN8,  {updateInput(8)})
    observeEvent(input$BTN9,  {updateInput(9)})
    observeEvent(input$BTN10, {updateInput(10)})
}

### Global initialization #####################################################

if (FALSE) {
lookahead <- read_rds(dpath_lookahead)                          # Load look ahead ngrams
fcmp      <- read_rds(dpath_fcm_p)                              # Load co-occurence
lt4       <- length(lookahead[[4]]$ahead)                       # N-gram table sizes for look-ahead weighting
lt3       <- length(lookahead[[3]]$ahead)                       #
lt2       <- length(lookahead[[2]]$ahead)                       #
lt1       <- length(lookahead[[1]]$ahead)                       #
bestguess <- arrange(lookahead[[1]],desc(psm))[1:predsetshow,]  # Best single token guess, for speed
cntxguess <- colnames(fcmp)                                     # Tokens in cooccurrence matrix, for speed
}

### Run the application #######################################################

shinyApp(ui = ui, server = server)

