#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("../PRDInc.R")

btnid    <- paste0("BTN",1:predsetshow)
btncount <- 0


predbtn <- function(bid,predword) {
    actionButton(bid,predword, class="btn btn-secondary")
}

clearPredButtons <- function() {
}

showPredButtons <- function(prediction) {
}


shinyServer(function(input, output, session) {

    output$tickerCount <- renderText({
        n <- nchar(input$inText)
        paste0(n," chars so far.")
        if (n>8) output$predButtons <- renderUI(predbtn("goB", input$inText))
    })

    
    
    observeEvent(input$clrButton, {
        updateTextInput(session, "inText", value = "")
    })
})
