#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

predbtn <- function(bid,predword) {
    actionButton(bid,predword, class="btn btn-outline-secondary")
}

shinyServer(function(input, output, session) {

    output$tickerCount <- renderText({
        n <- nchar(input$inText)
        if (n>8) {
            insertUI("#predRow",where = "afterBegin", ui = predbtn("goB", input$inText))
        }
        paste0(n," chars so far.")
    })
    
    observeEvent(input$clrButton, {
        updateTextInput(session, "inText", value = "")
    })
})
