#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    theme = "bootstrap.css",
    style = "width: 95%",
    fluidRow(h3("So Predictable...")),
    fluidRow(column(11, textInput("inText", label = NULL, value = "", width="100%")),
             column( 1, actionButton("clrButton", label = "Clear", class = "btn btn-warning"))),
    fluidRow(column(11,div(id="predRow","")),
             column( 1, "")),
    fluidRow(hr(id="sep")),
    fluidRow(tabsetPanel(
                 tabPanel("Checking", textOutput("tickerCount")), 
                 tabPanel("Summary", h4("Nothing")), 
                 tabPanel("Instructions", helpText("Let's see how this looks"))))
))
