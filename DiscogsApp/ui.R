#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(),

    # Application title
    titlePanel("Discogs Release Data by Country, Format"),
    
    sidebarLayout(
      sidebarPanel(
        numericInput("start_year",
                     "Earliest Year:",
                     min = 1860,
                     max = 2022,
                     value = 1860),
        numericInput("end_year",
                     "Latest Year:",
                     min = 1860,
                     max = 2022,
                     value = 2022)
      ),
      
      mainPanel(
      
        div(
          id = "loading_page",
          h1("Please be patient, reading in data for 15 million albums is a lot.")
        ),
        hidden(
          div(
            id = "main_content",
              plotlyOutput("distPlot")
            )
          )
      )
      
     
    )
))
