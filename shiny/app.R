## ############################################################################
##
## DISCLAIMER: 
##
## This script has been developed for illustrative purposes only. 
## The script is provided without any warranty of any kind, either express or 
## implied. The entire risk arising out of the use or performance of the sample 
## script and documentation remains with you. In no event shall its
## author, or anyone else involved in the creation, production, or delivery of 
## the script be liable for any damages whatsoever (including, without 
## limitation, damages for loss of business profits, business interruption, 
## loss of business information, or other pecuniary loss) arising out of the use 
## of or inability to use the sample scripts or documentation, even if the 
## author has been advised of the possibility of such damages. 
##
## ############################################################################
##
## DESCRIPTION
## Shiny app for word prediction
##
## Version 1: Initially created on 20 Dec 2016
##
## Dependencies: profanity.R, pre_process.R and capstoneFunctions.R
##
## Written by: Felipe J Colón-González
## For any problems with this code, please contact f.colon@uea.ac.uk
## 
## ############################################################################

## -------------------------
## Source packages
## -------------------------

require(shiny)
require(shinydashboard)
require(data.table)
require(tidyr)
require(plyr)
require(ngram)
require(stringi)
require(data.table)
require(RWeka)
require(quanteda)
require(caTools)
require(dplyr)

## -------------------------
## Source functions and data
## -------------------------

# Define directory tree
myDir <- file.path("~/Documents/GitHub/capstone/")

# Source functions
source(file.path(myDir, "capstoneFunctions.R"))
source(file.path(myDir, "profanity.R"))

# Read input data
ngram1 <- readRDS(file.path(myDir, "output", "ngram1.rds"))
ngram2 <- readRDS(file.path(myDir, "output", "ngram2.rds"))
ngram3 <- readRDS(file.path(myDir, "output", "ngram3.rds"))
ngram4 <- readRDS(file.path(myDir, "output", "ngram4.rds"))



## -------------------------
## Define UI options
## -------------------------

ui <- dashboardPage(skin="blue",
                    
                    dashboardHeader(title="Word Predictor", titleWidth=250),
                    
                    dashboardSidebar(width=250,
                                     numericInput("n", 
                                                  label=h3("Maximum number of
                                                          suggestions:"), 
                                                  value = 5,
                                                  min = 1, 
                                                  max = 50),

                                     br(),
                                     h5("Created by:"),
                                     tags$a("Felipe J Colon-Gonzalez", 
                                            href="https://github.com/FelipeJColon"),
                                     br(),
                                     br(),
                                     h5("Contact me:"),
                                     tags$a("Email", 
                                            href="mailto:F.Colon@uea.ac.uk")),
                    
                    dashboardBody(
                           fluidRow(column(12,
                                           box(title="Enter some text",
                                               width=10,
                                               status="primary", 
                                               solidHeader=TRUE,
                                               textInput(inputId="text", 
                                                         label="", 
                                                         value="Your text here")))),
                           fluidRow(
                                  column(4,
                                         tableOutput("table")))
                    )
)

## -------------------------
## Define Server options
## -------------------------

server <- shinyServer(function(input, output, session) {
       
       predWord <-  reactive( {
              inText <- input$text
              nWords <- input$n
              predWord <- predictWord(inText, nRows=nWords)
       })
       
       # Output table
       output$table <- renderTable(predWord())
})


# Run app
shinyApp(ui=ui, server=server)

# ----------------------------
# End of file
# ----------------------------
