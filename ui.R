#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyBS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Average hours worked per day per person"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(width = 4,
      h5("Questions? Wishes? Bug report? Contact jeromecholewa@gmail.com"),
      h6("If you see some NA, please send your employees' exact English spelling as shown in the XL file, and the name of their department to jeromecholewa@gmail.com"),
      h6("Some xlsx files won't work. In that case, convert them to xls."),
      h5("Max file size = 10 MB"),
      br(),
      fileInput("filenameInit", "Pick your xlsx extract from GAIA",
                accept = c("xls", "xlsx")),
      h6(textOutput("FILE")),
      textOutput("errorMessage") ,
      br(),
      downloadButton("downloadData", label = "Save and check the Employee List"),
      bsTooltip("downloadData",
                'If a name is missing in this XL file, please send an email to jeromecholewa@gmail.com with the name and department  of the missing employee(s), exactly as spelled in GAIA. "KIM,MO-SSI" is not the same as "KIM, MO-Ssi" (with the space), and it is case sensitive.',
                placement = "bottom", trigger = "hover"),
      br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(), br(), br(), br(),
      textInput("selectYearMonth",
                "Pick the year-month (YYYY-MM) you want to display",
                value  = "2018-03"),
      br(), br(), br(), br(), br(), br(), br(), br(),
      br(), br(), br(), br(), br(),
      textInput("selectDept",
                "Pick the department you want to display",
                value  = "PAE")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("period")),
      h5("Days of early leave (<4.5 hours) are not taken into account in the calculations"),
      htmlOutput("messageEarly"),
      br(),
      plotOutput("GAIAPlot1"),
      plotOutput("GAIAPlot2"),
      plotOutput("GAIAPlot3"),
      plotOutput("PlotDept")
    )
  )
))
