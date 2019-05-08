#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualisatie planning Orbid"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("DatumRange",label = "Selecteer periode:", weekstart = 1),
      selectInput("Factory",label = "Selecteer Vestiging:",choices = c("FC01","FC02","FC03","FC04","FC05"),multiple = FALSE, selected = "FC02"),
      uiOutput("ui"),
      width = 3
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      addSpinner(plotOutput("distPlot", height = "800px", width = "112%"), spin = "circle")
    )
  )
))
