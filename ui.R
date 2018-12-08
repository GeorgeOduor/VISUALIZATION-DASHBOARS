#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Learn ggPlot"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 5,
                   value = 3),
       selectInput(inputId = "color",label = "Select colour of your graph",choices = c("Brown" = "brown",
                                                                         "Blue" = "light blue"),selected = "Brown"),
       checkboxInput(inputId = 'axisTitle',label = "Labels",value = F),
       radioButtons(inputId = "Customize","customize Your Graph",choices = c(Labels="Labels",Colour = "colour")),
       #uiOutput("axes"),
       #for dynamic labeling
       conditionalPanel(condition = "input.Customize == 'Labels'",
                        checkboxInput(inputId = "Title",label = "Format title")),uiOutput("title"),
       conditionalPanel(condition = "input.Customize == 'colour'",
                        checkboxInput(inputId = "Title",label = "Format colour")               
                        ),
       selectInput(inputId = "themes","Select themes",choices = c("Bw"="theme_bw()", "Classic"="theme_classic()"),selected = "Bw")

    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      
       plotOutput("distPlot")
    )
  )
))
