#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   

  
  output$distPlot <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
   
    t = input$themes
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    
    ggplot(faithful,aes(faithful[,length(names(faithful))]))+geom_histogram(binwidth = input$bins,fill = input$color)+labs(x = input$xaxis,y = input$yaxis,title = input$title,caption = input$caption)+theme(plot.title = element_text(family = input$family,face = input$Face,colour = input$title_color,size = 20,hjust = input$hjust,))
    
  })
  
  output$axes <- renderUI({
    if(input$axisTitle == F) {return()}
    else
    
    labls = c(textInput(inputId = "title","Specify the title here","Plot title"),
              
     textInput(inputId = "caption","CAPTION text","")  ,
     textInput(inputId = "xaxis","X axis title"," X AXIS"),
    textInput(inputId = "yaxis","Y axis title"," Y AXIS"))
    return(labls)
    
  })
  
  #formating the title text
  output$title <- renderUI({
    if(input$Title == F) {return()}
    else
      
    return(c(textInput(inputId = "title","Specify the title here","Plot title"),
             selectInput(inputId = "Face",choices  = c(Bold = "bold",Regular = "plain",Italic = "italic","Bold Italic" = "bold.italic"),label = "Change the font face"),
             sliderInput(inputId = "title_color",label = "Change Color of the title",min = 0,max = 7,step = 1,value = 5),
    sliderInput(inputId = "vjust",label = "Change Vertical position of the title",min = 0,max = 1,step = .1,value = .5),
    sliderInput(inputId = "hjust",label = "Change Horizontal position of the title",min = 0,max = 1,step = .1,value = .5)))
  
    

  })
  
  
})
