#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
ista

#===========Define UI for application that draws a histogram============
library(shiny)
library(shinydashboard)
library(highcharter)
library(dplyr)
workplaceSecurity = read.csv('dashboard.csv')
workplaceSecurity$Country[workplaceSecurity$Country=="drc"] = "democratic republic of the congo"
workplaceSecurity$Country[workplaceSecurity$Country=="iraq/kri" |
                            workplaceSecurity$Country=="iraq / kri" |
                            workplaceSecurity$Country=="iraq/ kri"] = "democratic republic of the congo"
workplaceSecurity$Country[workplaceSecurity$Country=="car"] = "central african republic"
workplaceSecurity$Country[workplaceSecurity$Country=="sudan"] = "south sudan"


#============VALUE BOXES================= 

ui <- dashboardPage(skin = "purple",
  dashboardHeader(title = "INSECURITY INSIGHT - Aid Worker KKA (Killed,Kidnapped or Arrested)",titleWidth = 850,
    dropdownMenuOutput('v'),
                  dropdownMenuOutput("notification"),
                  dropdownMenuOutput("myskils")),
  #============SIDEBAR================= 
  
dashboardSidebar(disable = T,sidebarMenu(paste("Use this sidebar to dynamicaly change \n the charts.")
    
                   )),
  #============BODY================= 
  
  dashboardBody(
    fluidRow(
      column(width = 3,imageOutput("c_flag",height ="auto",width = "3.0%")),
      column(width = 3,infoBoxOutput("killed",width = 12)),
      column(width = 3,infoBoxOutput('kidnapped',width = 12)),
      column(width = 3,infoBoxOutput('arrested',width = 12))
    ),
#=============PIE CHART FIRST PLOT================
    fluidRow(
      box(height = 275,solidHeader = F,
        title = "Total affected  Aid workers", width = 6, status = "primary",
        column(width = 4,
               selectInput("year","Please Select Year",choices = c('2017' = 2017,'2018'=2018,'2019'=2019),
                                     width = "100%"),
               selectInput("country","Please Select Country",
                           choices = as.character(
                             x = (workplaceSecurity %>%
                                    filter(numberes_affected > 0))$Country %>%
                               unique() %>% as.character()),width = "100%")
               ),
        column(width = 8,
               highchartOutput("piechart",width ="100%",height="210px"))
        
      ),
#============Yearly reports second plot=================
      
      tabBox(id="tabs",title = "Monthly and Yearly Reports",height = 275,
             tabPanel("Monthly report",
                      highchartOutput("monthlyreport",width ="100%",height="220px")),
             tabPanel("Yearly Report",
                      highchartOutput("yearlyreport",width ="100%",height="220px")))
      
    ),
#==============DYANMIC TEXTS===============
fluidRow(
  box(id="naration_control",width=8,height=275,solidHeader = T,
      column(width = 4,tags$u(h3("SELECTIONS")),
             verbatimTextOutput("title"),
             verbatimTextOutput("years"),
             verbatimTextOutput("countrysel"),
             
             actionButton("aboutme","MEET THE APP DESIGNER",icon = icon("list"),color="red")),
      
      column(width = 8,tags$u(h3("COMMENTARYdd")),
             textOutput("report1"),"------------------",
              textOutput("report2"),"_________________",
              textOutput("report3"))),
  box("YEARLY TREND PER COUNTRY",height = 275, status = "warning", width = 4,
      highchartOutput("trend",width = "100%",height = "220px")))

))
  


##=============SERVER================

server <- function(input, output,session) {
  
  observeEvent(input$aboutme,{
    showModal(modalDialog(
      title = "Hello,My Name is George,I have designed this app from scratch.",easyClose = T,size = "m",
      "Thank you for stopping by to take a look at my app.",
      "I am currently looking for an entry position in data science and would realy appreciate if you recomend me.Thanks in advance.",
      imageOutput("george")
    ))
  })
  
  #images====
  
  output$george <- renderImage({
    # When input$country is kenya, filename is ./www/images/kenya.jpg
    filename <- normalizePath(file.path('./www/images',
                                        paste('','george', '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image name"),
         width= 400,
         height =400
         
    )
    
  }, deleteFile = FALSE)
  
  output$c_flag <- renderImage({
    # When input$country is kenya, filename is ./www/images/kenya.jpg
    filename <- normalizePath(file.path('./www/images',
                                        paste('',input$country, '.png', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste("Image name",input$country),
         width= 300,
         height = 90,
         margin = 50
         )
    
  }, deleteFile = FALSE)
  
  #report 1 =====
  output$report1 = renderText({
    df = workplaceSecurity  %>% group_by(effect)%>%
      filter(year==input$year)%>% 
      summarise(count = sum(na.rm = T,numberes_affected)) %>% 
      mutate(pecentage = (round(count/sum(count),2))*100) %>%
      arrange(pecentage)
    affected = df[max(df$count)==df$count,]
    paste("In the year",input$year,"a total of ",affected$count ," Aid workers were",
        affected$effect,
           "world wide which was ",affected$pecentage,"% of all the aid workers and was the highest propotion of aid workers affected that year.")
  })
  #report 2 =====
  output$report2 = renderText({
    monthly_data = workplaceSecurity %>%
      filter(year==input$year,Country == input$country) %>%
      group_by(month) %>%
      summarise(n=sum(na.rm = T,numberes_affected))%>%
      arrange(n)
    month = monthly_data[max(monthly_data$n) == monthly_data$n,]$month

    if (nrow(monthly_data) > 0 & sum(na.rm = T,max(monthly_data$n))!=0) {
      paste(
        ifelse(length(month) > 1 ,
               yes = paste(Hmisc::capitalize(knitr::combine_words(month)),"were the worst months"),
               no = paste(Hmisc::capitalize(knitr::combine_words(month))," was the worst month")
        ),
        "to work in ",input$country," in the year  ", input$year ," with a total of ", sum(max(monthly_data$n)) ,"affected aid workers")
    }else{
      paste("We had no data reported in ",input$year," from ",input$country,"or  zero reports were recorded from this country.")
    }
    
    
  })
  
  output$report3 = renderText({
    "Afganistann has experinced an improvement in security towards the year 2019 while 2018 was the most insecure year."
    
    
  })
  
  #report 3=====
  output$report3 = renderText({
    yeartrnd = workplaceSecurity %>% filter(year==input$year)%>%group_by(Country) %>% summarise(tot=sum(na.rm=T,numberes_affected)) %>% arrange(desc(tot))
    paste0("The year ",input$year," ",yeartrnd$Country[1]," had the highest number of affected Aid workers in the whole world totaling to ",yeartrnd$tot[1],".")
  })
  #selected textss==== 
  
  output$title = renderText({
    "You have selected"
  })
  
  output$yearsel = renderText({
    paste("Year: ",input$year)
  })
  output$countrysel = renderText({
    paste("Country: ",input$country)
  })
  #reacctive expressions====
  filteredf <- reactive({
    monthlyrepo = workplaceSecurity
    monthlyrepo = workplaceSecurity %>% filter(Country == input$country,year==input$year) 
  })
  #yearly report=====
  output$yearlyreport = renderHighchart({
    workplaceSecurity %>% group_by(Country,year,effect) %>%
      summarise(n = sum(na.rm = T,numberes_affected)) %>%
      filter(Country== input$country) %>%
      hchart("bar",hcaes(x="year",y="n",group = "effect"))
      
  })  
  
  #monthly report====
  output$monthlyreport = renderHighchart({
    dat = filteredf()
    if (dim(dat)[1]>1 & sum(na.rm = T,dat[,5]) != 0 ){
      hc_legend(workplaceSecurity %>%
                  filter(Country==input$country,year == input$year) %>%
                  arrange(numberes_affected) %>%
                  hchart("column",hcaes(x="month",y="numberes_affected",group = "effect")) %>%
                  hc_add_theme(hc_theme_google()),verticalAlign = "top")%>% hc_add_theme(hc_theme_ffx())
    }
  })

  #trend ====
  output$trend = renderHighchart({
     return(
       workplaceSecurity %>%
         group_by(year,effect) %>% filter(Country == input$country) %>%
         summarise(Count=sum(na.rm = T,numberes_affected)) %>%
         hchart(type="spline",hcaes("year","Count",group="effect"),colors = c("green","yellow","red"))%>%
         hc_xAxis(title = "",opposite = F)%>%
         hc_add_theme(hc_theme_ffx())
    )
  })
  
  
  #Overall distribution piechart====
  output$piechart = renderHighchart({
    highchart()%>%
      hc_chart(type="pie" ) %>% 
      hc_add_series_labels_values(labels = levels(factor(workplaceSecurity$effect)),
                                  values =  (workplaceSecurity %>% 
                                               filter(year== input$year)%>% group_by(effect) %>% 
                                               summarise(n=sum(na.rm = T,numberes_affected)))$n,
                                  colors = c("green","yellow","red") )%>%
      hc_tooltip(pointFormat = paste('{point.y} aid workers<br/>This is <b>{point.percentage:.0f}%</b> of the total'))%>% hc_add_theme(hc_theme_ffx())
  })
  
  
  #valuebox server====
  output$killed = renderInfoBox({
    infoBox("Killed",tags$h3((workplaceSecurity %>%
                         filter(year==input$year,Country == input$country) %>% group_by(effect) %>%summarise(n=sum(na.rm = T,numberes_affected)))[3,2], icon = icon("list")),
             color = "red")
  })

  output$kidnapped <- renderInfoBox({
    infoBox(
      "Kidnapped",tags$h3((workplaceSecurity %>%
                     filter(year==input$year,Country == input$country) %>% group_by(effect) %>%summarise(n=sum(na.rm = T,numberes_affected)))[2,2]), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  output$arrested = renderInfoBox({
    infoBox("Arrested",tags$h3((workplaceSecurity %>%
                  filter(year==input$year,Country == input$country) %>% group_by(effect) %>%summarise(n=sum(na.rm = T,numberes_affected)))[1,2]),color = "green")
  })
  #Messages dropdown menu.====
  output$v <- renderMenu({
    
    dropdownMenu(type = "messages",
                 messageItem(
                   from = "Designed By",
                   message = "George Oduor."
                 ),
                 messageItem(
                   from = "Contact",
                   message = "0711894704",
                   icon = icon("contact")
                 ),
                 messageItem(
                   from = "Hire me",
                   message = "I can help you solve a data science problem.",
                   icon = icon("life-ring"),
                   time = Sys.time()
                 )
    )
  })
  
  output$notification = renderMenu({
    dropdownMenu(type = "notifications",headerText = "MY AREAS OF COMPETENCE",
                 notificationItem(
                   text = "MACHINE LEARNING",
                   icon("users")
                 ),
                 notificationItem(
                   text = "DATA ANALYSIS",
                   icon("truck"),
                   status = "success"
                 ),
                 notificationItem(
                   text = "DATA VISUALIZATION",
                   icon = icon("exclamation-triangle"),
                   status = "warning"
                 )
    )
  })
  
  output$myskils = renderMenu({
    
    dropdownMenu(type = "tasks", badgeStatus = "success",headerText = "MY SKILLS",
                 taskItem(value = 95, color = "green",
                          "R Programming"
                 ),
                 taskItem(value = 94, color = "aqua",
                          "Python"
                 ),
                 taskItem(value = 91, color = "yellow",
                          "SPSS"
                 ),
                 taskItem(value = 90, color = "red",
                          "STATA "
                 ),
                 taskItem(value = 90, color = "red",
                          "ADVANCED EXCEL "
                 ),
                 taskItem(value = 70, color = "red",
                          "POWER BI "
                 ),
                 taskItem(value = 65, color = "red",
                          "TABLEAU "
                 )
    )
  })
  
  
}



shinyApp(ui, server)