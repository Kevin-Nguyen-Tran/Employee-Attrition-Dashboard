rm(list = ls())
library(shiny)
library(tidyverse)
library(wesanderson)
library(shinydashboard)

data <- read.csv("C:/Users/Kevin/Desktop/data/HR-Employee-Attrition-Dashboard.csv")
employee_data <- as.tibble(data)

employee_data <- rename(employee_data, Age = ï..Age)

employee_data2 <- employee_data %>%
  select(Age, HourlyRate)

employee_data3 <- top_n(employee_data, 50)

raw_data4 <- employee_data3 %>%
  select(Age, HourlyRate, BusinessTravel, JobSatisfaction)

#--------------------------------------------------------------------------------------------------------------------------------
#DIDN'T WORK WILL NOT USE IN NAVBAR
#--------------------------------------------------------------------------------------------------------------------------------

#ui <- fluidPage(
 # selectInput(inputId = "varix", label = "Choose X Variable", "Distance" = "employee_data[ ,4]", "Age" = "employee_data[ ,1]"),
  #selectInput(inputId = "variy", label = "Choose Y Variable", "Distance" = "employee_data[ ,4]", "Age" = "employee_data[ ,1]"),
  #plotOutput(outputId = "scatter")
#)

#server <- function(input, output){
 # output$scatter <- renderPlot({ggplot(data = employee_data, aes(x = input$varix, y = input$variy)) +
  #    geom_point()})
  
#}

#shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------------------------
# Age vs hourly rate
#--------------------------------------------------------------------------------------------------------------------------------

#ui <- fluidPage(
 #   radioButtons(inputId = "plotType", label = "Plot Type:",choices = c("Scatter" = "p", "Line" = "l")),
  #  plotOutput(outputId = "plot")
  #)  

#server <- function(input, output){
 # output$plot <- renderPlot({
  #  if(input$plotType == "p"){
   #   ggplot(employee_data3, aes(x = Age, y = HourlyRate)) +
    #    geom_point() +
     #   geom_smooth(se = FALSE) +
      #  labs(x = "Age", 
       #      y = "Hourly Rate",
        #     title = "Is Salary Affected by Age?",
         #    caption = "Source: IBM HR Analytics") +
      #  theme(plot.title = element_text(hjust = 0.5),
       #       plot.subtitle = element_text(hjust = 0.5))
  #  } else {
   #   ggplot(employee_data3, aes(x = Age, y = HourlyRate)) +
    #    geom_line() +
     #   labs(x = "Age", 
      #       y = "Hourly Rate",
       #      title = "Is Salary Affected by Age?",
        #     caption = "Source: IBM HR Analytics") +
        #theme(plot.title = element_text(hjust = 0.5),
         #     plot.subtitle = element_text(hjust = 0.5))
    #}
  #})
  
#}

#shinyApp(ui = ui, server = server)


#--------------------------------------------------------------------------------------------------------------------------------
# Age data
#--------------------------------------------------------------------------------------------------------------------------------

#ui <- fluidPage(
 # radioButtons(inputId = "plotType", label = "Plot Type:", choices = c("Box Plot" = "box", "Bar Chart" = "bar")),
  #plotOutput(outputId = "plot")
#)

#server <- function(input, output){
 # output$plot <- renderPlot({
  #  if(input$plotType == "box"){
   #   boxplot(employee_data3$Age,
    #          xlab = "Employees",
     #         ylab = "Age (averages")
    #} else {
     # barplot(employee_data3$Age,
      #        xlab = "Employee",
       #       ylab = "Age")
    #}
  #})
#}

#shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------------------------
# Business Travel vs Job Satisfaction
#--------------------------------------------------------------------------------------------------------------------------------

#ui <- fluidPage(
 # radioButtons(inputId = "plotType", label = "Plot Type:", choices = c("Box Plot" = "box", "Violin Plot" = "viola")),
 # plotOutput(outputId = "plot")
#)

#server <- function(input, output){
 # output$plot <- renderPlot({
  #  if(input$plotType == "box"){
   #   ggplot(employee_data3, aes(x = BusinessTravel, y = JobSatisfaction)) +
     #   geom_boxplot(fill = wes_palette("GrandBudapest2", n = 3)) +
   #     labs(x = "Business Travel", 
          #   y = "Job Satisfaction",
        #     title = "Satisfaction Level in Regards to Travel",
       #      caption = "Source: IBM HR Analytics") +
   #     theme(plot.title = element_text(hjust = 0.5),
       #       plot.subtitle = element_text(hjust = 0.5))
   # } else {
    #  ggplot(employee_data3, aes(x = BusinessTravel, y = JobSatisfaction)) +
    #    geom_violin(fill = wes_palette("GrandBudapest1", n = 1)) +
     #   labs(x = "Business Travel", 
        #     y = "Job Satisfaction",
         #    title = "Satisfaction Level in Regards to Travel",
        #     caption = "Source: IBM HR Analytics") +
       # theme(plot.title = element_text(hjust = 0.5),
             # plot.subtitle = element_text(hjust = 0.5))
  #  }
#  })
#}

#shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------------------------
#NAVBAR FOR THE ABOVE 3 SHINY APPS
#--------------------------------------------------------------------------------------------------------------------------------

#ui <- navbarPage(title = "Employee Attrition EDA",
           #tabPanel("Age Data",
           #         radioButtons(inputId = "plotType1", label = "Plot Type:", choices = c("Box Plot" = "box", "Bar Chart" = "bar")),
           #         plotOutput(outputId = "plot1"),
            #        verbatimTextOutput(outputId = "sum1")
          #          ),
          # tabPanel("Age vs Hourly Rate",
                   # radioButtons(inputId = "plotType2", label = "Plot Type:",choices = c("Scatter" = "p", "Line" = "l")),
                  #  plotOutput(outputId = "plot2"),
                 #   tableOutput(outputId = "sum2")
                #    ),
         #  tabPanel("Business Travel and Job Satisfaction",
                #    radioButtons(inputId = "plotType3", label = "Plot Type:", choices = c("Box Plot" = "box", "Violin Plot" = "viola")),
                #    plotOutput(outputId = "plot3"),
                 #   tableOutput(outputId = "sum3")
                 #   )
         #  )

#server <- function(input, output){
 # output$plot1 <- renderPlot({
   # if(input$plotType1 == "box"){
    #  boxplot(employee_data3$Age,
             # xlab = "Employees",
            #  ylab = "Age (averages")
   # } else {
     # barplot(employee_data3$Age,
           #   xlab = "Employee",
           #   ylab = "Age")
  #  }
 # })
 # output$sum1 <- renderPrint({
   # if(input$plotType1 == "box"){
   #   summary(employee_data3$Age)
  #  } else {
  #    summary(employee_data3$Age)
 #   }
#  })
  #code above is for tab 1
  
 # output$plot2 <- renderPlot({
    #if(input$plotType2 == "p"){
     # ggplot(employee_data3, aes(x = Age, y = HourlyRate)) +
      #  geom_point() +
       # geom_smooth(se = FALSE) +
       # labs(x = "Age", 
         #    y = "Hourly Rate",
          #   title = "Is Salary Affected by Age?",
          #   caption = "Source: IBM HR Analytics") +
        #theme(plot.title = element_text(hjust = 0.5),
             # plot.subtitle = element_text(hjust = 0.5))
  #  } else {
     # ggplot(employee_data3, aes(x = Age, y = HourlyRate)) +
      #  geom_line() +
     #   labs(x = "Age", 
        #     y = "Hourly Rate",
            # title = "Is Salary Affected by Age?",
        #     caption = "Source: IBM HR Analytics") +
       # theme(plot.title = element_text(hjust = 0.5),
         #     plot.subtitle = element_text(hjust = 0.5))
   # }
 # })
  
#  output$sum2 <- renderTable({
   # if(input$plotType1 == "p"){
    #  employee_data3 %>% 
     #   select(Age, HourlyRate) %>% 
      #  summarise(across(everything(), list(min, median, mean, max))) %>%
    #    rename(Age.Min = Age_1, Age.Median = Age_2 , Age.Mean = Age_3 , Age.Max = Age_4, 
    #           HourlyRate.Min = HourlyRate_1, HourlyRate.Median = HourlyRate_2, HourlyRate.Mean = HourlyRate_3, HourlyRate.Max = HourlyRate_4)
   # } else {
    #  employee_data3 %>% 
      #  select(Age, HourlyRate) %>% 
     #   summarise(across(everything(), list(min, median, mean, max))) %>%
      #  rename(Age.Min = Age_1, Age.Median = Age_2 , Age.Mean = Age_3 , Age.Max = Age_4, 
         #      HourlyRate.Min = HourlyRate_1, HourlyRate.Median = HourlyRate_2, HourlyRate.Mean = HourlyRate_3, HourlyRate.Max = HourlyRate_4)    }
 # })
  
  #code above is for tab 2
  
  #output$plot3 <- renderPlot({
    #if(input$plotType3 == "box"){
    #  ggplot(employee_data3, aes(x = BusinessTravel, y = JobSatisfaction)) +
     #   geom_boxplot(fill = wes_palette("GrandBudapest2", n = 3)) +
     #   labs(x = "Business Travel", 
        #     y = "Job Satisfaction",
           #  title = "Satisfaction Level in Regards to Travel",
         #    caption = "Source: IBM HR Analytics") +
        #theme(plot.title = element_text(hjust = 0.5),
        #      plot.subtitle = element_text(hjust = 0.5))
    #} else {
    #  ggplot(employee_data3, aes(x = BusinessTravel, y = JobSatisfaction)) +
     #   #geom_violin(fill = wes_palette("GrandBudapest1", n = 1)) +
        #labs(x = "Business Travel", 
           #  y = "Job Satisfaction",
            # title = "Satisfaction Level in Regards to Travel",
            # caption = "Source: IBM HR Analytics") +
      #  theme(plot.title = element_text(hjust = 0.5),
            #  plot.subtitle = element_text(hjust = 0.5))
   # }
 # })
#output$sum3 <- renderTable({
 # if(input$plotType3 == "box"){
   # employee_data3 %>% 
     # group_by(BusinessTravel) %>% 
     # summarise(JobSatisfaction.min = min(JobSatisfaction), JobSatisfaction.median = median(JobSatisfaction), JobSatisfaction.mean = mean(JobSatisfaction), JobSatisfaction.max = max(JobSatisfaction))
 # } else {
  #  employee_data3 %>% 
     # group_by(BusinessTravel) %>% 
      #summarise(JobSatisfaction.min = min(JobSatisfaction), JobSatisfaction.median = median(JobSatisfaction), JobSatisfaction.mean = mean(JobSatisfaction), JobSatisfaction.max = max(JobSatisfaction))
#}
#})
 # }

#shinyApp(ui = ui, server = server)


#--------------------------------------------------------------------------------------------------------------------------------
#SHINYDASHBOARD!
#--------------------------------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Employee Attrition EDA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboards", icon = icon("dashboard")),
        menuSubItem("Age Data", tabName = "dashboard1"),
        menuSubItem("Age vs Hourly Rate", tabName = "dashboard2"),
        menuSubItem("Business Travel and Job Satisfaction Dashboard", tabName = "dashboard3"),
      menuItem("Raw Data", icon = icon("save"), tabName = "rawdata")
  )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard1",
      fluidRow(
      box(radioButtons(inputId = "plotType1", label = "Plot Type:", choices = c("Box Plot" = "box", "Histogram" = "hist")), plotOutput(outputId = "plot1")),
      box(
        title = "Controls for Histogram",
        sliderInput("slider", "Number of Observations:", 0, 50, 50)
      )
      ),
      fluidRow(
      box(verbatimTextOutput(outputId = "sum1"))
    )),
    tabItem(tabName = "dashboard2",
            fluidRow(
              box(radioButtons(inputId = "plotType2", label = "Plot Type:",choices = c("Scatter" = "p", "Line" = "l")), plotOutput(outputId = "plot2"))
            ),
            fluidRow(
              box(tableOutput(outputId = "sum2"))
            )),
    tabItem(tabName = "dashboard3",
            fluidRow(
              box(radioButtons(inputId = "plotType3", label = "Plot Type:", choices = c("Box Plot" = "box", "Violin Plot" = "viola")),
                  plotOutput(outputId = "plot3"))
            ),
            fluidRow(
              box(tableOutput(outputId = "sum3")))),
    tabItem(tabName = "rawdata",
            numericInput("maxrows", "Rows to Show", 50),
            verbatimTextOutput(outputId = "rawdata"),
            downloadButton("downloadCsv", "Download as CSV"))
    )
    )
)

server <- function(input, output) { 
  output$plot1 <- renderPlot({
    if(input$plotType1 == "box"){
      boxplot(employee_data3$Age,
              xlab = "Employees",
              ylab = "Age (averages")
    } else {
      hist(employee_data3$Age,
              xlab = "Age",
              ylab = "Number of Employees",
              breaks = input$slider)
    }
  })
  
  output$sum1 <- renderPrint({
    if(input$plotType1 == "box"){
      summary(employee_data3$Age)
    } else {
      summary(employee_data3$Age)
    }
  }) #Age tab
  
  output$plot2 <- renderPlot({
    if(input$plotType2 == "p"){
      ggplot(employee_data3, aes(x = Age, y = HourlyRate)) +
        geom_point() +
        geom_smooth(se = FALSE) +
        labs(x = "Age", 
             y = "Hourly Rate",
             title = "Is Salary Affected by Age?",
             caption = "Source: IBM HR Analytics") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    } else {
      ggplot(employee_data3, aes(x = Age, y = HourlyRate)) +
        geom_line() +
        labs(x = "Age", 
             y = "Hourly Rate",
             title = "Is Salary Affected by Age?",
             caption = "Source: IBM HR Analytics") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
  })
  
  output$sum2 <- renderTable({
    if(input$plotType1 == "p"){
      employee_data3 %>% 
        select(Age, HourlyRate) %>% 
        summarise(across(everything(), list(min, median, mean, max))) %>%
        rename(Age.Min = Age_1, Age.Median = Age_2 , Age.Mean = Age_3 , Age.Max = Age_4, 
               HourlyRate.Min = HourlyRate_1, HourlyRate.Median = HourlyRate_2, HourlyRate.Mean = HourlyRate_3, HourlyRate.Max = HourlyRate_4)
    } else {
      employee_data3 %>% 
        select(Age, HourlyRate) %>% 
        summarise(across(everything(), list(min, median, mean, max))) %>%
        rename(Age.Min = Age_1, Age.Median = Age_2 , Age.Mean = Age_3 , Age.Max = Age_4, 
               HourlyRate.Min = HourlyRate_1, HourlyRate.Median = HourlyRate_2, HourlyRate.Mean = HourlyRate_3, HourlyRate.Max = HourlyRate_4)    }
  })
  
  output$plot3 <- renderPlot({
    if(input$plotType3 == "box"){
      ggplot(employee_data3, aes(x = BusinessTravel, y = JobSatisfaction)) +
        geom_boxplot(fill = wes_palette("GrandBudapest2", n = 3)) +
        labs(x = "Business Travel", 
             y = "Job Satisfaction",
             title = "Satisfaction Level in Regards to Travel",
             caption = "Source: IBM HR Analytics") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    } else {
      ggplot(employee_data3, aes(x = BusinessTravel, y = JobSatisfaction)) +
        geom_violin(fill = wes_palette("GrandBudapest1", n = 1)) +
        labs(x = "Business Travel", 
             y = "Job Satisfaction",
             title = "Satisfaction Level in Regards to Travel",
             caption = "Source: IBM HR Analytics") +
        theme(plot.title = element_text(hjust = 0.5),
              plot.subtitle = element_text(hjust = 0.5))
    }
  })
  output$sum3 <- renderTable({
    if(input$plotType3 == "box"){
      employee_data3 %>% 
        group_by(BusinessTravel) %>% 
        summarise(JobSatisfaction.min = min(JobSatisfaction), JobSatisfaction.median = median(JobSatisfaction), JobSatisfaction.mean = mean(JobSatisfaction), JobSatisfaction.max = max(JobSatisfaction))
    } else {
      employee_data3 %>% 
        group_by(BusinessTravel) %>% 
        summarise(JobSatisfaction.min = min(JobSatisfaction), JobSatisfaction.median = median(JobSatisfaction), JobSatisfaction.mean = mean(JobSatisfaction), JobSatisfaction.max = max(JobSatisfaction))
    }
  })
  
  output$rawdata <- renderPrint({
    orig <- options(width = 8000)
    print(tail(x = raw_data4, n = input$maxrows), row.names = FALSE)
    options(orig)
  })
  
  output$downloadCsv <- downloadHandler(
    filename = "raw_data4.csv",
    content = function(file) {
      write.csv(raw_data4, file)
    },
    contentType = "text/csv"
  )
  
  }

shinyApp(ui, server)














