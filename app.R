rm(list = ls())
library(shiny)
library(tidyverse)
library(wesanderson)

data <- read.csv("C:/Users/Kevin/Desktop/Employee-Attrition-Dashboard/WA_Fn-UseC_-HR-Employee-Attrition.csv")
employee_data <- as.tibble(data)

employee_data <- rename(employee_data, Age = ï..Age)

employee_data2 <- employee_data %>%
  select(Age, HourlyRate)

employee_data3 <- top_n(employee_data, 50)

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

ui <- fluidPage(
    radioButtons(inputId = "plotType", label = "Plot Type:",choices = c("Scatter" = "p", "Line" = "l")),
    plotOutput(outputId = "plot")
  )  

server <- function(input, output){
  output$plot <- renderPlot({
    if(input$plotType == "p"){
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
  
}

shinyApp(ui = ui, server = server)


#--------------------------------------------------------------------------------------------------------------------------------
# Age data
#--------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  radioButtons(inputId = "plotType", label = "Plot Type:", choices = c("Box Plot" = "box", "Bar Chart" = "bar")),
  plotOutput(outputId = "plot")
)

server <- function(input, output){
  output$plot <- renderPlot({
    if(input$plotType == "box"){
      boxplot(employee_data3$Age,
              xlab = "Employees",
              ylab = "Age (averages")
    } else {
      barplot(employee_data3$Age,
              xlab = "Employee",
              ylab = "Age")
    }
  })
}

shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------------------------
# Business Travel vs Job Satisfaction
#--------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  radioButtons(inputId = "plotType", label = "Plot Type:", choices = c("Box Plot" = "box", "Violin Plot" = "viola")),
  plotOutput(outputId = "plot")
)

server <- function(input, output){
  output$plot <- renderPlot({
    if(input$plotType == "box"){
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
}

shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------------------------
#NAVBAR FOR THE ABOVE 3 SHINY APPS
#--------------------------------------------------------------------------------------------------------------------------------

ui <- navbarPage(title = "Employee Attrition EDA",
           tabPanel("Age Data",
                    radioButtons(inputId = "plotType1", label = "Plot Type:", choices = c("Box Plot" = "box", "Bar Chart" = "bar")),
                    plotOutput(outputId = "plot1")
                    ),
           tabPanel("Age vs Hourly Rate",
                    radioButtons(inputId = "plotType2", label = "Plot Type:",choices = c("Scatter" = "p", "Line" = "l")),
                    plotOutput(outputId = "plot2")
                    ),
           tabPanel("Business Travel and Job Satisfaction",
                    radioButtons(inputId = "plotType3", label = "Plot Type:", choices = c("Box Plot" = "box", "Violin Plot" = "viola")),
                    plotOutput(outputId = "plot3")
                    )
           )

server <- function(input, output){
  output$plot1 <- renderPlot({
    if(input$plotType1 == "box"){
      boxplot(employee_data3$Age,
              xlab = "Employees",
              ylab = "Age (averages")
    } else {
      barplot(employee_data3$Age,
              xlab = "Employee",
              ylab = "Age")
    }
  })
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
}

shinyApp(ui = ui, server = server)


