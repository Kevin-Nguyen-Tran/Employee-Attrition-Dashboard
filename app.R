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

#--------------------------------------------------------------------------------------------------------------------------------

ui <- fluidPage(
  selectInput(inputId = "varix", label = "Choose X Variable", "Distance" = "employee_data[ ,4]", "Age" = "employee_data[ ,1]"),
  selectInput(inputId = "variy", label = "Choose Y Variable", "Distance" = "employee_data[ ,4]", "Age" = "employee_data[ ,1]"),
  plotOutput(outputId = "scatter")
)

server <- function(input, output){
  output$scatter <- renderPlot({ggplot(data = employee_data, aes(x = input$varix, y = input$variy)) +
      geom_point()})
  
}

shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------------------------

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









