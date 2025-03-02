
library(shiny)
library(ggplot2)
library(lattice)
library(DT)
library(dplyr)
library(tidyr)
library(shinydashboard)
library(mlbench)
library(rpart)
library(randomForest)


# Data set 

data(Sonar)
set.seed(123)

# VAriable convert into a factor 
Sonar$Class <- as.factor(Sonar$Class)

# training and test Data 
trainIndex <- sample(1:nrow(Sonar), 0.7 * nrow(Sonar))
trainData <- Sonar[trainIndex, ]
testData <- Sonar[-trainIndex, ]

# Decision Tree model 
dt_model <- rpart(Class ~ ., data = trainData, method = "class")
dt_pred <- predict(dt_model, testData, type = "class")
dt_accuracy <- sum(dt_pred == testData$Class) / nrow(testData)

#  Random Forest model 
rf_model <- randomForest(Class ~ ., data = trainData, ntree = 100)
rf_pred <- predict(rf_model, testData)
rf_accuracy <- sum(rf_pred == testData$Class) / nrow(testData)

# Deep Learning loop 
set.seed(123)
w <- matrix(runif(ncol(trainData) - 1), nrow = ncol(trainData) - 1)
b <- runif(1)

sigmoid <- function(x) { 1 / (1 + exp(-x)) }

for (epoch in 1:1000) {
  z <- as.matrix(trainData[, -ncol(trainData)]) %*% w + b
  y_hat <- sigmoid(z)
  
  grad_w <- t(as.matrix(trainData[, -ncol(trainData)])) %*% (y_hat - (as.numeric(trainData$Class) - 1))
  grad_b <- sum(y_hat - (as.numeric(trainData$Class) - 1))
  
  w <- w - 0.01 * grad_w
  b <- b - 0.01 * grad_b
}

nn_pred <- sigmoid(as.matrix(testData[, -ncol(testData)]) %*% w + b)
nn_pred <- ifelse(nn_pred > 0.5, "R", "M")
nn_accuracy <- sum(nn_pred == testData$Class) / nrow(testData)

# vs of models 
model_comparison <- data.frame(
  Model = c("Decision Tree", "Random Forest", "Deep Learning"),
  Accuracy = c(dt_accuracy, rf_accuracy, nn_accuracy)
)

# 
descriptive_stats <- Sonar %>% select(-Class) %>% 
  summarise_all(list(mean = mean, sd = sd, min = min, max = max)) %>% 
  pivot_longer(cols = everything(), names_to = c("Variable", "Statistic"), names_sep = "_")

#  UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Machine Learning Model Comparison"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary Statistics", tabName = "summary", icon = icon("table")),
      menuItem("Descriptive Statistics", tabName = "descriptive", icon = icon("chart-bar")),
      menuItem("Model Performance", tabName = "performance", icon = icon("chart-line"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Dataset Summary", width = 12, status = "primary", solidHeader = TRUE,
                    DTOutput("summaryTable"))
              )
      ),
      tabItem(tabName = "descriptive",
              fluidRow(
                box(title = "Descriptive Statistics", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("statType", "Choose Statistic:", choices = unique(descriptive_stats$Statistic), selected = "mean"),
                    selectInput("varType", "Choose Variable:", choices = unique(descriptive_stats$Variable), selected = "V1"),
                    plotOutput("descStatsPlot"),
                    plotOutput("scatterPlot"))
              )
      ),
      tabItem(tabName = "performance",
              fluidRow(
                box(title = "Model Accuracy Comparison", width = 12, status = "primary", solidHeader = TRUE,
                    plotOutput("accuracyPlot"))
              )
      )
    )
  )
)

# Server definition
server <- function(input, output) {
  output$summaryTable <- renderDT({
    datatable(summary(Sonar), options = list(pageLength = 5))
  })
  
  output$descStatsPlot <- renderPlot({
    filtered_stats <- descriptive_stats %>%
      filter(Statistic == input$statType)
    
    ggplot(filtered_stats, aes(x = Variable, y = value, fill = Statistic)) +
      geom_bar(stat = "identity", position = "dodge") +
      theme_minimal() +
      labs(title = "Descriptive Statistics", y = "Value", x = "Variable") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
  
  output$scatterPlot <- renderPlot({
    filtered_data <- Sonar %>%
      select(input$varType, Class)
    
    ggplot(filtered_data, aes(x = Class, y = .data[[input$varType]], color = Class)) +
      geom_jitter(width = 0.2, alpha = 0.5) +
      theme_minimal() +
      labs(title = "Scatter Plot", y = input$varType, x = "Class")
  })
  
  output$accuracyPlot <- renderPlot({
    ggplot(model_comparison, aes(x = Model, y = Accuracy, fill = Model)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      labs(title = "Model Accuracy Comparison", y = "Accuracy", x = "Model")
  })
}

# Dashboard 
shinyApp(ui = ui, server = server)

