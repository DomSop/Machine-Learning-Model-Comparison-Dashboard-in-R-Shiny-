Machine Learning Model Comparison Dashboard in R (Shiny)
This project implements a Shiny Dashboard to compare different machine learning models applied to the Sonar dataset from the mlbench package.
It includes Decision Tree, Random Forest, and a simple Deep Learning model. The dashboard provides data exploration, descriptive statistics, and model performance comparison.

Features
📊 Dataset Summary: View a structured summary of the dataset.
📈 Descriptive Statistics: Interactive plots showing mean, standard deviation, min, and max values for each variable.
🏆 Model Performance: Accuracy comparison of Decision Tree, Random Forest, and Deep Learning models.
Models Implemented
Decision Tree (rpart package)
Random Forest (randomForest package)
Simple Deep Learning Model (implemented manually with a sigmoid activation function)

How to Run
Make sure you have the required libraries installed:

install.packages(c("shiny", "ggplot2", "lattice", "DT", "dplyr", "tidyr", "shinydashboard", "mlbench", "rpart", "randomForest"))

shiny::runApp("app.R")
