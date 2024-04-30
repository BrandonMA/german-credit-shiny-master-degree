library(shiny)
library(dplyr)
library(ggplot2)
library(randomForest)
library(rpart)
library(e1071)
library(pROC)  # Asegúrate de que pROC está instalado y cargado correctamente

# Front page module

# UI function
tab_0_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      box(width = 3, textInput(ns("dat1"), value = "GermanCredit.csv", label = "Introduzca el nombre del dataset")),
      box(width = 3, textInput(ns("dat2"), value = "credit_risk", label = "Introduzca el nombre de la target")),
      box(width = 6, checkboxGroupInput(ns("vars"), "Columnas en dataset", choices = names(df), inline = TRUE))
    ),
    fluidRow(
      box(width = 3, numericInput(ns("test1"), min = 0, max = 1, value = 0.7, label = "Elija proporción test - training")),
      box(width = 3, selectInput(ns("sel1"), choices = c("Logistica", "Random Forest", "Decision Tree", "Naive Bayes"), label = "Elija su modelo"))
    ),
    fluidRow(
      actionButton(ns("SubmitButton1"), "Genera predicciones")
    ),
    fluidRow(
      box(title = "Lift en el primer decil", width = 3, verbatimTextOutput(ns("Lift10"))),
      box(title = "Probabilidad de Predicción", status = "primary", solidHeader = TRUE, width = 12, plotOutput(ns("modelPlot")))
    ),
    fluidRow(
      box(title = "Gráfico Específico del Modelo", status = "primary", solidHeader = TRUE, width = 12, plotOutput(ns("specificModelPlot")))
    )
  )
}

# Server function
tab_0_server <- function(input, output, session) {
  results <- reactiveValues(df_test_fin = NULL, model = NULL)
  
  CalculaLift <- eventReactive(input$SubmitButton1, {
    selected_vars <- input$vars
    df2 <- df %>% select(all_of(selected_vars))
    df2[is.na(df2)] <- 0
    
    set.seed(1)
    index <- sample(c(1:nrow(df2)), input$test1 * nrow(df2))
    df_train <- df2[index,]
    df_test <- df2[-index,]
    
    if (input$sel1 == "Logistica") {
      formula = as.formula(paste(input$dat2, "~", paste(names(df_train)[-which(names(df_train) == input$dat2)], collapse = " + ")))
      results$model <- glm(formula, data = df_train, family = "binomial")
    } else if (input$sel1 == "Random Forest") {
      formula_rf <- as.formula(paste(input$dat2, "~ ."))
      results$model <- randomForest(formula_rf, data=df_train, ntree=100)
    } else if (input$sel1 == "Decision Tree") {
      formula_dt <- as.formula(paste(input$dat2, "~ ."))
      results$model <- rpart(formula_dt, data=df_train, method="class")
    } else if (input$sel1 == "Naive Bayes") {
      results$model <- naiveBayes(df_train[,-which(names(df_train) == input$dat2)], df_train[[input$dat2]])
    }
    
    df_test$prediction <- predict(results$model, df_test, type = ifelse(input$sel1 %in% c("Logistica", "Naive Bayes"), "response", "prob"))
    results$df_test_fin <- df_test %>% arrange(desc(prediction))
    
    lift10 = sum(df_test[1:round(nrow(df_test) / 10), input$dat2]) / (sum(df_test[[input$dat2]]) / 10)
    return(lift10)
  })
  
  output$Lift10 <- renderText({ CalculaLift() })
  
  output$modelPlot <- renderPlot({
    req(results$df_test_fin)
    df_test_fin <- results$df_test_fin
    ggplot(df_test_fin, aes(x = prediction, fill = factor(df_test_fin[[input$dat2]]))) +
      geom_histogram(binwidth = 0.05) +
      labs(title = "Distribución de predicciones", x = "Probabilidad de predicción", fill = "Resultado Real") +
      theme_minimal()
  })
  
  output$specificModelPlot <- renderPlot({
    req(results$model)
    if (input$sel1 == "Logistica") {
      roc_curve <- roc(results$df_test_fin[[input$dat2]], results$df_test_fin$prediction)
      plot(roc_curve, main="Curva ROC para Regresión Logística")  # Utiliza la función plot genérica
    } else if (input$sel1 == "Random Forest") {
      varImpPlot(results$model, main="Importancia de las Variables para Random Forest")
    } else if (input$sel1 == "Decision Tree") {
      rpart.plot::rpart.plot(results$model, main="Diagrama del Árbol de Decisión")
    } else if (input$sel1 == "Naive Bayes") {
      df_test_fin <- results$df_test_fin
      ggplot(df_test_fin, aes(x = prediction)) +
        geom_density(aes(fill = factor(df_test_fin[[input$dat2]])), alpha = 0.5) +
        labs(title = "Densidades de Probabilidad para Naive Bayes", x = "Probabilidad", y = "Densidad")
    }
  })
}

