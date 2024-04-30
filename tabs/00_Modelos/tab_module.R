# Front page module
library(pROC)
library(plotly)
library(dplyr)

# UI function
tab_0_ui <- function(id) {
  ns <- NS(id)  # Namespace function for UI id encapsulation

  # Main UI layout using fluidPage
  fluidPage(
    fluidRow(
      box(title = "Data Input", status = "primary", solidHeader = TRUE, width = 6,
          textInput(ns("dat1"), value = "GermanCredit.csv", label = "Introduzca el nombre del dataset"),
          textInput(ns("dat2"), value = "credit_risk", label = "Introduzca el nombre de la target"),
          checkboxGroupInput(ns("vars"), "Columnas en dataset:", names(df), inline = TRUE),
          fluidRow(
            column(width = 6, numericInput(ns("test1"), "Elija proporcion test - training", min = 0, max = 1, value = 0.7)),
            column(width = 6, selectInput(ns("sel1"), "Elija su modelo", choices = c("Logistica", "Random Forest", "Decision Tree", "Naive Bayes")))
          ),
          actionButton(ns("SubmitButton1"), "Genera predicciones")
      ),
      box(title = "Lift en el primer decil", status = "info", solidHeader = TRUE, width = 6,
        verbatimTextOutput(ns("Lift10")),
        plotlyOutput(ns("specificModelPlot"))
      )
    ),
  )
}


# Server function
tab_0_server <- function(input, output, session) {
  CalculaLift <- eventReactive(input$SubmitButton1, {
    selected_vars <- input$vars
    df2 <- df %>% select(all_of(selected_vars))
    df2[is.na(df2)] <- 0  # Handling missing values by setting to 0

    showNotification("Ejecutando modelo", duration = 5, type = "message")

    if (input$sel1 == "Logistica"){
      
      set.seed(1)
      index <- sample(c(1:nrow(df2)), input$test1 * nrow(df2))
      
      df_train <- df2[index,]
      df_test <- df2[-index,] 
      
      suma = names(df2)[1]
      indice_target <- which(names(df2) == input$dat2) 
      
      i = 1
      
      while (i < length(names(df2 %>% select(-all_of(indice_target))))){
        
        if (i < length(names(df2 %>% select(-all_of(indice_target))))){
          
          i = i + 1
          suma = paste0(names(df2)[i], " + ", suma)
          
        } else {
          
          suma = suma
          i = i + 1
          
        }
        
      }
      
      formula = paste0(input$dat2, "~", suma)
      
      mod_log <- glm(formula, data = df_train, family = "binomial")
      
      pred_test <- predict(mod_log, df_test, type="response" )
      
      df_test$prediction = pred_test
      
      index1 <- which(names(df_test) == "credit_risk")
      index2 <- which(names(df_test) == "prediction")
      
      columnas <- c(index1, index2)
      
      df_test_fin <- df_test %>% select(all_of(columnas))
      
      df_test_fin <- df_test_fin[order(df_test_fin$prediction, decreasing =  T),]
      
      n10 = round((nrow(df_test) / 10), 0)
      lift10 = df_test_fin[c(1: n10),] 
      lift10 = sum(lift10[,1]) / (sum(df_test_fin[,1]) / 10)
      
      return(list(lift = lift10, model = list(mod_log = mod_log, df_test_fin = df_test_fin)))
    } else if (input$sel1 == "Random Forest") {
      
      library(randomForest)
      
      set.seed(1)
      index <- sample(c(1:nrow(df2)), input$test1 * nrow(df2))
      
      df_train <- df2[index,]
      df_test <- df2[-index,] 
      
      target <- input$dat2
      formula_rf <- as.formula(paste(target, "~ ."))
      
      mod_rf <- randomForest(formula_rf, data=df_train, ntree=100)
      
      pred_test_rf <- predict(mod_rf, df_test, type="response")
      
      # Convertir las predicciones a numérico, en caso de ser necesario
      df_test$prediction <- as.numeric(pred_test_rf)
      
      index1 <- which(names(df_test) == target)
      index2 <- which(names(df_test) == "prediction")
      
      columnas <- c(index1, index2)
      
      df_test_fin <- df_test %>% select(all_of(columnas))
      
      # Ordenar el dataset en función de la probabilidad
      df_test_fin <- df_test_fin[order(df_test_fin$prediction, decreasing = TRUE),]
      
      # Cálculo del lift en el primer decil
      n10 = round((nrow(df_test) / 10), 0)
      lift10 = df_test_fin[c(1:n10),] 
      lift10 = sum(lift10[,1]) / (sum(df_test_fin[,1]) / 10)
      
      return(list(lift = lift10, model = list(mod_rf = mod_rf, df_test_fin = df_test_fin)))
    } else if (input$sel1 == "Decision Tree") {
      library(rpart)
      
      set.seed(1)
      index <- sample(c(1:nrow(df2)), input$test1 * nrow(df2))
      
      df_train <- df2[index,]
      df_test <- df2[-index,]
      
      formula_dt <- as.formula(paste(input$dat2, "~ ."))
      
      mod_dt <- rpart(formula_dt, data=df_train, method="class")
      pred_test_dt <- predict(mod_dt, df_test, type="prob")[,2]
      
      df_test$prediction <- pred_test_dt
      
      index1 <- which(names(df_test) == input$dat2)
      index2 <- which(names(df_test) == "prediction")
      
      columnas <- c(index1, index2)
      
      df_test_fin <- df_test %>% select(all_of(columnas))
      
      df_test_fin <- df_test_fin[order(df_test_fin$prediction, decreasing = TRUE),]
      
      n10 = round((nrow(df_test) / 10), 0)
      lift10 = df_test_fin[c(1:n10),]
      lift10 = sum(lift10[,1]) / (sum(df_test_fin[,1]) / 10)
      
      return(list(lift = lift10, model = list(mod_dt = mod_dt, df_test_fin = df_test_fin)))
    } else if (input$sel1 == "Naive Bayes") {
      library(e1071)
      
      set.seed(1)
      index <- sample(c(1:nrow(df2)), input$test1 * nrow(df2))
      
      df_train <- df2[index,]
      df_test <- df2[-index,]
      
      # Asegurarse de que no haya valores NA en los datos de entrenamiento y prueba
      df_train <- na.omit(df_train)
      df_test <- na.omit(df_test)
      
      formula_nb <- as.formula(paste(input$dat2, "~ ."))
      
      # Entrenamiento del modelo Naive Bayes
      mod_nb <- naiveBayes(formula_nb, data=df_train)
      
      # Predicciones con el modelo Naive Bayes
      pred_test_nb <- predict(mod_nb, df_test, type = "raw")
      
      # Asumiendo que quieres la probabilidad de la clase positiva
      df_test$prediction <- pred_test_nb[,2]
      
      index1 <- which(names(df_test) == input$dat2)
      index2 <- which(names(df_test) == "prediction")
      
      columnas <- c(index1, index2)
      
      df_test_fin <- df_test %>% select(all_of(columnas))
      
      df_test_fin <- df_test_fin[order(df_test_fin$prediction, decreasing = TRUE),]
      
      n10 = round((nrow(df_test) / 10), 0)
      lift10 = df_test_fin[c(1:n10),]
      lift10 = sum(lift10[,1]) / (sum(df_test_fin[,1]) / 10)
      
      return(list(lift = lift10, model = list(mod_nb = mod_nb, df_test_fin = df_test_fin)))
    }
  })

  output$Lift10 <- renderText({
    CalculaLift()$lift
  })

  output$specificModelPlot <- renderPlotly({
    req(CalculaLift()$model)
    results <- CalculaLift()$model
    if (input$sel1 == "Logistica") {
      roc_curve <- roc(results$df_test_fin[[input$dat2]], results$df_test_fin$prediction)

      # Create a data frame with the false positive rate (1 - Specificity) and the true positive rate (Sensitivity)
      roc_data <- data.frame(
        FPR = 1 - roc_curve$specificities,  # False Positive Rate
        TPR = roc_curve$sensitivities       # True Positive Rate
      )

      # Generate the plot with plotly
      interactive_roc <- plot_ly(data = roc_data, x = ~FPR, y = ~TPR, type = 'scatter', mode = 'lines',
                                line = list(color = 'rgba(205, 12, 24, 0.8)', width = 2)) %>%
        layout(title = "Curva ROC para Regresión Logística",
              xaxis = list(title = "1 - Especificidad", zeroline = FALSE),
              yaxis = list(title = "Sensibilidad", zeroline = FALSE),
              hovermode = 'closest')

      # Optionally, add the diagonal line that represents the "no-skill" classifier
      interactive_roc <- add_trace(interactive_roc, x = c(0, 1), y = c(0, 1), mode = "lines", 
                                  line = list(color = 'navy', dash = 'dash'), 
                                  showlegend = FALSE)

      return(interactive_roc)
    } else if (input$sel1 == "Random Forest") {
      varImpPlot(results$mod_rf, main="Importancia de las Variables para Random Forest")
    } else if (input$sel1 == "Decision Tree") {
      rpart.plot::rpart.plot(results$mod_dt, main="Diagrama del Árbol de Decisión")
    } else if (input$sel1 == "Naive Bayes") {
      df_test_fin <- results$df_test_fin
      ggplot(df_test_fin, aes(x = prediction)) +
        geom_density(aes(fill = factor(df_test_fin[[input$dat2]])), alpha = 0.5) +
        labs(title = "Densidades de Probabilidad para Naive Bayes", x = "Probabilidad", y = "Densidad")
    }
  })
}
