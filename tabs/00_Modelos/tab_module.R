# Front page module

# UI function
tab_0_ui <- function(id) {
  
  # Basically not needed. Just kept here to preserve commonality across files.
  ns <- NS(id)
  
  # Main UI
  
  fluidPage(
    
    fluidRow(
      
      box(width = 3, textInput(ns("dat1"), 
                               value = "GermanCredit.csv", 
                               label = "Introduzca el nombre del dataset")),
      box(width = 3, textInput(ns("dat2"), 
                               value = "credit_risk", 
                               label = "Introduzca el nombre de la target")),
      box(width = 6, checkboxGroupInput(ns("vars"), "Columnas en dataset:",
                                        names(df), inline = T))
      
    ), # end: fluidRow
    
    fluidRow(
      box(width = 3, numericInput(ns("test1"), min = 0, max = 1, value =0.7, label = "Elija proporcion test - training")),        
      box(width = 3, selectInput(ns("sel1"), choices = c("Logistica", "Random Forest", "Decision Tree", "Naive Bayes"), label = "Elija su modelo"))
      
    ), # end: fluidRow
    fluidRow(
      actionButton(ns("SubmitButton1"), "Genera predicciones")
      
    ), # end: fluidRow
    
    #Este fluidRow se puede dejar para dibujar resultados
    
    fluidRow(
      
      box(title = "Lift en el primer decil",
          width = 3, 
          verbatimTextOutput(ns("Lift10")))
    )
    
  ) # end: fluidPage
  
} # end: tab_0_ui()

# Server function
tab_0_server <- function(input, output, session) {
  
  CalculaLift <- eventReactive(input$SubmitButton1,{
    
    selected_vars <- input$vars
    df2 <- df %>% select(all_of(selected_vars))
    
    #Se ponen todos los missing a 0
    df2[is.na(df2)] <- 0
    
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
          
        }else{
          
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
      
      return(lift10)
      
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
      
      return(lift10)
    }
    
    else if (input$sel1 == "Decision Tree") {
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
      
      return(lift10)
    }
    
    else if (input$sel1 == "Naive Bayes") {
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
      
      return(lift10)
    }
  })
  
  output$Lift10 <- renderText({CalculaLift()})
  
} # end: tab_0_server()
                              