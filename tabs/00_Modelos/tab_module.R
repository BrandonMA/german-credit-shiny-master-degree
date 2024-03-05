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
      box(width = 3, selectInput(ns("sel1"), choices = c("Logistica", "Random Forest"), label = "Elija su modelo"))

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
      #index <- sample(c(1:nrow(df)), 0.7 * nrow(df))
      
      df_train <- df2[index,]
      df_test <- df2[-index,] 
      
      #Se crea la fórmula con la target y las variables del modelo
      
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
      summary(mod_log)
      
      pred_test <- predict(mod_log, df_test, type="response" )
      
      df_test$prediction = pred_test
      
      index1 <- which(names(df_test) == "credit_risk")
      index2 <- which(names(df_test) == "prediction")
      
      columnas <- c(index1, index2)
      
      df_test_fin <- df_test %>% select(all_of(columnas))
      
      #Se ordena el dataset en frunción de la probabilidad
      
      df_test_fin <- df_test_fin[order(df_test_fin$prediction, decreasing =  T),]
      
      #Cálculo de la lift en el primer decil
      
      n10 = round((nrow(df_test) / 10), 0)
      lift10 = df_test_fin[c(1: n10),] 
      lift10 = sum(lift10[,1]) / (sum(df_test_fin[,1]) / 10)
      
      print(lift10)
     
    }
    
    
  })
  
  output$Lift10 <- renderText({CalculaLift()})
  
} # end: tab_0_server()
