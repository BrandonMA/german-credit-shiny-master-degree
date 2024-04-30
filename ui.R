#http://fontawesome.io/icons/

############################## CREATE THE SIDEBAR ##############################
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("ML Prediction", tabName = "Tab0", icon = icon("info-circle"))

  ),

  # Logo in sidebar menu
  div(style = "position: fixed; bottom: 35px; left: 35px;",
      img(src = 'external_logos/r.jpg', width = 197)
  )
)

############################### CREATE THE BODY ################################
body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "AdminLTE.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "_all-skins.min.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "skin-yellow.min.css")
  ),

  # Write the UI reference of the modules
  tabItems(
    tabItem(tabName = "Tab0", tab_0_ui("tab_0"))
  )
)

#################### PUT THEM TOGETHER INTO A DASHBOARDPAGE ####################
dashboardPage(skin = "blue",
  dashboardHeader(title = "Predicciones ML"),
  sidebar,
  body
)
