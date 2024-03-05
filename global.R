################################ LOAD LIBRARIES ################################

library(shiny)
library(shinydashboard)
library(data.table)
library(dplyr)
library(plotly)


df <- fread(paste0("data/GermanCredit.csv"))

################################# LOAD MODULES #################################

my_modules <- list.files("tabs", pattern = "tab_module.R", full.names = TRUE,
                         recursive = TRUE)

for(my_module in my_modules) source(my_module)

