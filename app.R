library(shiny)
library(dplyr)
library(factoextra)
library(rpart)
library(rpart.plot)

source("./ui.R")
source("./server.R")

shinyApp(ui = ui, server = server)
