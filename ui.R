source("ui/data_ui.R")
source("ui/outliers.R")
source("ui/kmeans_ui.R")
source("ui/cluster_summary_ui.R")
source("ui/tree_ui.R")
source("ui/reg_ui.R")
ui <- fluidPage(
  titlePanel("dotR Delivery Analysis"),
  fluidRow(
    column(width = 3, wellPanel(
      h3("Controls"),
      fileInput("file", "Upload CSV", accept = ".csv"),
      numericInput("k", "Number of Clusters", value = 3, min = 2, max = 10)
    )),
    column(
      width = 9,
      tabsetPanel(
        data_ui,
        tabPanel("Central Tendency", plotOutput("tend_plot")),
        tabPanel("Categorical Data", uiOutput("cat_plots")),
        tabPanel("Relation Plots", uiOutput("relation_plots")),
        outliers_ui,
        kmeans_ui,
        cluster_summary_ui,
        tree_ui,
        reg_ui
      )
    )
  )
)
