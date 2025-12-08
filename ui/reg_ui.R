reg_ui <- tabPanel(
    "Regression Tree",
    h3("Regression Tree"),
    plotOutput("treePlotreg", height = "600px"),
    h3("Regression Tree Metrics"),
    textOutput("reg_tree_metrics"),
    h3("Model Accuracy"),
    tableOutput("reg_predictions")
)
