tree_ui <- tabPanel(
    "Classification Tree",
    h3("Classification Tree"),
    plotOutput("treePlot", height = "600px"),
    h3("Model Accuracy"),
    tableOutput("tree_predictions")
)
