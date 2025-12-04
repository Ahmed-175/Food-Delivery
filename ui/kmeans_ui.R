kmeans_ui <- tabPanel(
    "K-Means Clustering",
    h3("Cluster Table"),
    tableOutput("cluster_table"),
    h3("Cluster Plot"),
    plotOutput("cluster_plot", height = "400px")
)
