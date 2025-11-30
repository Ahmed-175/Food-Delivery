library(shiny)
library(bslib)
library(dplyr)
library(factoextra)

# Load external functions
source("R/cleaning.R")
source("R/kmeans.R")

ui <- page_sidebar(
  title = "dotR Delivery Analysis",
  sidebar = sidebar(
    width = 300,
    open = "always",
    h3("⚙️ Controls"),
    tags$hr(),
    fileInput("file", "Upload CSV", accept = ".csv"),
    selectInput("col", "Choose Column for Histogram", choices = NULL),
    numericInput("bins", "Number of Bins", value = 15, min = 5, max = 50),
    numericInput("k", "Number of Clusters", value = 3, min = 2, max = 10),
    actionButton("run", "Generate", class = "btn-primary btn-lg")
  ),
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins")
  ),
  layout_column_wrap(
    width = 1 / 2,
    card(
      full_screen = TRUE,
      card_header("📄 Uploaded Data"),
      tableOutput("data")
    ),
    card(
      full_screen = TRUE,
      card_header("📊 Histogram"),
      plotOutput("graph", height = "400px")
    ),
    card(
      full_screen = TRUE,
      card_header("🧩 Clusters"),
      tableOutput("cluster_table"),
      plotOutput("cluster_plot", height = "400px")
    )
  )
)

server <- function(input, output, session) {
  # Reactive: read and clean data
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    clean_data(df)
  })

  # Update selectInput with column names
  observe({
    req(data())
    updateSelectInput(session, "col", choices = names(data()))
  })


  # Render data table
  output$data <- renderTable({
    data()
  })
  # Render histogram
  output$graph <- renderPlot({
    req(input$col)
    req(input$run)
    isolate({
      hist(
        data()[[input$col]],
        breaks = input$bins,
        col = "#4C9ED9",
        border = "white",
        main = paste("Distribution of", input$col),
        xlab = input$col
      )
    })
  })
  clusters <- reactive({
    req(data())
    perform_kmeans(data(), k = input$k)
  })
  output$cluster_table <- renderTable({
    req(clusters())
    head(clusters()$data[, c("Order_ID", "cluster", "Distance_km", "Delivery_Time_min", "Speed_kmph")], 20)
  })

  output$cluster_plot <- renderPlot({
    req(clusters())
    fviz_cluster(clusters()$km, data = clusters()$scaled, geom = "point", stand = FALSE)
  })
}

shinyApp(ui = ui, server = server)
