data_visualication <- tabPanel(
  "Data Visualization",
  fluidRow(
    column(
      6,
      box(width = 12, title = "Central Tendency", status = "primary",
          plotOutput("viz_tend"))
    ),
    column(
      6,
      box(width = 12, title = "Categorical Distribution", status = "warning",
          plotOutput("viz_table"))
    )
  ),
  fluidRow(
    column(
      6,
      box(width = 12, title = "Numerical Frequency", status = "success",
          plotOutput("viz_freq"))
    ),
    column(
      6,
      box(width = 12, title = "Relation Plot", status = "danger",
          plotOutput("viz_relation"))
    )
  )
)