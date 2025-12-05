# R/tree_module.R


tree_server <- function(input, output, cleaned_df) {
    reactive_tree <- reactive({
        req(cleaned_df())
        dt(cleaned_df())
    })

    output$treePlot <- renderPlot({
        req(reactive_tree())
        rpart.plot(reactive_tree(),
            main = "Classification Tree - Late Delivery",
            type = 2, extra = 104, fallen.leaves = TRUE
        )
    })

    reactive_reg_tree <- reactive({
        req(cleaned_df())
        rpart(
            Delivery_Time_min ~ Traffic_Level + Time_of_Day +
                Vehicle_Type + Weather,
            data = cleaned_df(),
            method = "anova",
            control = rpart.control(minsplit = 5)
        )
    })

    output$treePlotreg <- renderPlot({
        req(reactive_reg_tree())
        rpart.plot(reactive_reg_tree(),
            main = "Regression Tree - Expected Delivery Time",
            type = 4, extra = 101, fallen.leaves = TRUE
        )
    })
}
