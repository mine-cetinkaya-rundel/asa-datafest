
source("helper.R", local = TRUE)
shinyApp(
  ui = fluidPage(
    fluidRow(
      column(12,
             tableOutput('table')
      )
    )
  ),
  server = function(input, output) {
    output$table <- renderTable(datafest)
  }
)


