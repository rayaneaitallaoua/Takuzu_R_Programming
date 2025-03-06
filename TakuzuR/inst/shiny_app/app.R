library(shiny)
library(TakuzuR)

ui <- fluidPage(
  titlePanel("Takuzu Game"),
  actionButton("new_game", "New Game"),
  tableOutput("takuzu_grid")
)

server <- function(input, output) {
  grid <- reactiveVal(generate_takuzu_grid(8))

  observeEvent(input$new_game, {
    grid(generate_takuzu_grid(8))
  })

  output$takuzu_grid <- renderTable(grid())
}

shinyApp(ui, server)
