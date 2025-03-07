library(shiny)
library(TakuzuR)

ui <- fluidPage(
  titlePanel("Takuzu Game"),
  actionButton("new_game", "New Game"),
  uiOutput("takuzu_grid_ui") # Dynamic grid
)

server <- function(input, output, session) {
  # Store the game grid as a reactiveValues object
  game_state <- reactiveValues(grid = generate_takuzu_grid(8))

  # Generate new grid when "New Game" is clicked
  observeEvent(input$new_game, {
    game_state$grid <- generate_takuzu_grid(8)
  })

  # Generate the interactive grid UI (updates when game_state$grid changes)
  output$takuzu_grid_ui <- renderUI({
    grid <- game_state$grid
    size <- nrow(grid)

    tagList(
      lapply(1:size, function(i) {
        fluidRow(
          lapply(1:size, function(j) {
            actionButton(
              inputId = paste0("cell_", i, "_", j),
              label = as.character(grid[i, j]), # Ensure text format
              style = "width: 40px; height: 40px; font-size: 18px;"
            )
          })
        )
      })
    )
  })

  # Observe button clicks and toggle the corresponding cell value
  observe({
    grid <- game_state$grid
    size <- nrow(grid)
    for (i in 1:size) {
      for (j in 1:size) {
        cell_id <- paste0("cell_", i, "_", j)
        observeEvent(input[[cell_id]], {
          # Toggle the value in the grid
          game_state$grid[i, j] <- 1 - game_state$grid[i, j]

          # Dynamically update the label of the clicked button
          updateActionButton(session, cell_id, label = as.character(game_state$grid[i, j]))
        }, ignoreInit = TRUE)
      }
    }
  })
}

shinyApp(ui, server)
