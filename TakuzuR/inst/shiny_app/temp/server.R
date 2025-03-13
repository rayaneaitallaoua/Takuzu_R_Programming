source("global.R")

server <- function(input, output, session) {
  # Store the game grid as a reactive value
  game_state <- reactiveValues(grid = generate_takuzu_grid(8))

  # Generate new grid when "New Game" is clicked
  observeEvent(input$new_game, {
    game_state$grid <- generate_takuzu_grid(8)
  })

  # Render the Takuzu grid
  output$takuzu_grid_ui <- renderUI({
    grid <- game_state$grid
    size <- nrow(grid)

    tagList(
      lapply(1:size, function(i) {
        fluidRow(
          lapply(1:size, function(j) {
            actionButton(
              inputId = paste0("cell_", i, "_", j),
              label = as.character(grid[i, j]),
              style = "width: 40px; height: 40px; font-size: 18px; margin: 2px;"
            )
          })
        )
      })
    )
  })

  # Observe button clicks and update the grid
  observe({
    size <- nrow(game_state$grid)
    for (i in 1:size) {
      for (j in 1:size) {
        cell_id <- paste0("cell_", i, "_", j)

        observeEvent(input[[cell_id]], {
          # Toggle the value in the grid
          game_state$grid[i, j] <- 1 - game_state$grid[i, j]

          # Re-render the UI to update all button labels
          output$takuzu_grid_ui <- renderUI({
            grid <- game_state$grid
            tagList(
              lapply(1:size, function(i) {
                fluidRow(
                  lapply(1:size, function(j) {
                    actionButton(
                      inputId = paste0("cell_", i, "_", j),
                      label = as.character(grid[i, j]),
                      style = "width: 40px; height: 40px; font-size: 18px; margin: 2px;"
                    )
                  })
                )
              })
            )
          })
        }, ignoreInit = TRUE)
      }
    }
  })
}