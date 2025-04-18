#' Takuzu Shiny App
#'
#' This application allows the user to play Takuzu interactively in the browser. It follows all three game rules
#' and supports multiple difficulty levels. Grid cells are colored according to rule violations and display
#' appropriate messages for feedback. A congratulatory message and visual feedback are shown upon completion.

library(shiny)
library(Rcpp)
library(TakuzuR)
library(shinycustomloader)

#' Generate a Takuzu grid based on difficulty
#'
#' @param size Integer, size of the Takuzu grid (default is 8x8)
#' @param difficulty Character, one of "Easy", "Medium", or "Hard"
#' @return A character matrix of Takuzu grid values with some prefilled cells
generate_takuzu_grid <- function(size, difficulty) {
  grid <- matrix("", nrow = size, ncol = size)
  fill_ratio <- switch(difficulty,
                       "Easy" = 0.7,
                       "Medium" = 0.5,
                       "Hard" = 0.25)
  num_filled <- round(size * size * fill_ratio)
  filled_positions <- sample(1:(size * size), num_filled)
  for (pos in filled_positions) {
    i <- ((pos - 1) %% size) + 1
    j <- ((pos - 1) %/% size) + 1
    attempt_value <- sample(c("0", "1"), 1)
    grid[i, j] <- attempt_value
    # Remove the value if it breaks a rule
    if (check_rule_1(grid, i-1, j-1) || check_rule_2(grid, i-1, j-1) || check_rule_3(grid)) {
      grid[i, j] <- ""
    }
  }
  return(grid)
}

# User Interface layout
iui <- fluidPage(

  titlePanel("Takuzu Game"),
  actionButton("play_music", "🎵 Play Music"),
  actionButton("pause_music", "⏸️ Pause Music"),
  tags$audio(id = "bg_music", src = "theme.mp3", type = "audio/mp3", autoplay = NA, loop = NA),

  # JS to control
  tags$script(HTML("
  $('#play_music').on('click', function() {
    document.getElementById('bg_music').play();
  });
  $('#pause_music').on('click', function() {
    document.getElementById('bg_music').pause();
  });
")),

  sidebarLayout(
    sidebarPanel(
      h3("Game Rules"),
      p("Takuzu (also called Binairo) is a logic puzzle game where you must fill the grid using 0s and 1s, following these rules:"),
      tags$ul(
        tags$li("Each row and each column must contain an equal number of 0s and 1s."),
        tags$li("No more than two consecutive 0s or two 1s in the same row or column."),
        tags$li("No two rows or two columns can be identical."),
        tags$li("Click a cell to toggle its value (empty → 0 → 1 → empty).")
      ),
      selectInput("difficulty", "Choose Difficulty:", choices = c("Easy", "Medium", "Hard")),
      actionButton("new_game", "New Game", class = "btn-primary")
    ),
    mainPanel(
      # Top messages (always shown above the grid)
      div(
        style = "height: 200px; overflow: hidden; display: flex;
                 flex-direction: column; justify-content: center;",
        uiOutput("error_messages"),
        uiOutput("congratulations_message")
      ),
      # Grid + GIF in a row
      fluidRow(
        div(
          style = "display: flex; align-items: center; justify-content: space-between;",
          # Grid area
          div(
            style = "flex: 1; padding-left: 20px; padding-right: 20px; padding-bottom: 20px;",
            uiOutput("takuzu_grid_ui")
          ),
          # GIF area
          div(
            style = "flex: 0 0 300px; padding-right: 20px; padding-bottom: 20px;",
            uiOutput("gif_message")
          )
        )
      )
    )
  )
)

#' Server logic
#'
#' Observes game state, updates the UI, handles cell toggling and error messaging.
server <- function(input, output, session) {
  size <- 8
  game_state <- reactiveValues(grid = generate_takuzu_grid(size, "Easy"))

  observe({
    grid <- game_state$grid
    # Check if any rule is violated across the entire grid
    rule1_violated <- any(sapply(1:size, function(i) {
      any(sapply(1:size, function(j) check_rule_1(grid, i-1, j-1)))
    }))
    rule2_violated <- any(sapply(1:size, function(i) {
      any(sapply(1:size, function(j) check_rule_2(grid, i-1, j-1)))
    }))
    rule3_violations <- check_rule_3(grid)

    has_identical_rows <- any(sapply(rule3_violations, function(x) x == 1))
    has_identical_cols <- any(sapply(rule3_violations, function(x) x == -1))


    # Render the grid buttons with appropriate color based on rule violations
    output$takuzu_grid_ui <- renderUI({
      grid <- game_state$grid
      tagList(
        lapply(1:size, function(i) {
          fluidRow(
            lapply(1:size, function(j) {
              cell_red <- check_rule_1(grid, i - 1, j - 1)
              row_or_col_red <- check_rule_2(grid, i - 1, j - 1)
              color <- if (cell_red) "red" else if (row_or_col_red) "lightcoral" else "white"
              actionButton(
                inputId = paste0("cell_", i, "_", j),
                label = as.character(grid[i, j]),
                style = paste(
                  "width: 40px; height: 40px; font-size: 18px; margin: 2px; background-color:",
                  color, ";"
                )
              )
            })
          )
        })
      )
    })

    # Display rule violation messages
    output$error_messages <- renderUI({
      tagList(
        if (rule1_violated)
          tags$p("🔴 Rule violated: More than two identical values in a row/column.", style = "color: red; font-weight: bold;"),
        if (rule2_violated)
          tags$p("🔴 Rule violated: Unequal number of 0s and 1s in a full row/column.", style = "color: red; font-weight: bold;"),
        if (has_identical_rows)
          tags$p("🔴 Rule violated: Two identical rows.", style = "color: red; font-weight: bold;"),
        if (has_identical_cols)
          tags$p("🔴 Rule violated: Two identical columns.", style = "color: red; font-weight: bold;")
      )
    })

    # Display congratulatory message if grid is complete and valid
    is_grid_complete <- all(grid != "")
    is_grid_half_complete <- (sum(apply(grid, 1, function(row) all(row != ""))) >= size / 2) ||
      (sum(apply(grid, 2, function(col) all(col != ""))) >= size / 2)

    all_rules_respected <- !rule1_violated && !rule2_violated && !has_identical_rows && !has_identical_cols
    output$congratulations_message <- renderUI({
      if (is_grid_complete && all_rules_respected) {
        tags$p("🎉 Congratulations! You've completed the Takuzu grid correctly!",
               style = "color: green; font-weight: bold; font-size: 20px;")
      } else if(is_grid_half_complete && all_rules_respected){
        tags$p("🎉 Congratulations! You've completed half of the Takuzu grid correctly!",
               style = "color: green; font-weight: bold; font-size: 20px;")
      } else {
        NULL
      }
    })

    # Display GIFs for success or error using shinycustomloader
    output$gif_message <- renderUI({
      if (rule1_violated && rule2_violated ) {
        tags$img(src = "no.gif",  style = "max-width: 300px; max-height: 100%;")
      } else if (rule1_violated ) {
        tags$img(src = "false-wrong.gif",  style = "max-width: 300px; max-height: 100%;")
      } else if (has_identical_rows || has_identical_cols ) {
        tags$img(src = "the-office-michael-scott.gif",  style = "max-width: 300px; max-height: 100%;")
      } else if(is_grid_half_complete && all_rules_respected ) {
        tags$img(src = "dance.gif",  style = "max-width: 300px; max-height: 100%;")

      } else if (is_grid_complete && all_rules_respected) {
        tags$img(src = "happy-dance-gif-5.gif",  style = "max-width: 300px; max-height: 100%;")
      } else {
        NULL
      }
    })
  })

  # Observe all cell clicks and update values cyclically ("" -> "0" -> "1" -> "")
  observe({
    lapply(1:size, function(i) {
      lapply(1:size, function(j) {
        cell_id <- paste0("cell_", i, "_", j)
        observeEvent(input[[cell_id]], {
          current_value <- game_state$grid[i, j]
          new_value <- ifelse(current_value == "", "0", ifelse(current_value == "0", "1", ""))
          game_state$grid[i, j] <- new_value
        }, ignoreInit = TRUE)
      })
    })
  })

  # Generate a new grid based on difficulty input when "New Game" is clicked
  observeEvent(input$new_game, {
    game_state$grid <- generate_takuzu_grid(size, input$difficulty)
  })
}

#' Launch the Shiny app
shinyApp(iui, server)
