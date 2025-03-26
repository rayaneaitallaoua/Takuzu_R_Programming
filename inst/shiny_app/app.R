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
        style = "margin-bottom: 10px; min-height: 80px;",  # fixed height to avoid movement
        uiOutput("error_messages"),
        uiOutput("congratulations_message")
      ),

      # Grid + GIF in a row
      fluidRow(
        column(
          width = 9,
          uiOutput("takuzu_grid_ui")
        ),
        column(
          width = 3,
          div(
            style = "min-height: 300px;",  # reserve space to avoid layout shift
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

    rule3_violated <- check_rule_3(grid)

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
        if (rule3_violated)
          tags$p("🔴 Rule violated: Two full rows or columns are identical.", style = "color: red; font-weight: bold;")
      )
    })

    # Display congratulatory message if grid is complete and valid
    is_grid_complete <- all(grid != "")
    all_rules_respected <- !rule1_violated && !rule2_violated && !rule3_violated

    output$congratulations_message <- renderUI({
      if (is_grid_complete && all_rules_respected) {
        tags$p("🎉 Congratulations! You've completed the Takuzu grid correctly!",
               style = "color: green; font-weight: bold; font-size: 20px;")
      } else {
        NULL
      }
    })

    # Display GIFs for success or error using shinycustomloader
    output$gif_message <- renderUI({
      if (rule1_violated || rule2_violated || rule3_violated) {
        withLoader(plotOutput("distPlot"), type = "image", loader = "false-wrong.gif")
      } else if (is_grid_complete && all_rules_respected) {
        withLoader(plotOutput("distPlot"), type = "image", loader = "happy-dance-gif-5.gif")
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
