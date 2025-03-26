library(shiny)
library(Rcpp)
library(TakuzuR)
library(shinycustomloader)

# Fonction pour générer une grille Takuzu avec différents niveaux de difficulté en respectant les règles
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

    if (check_rule_1(grid, i-1, j-1) || check_rule_2(grid, i-1, j-1) || check_rule_3(grid)) {
      grid[i, j] <- ""
    }
  }

  return(grid)
}

ui <- fluidPage(
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
      br(),
      selectInput("difficulty", "Choose Difficulty:", choices = c("Easy", "Medium", "Hard")),
      actionButton("new_game", "New Game", class = "btn-primary")
    ),

    mainPanel(
      uiOutput("takuzu_grid_ui"),
      div(
        style = "margin-top: 20px;",
        uiOutput("error_messages"),
        uiOutput("congratulations_message"),
        uiOutput("gif_message")
      )
    )
  )
)

server <- function(input, output, session) {
  size <- 8
  game_state <- reactiveValues(grid = generate_takuzu_grid(size, "Easy"))

  observe({
    grid <- game_state$grid

    rule1_violated <- any(sapply(1:size, function(i) {
      any(sapply(1:size, function(j) check_rule_1(grid, i-1, j-1)))
    }))

    rule2_violated <- any(sapply(1:size, function(i) {
      any(sapply(1:size, function(j) check_rule_2(grid, i-1, j-1)))
    }))

    rule3_violated <- check_rule_3(grid)

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

    # Affichage des messages sous la grille
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

    # Affichage des messages d'erreur en bas de la grille
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

    # Vérification si la grille est complète et respecte les règles
    is_grid_complete <- all(grid != "")  # Vérifie que toutes les cases sont remplies
    all_rules_respected <- !rule1_violated && !rule2_violated && !rule3_violated

    # Affichage du message de félicitations si la grille est complète et les règles sont respectées
    output$congratulations_message <- renderUI({
      if (is_grid_complete && all_rules_respected) {
        tags$p("🎉 Congratulations! You've completed the Takuzu grid correctly!", style = "color: green; font-weight: bold; font-size: 20px;")
      } else {
        NULL  # Aucun message si la grille n'est pas complète ou les règles ne sont pas respectées
      }
    })

    # Affichage du GIF en cas d'erreur ou de succès
    output$gif_message <- renderUI({
      if (rule1_violated || rule2_violated || rule3_violated) {
        # Utiliser shinycustomloader pour afficher un GIF d'erreur
        withLoader(plotOutput("distPlot"), type = "image", loader = "false-wrong.gif")
      } else if (is_grid_complete && all_rules_respected) {
        # Utiliser shinycustomloader pour afficher un GIF de succès
        withLoader(plotOutput("distPlot"), type = "image", loader = "happy-dance-gif-5.gif")
      } else {
        NULL  # Aucun GIF si aucune erreur et pas de succès
      }
    })
  })

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

  observeEvent(input$new_game, {
    game_state$grid <- generate_takuzu_grid(size, input$difficulty)
  })
}

shinyApp(ui, server)


