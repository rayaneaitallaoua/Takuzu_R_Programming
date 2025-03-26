library(shiny)
library(Rcpp)
library(TakuzuR)

# Function to generate a starting grid with "", "0" and "1"
generate_takuzu_grid <- function(size) {
  grid <- matrix("", nrow = size, ncol = size)  # Initialise une grille vide

  # Définir une probabilité plus faible pour "0" et "1"
  values <- c("0", "1", "")
  probs <- c(0.2, 0.2, 0.6)  # Moins de 0 et 1, plus de vides

  for (i in 1:size) {
    for (j in 1:size) {
      # Essayer d'attribuer un 0 ou un 1 avec probabilité faible
      if (runif(1) < 0.3) {  # 30% de chance de mettre un chiffre
        attempt_value <- sample(c("0", "1"), 1)
        grid[i, j] <- attempt_value

        # Vérifier si la règle est violée après ajout
        if (check_rule_1(grid, i-1, j-1)) {
          grid[i, j] <- ""  # Annuler si cela cause un problème
        }
      }
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
      actionButton("new_game", "New Game", class = "btn-primary")
    ),

    mainPanel(
      uiOutput("takuzu_grid_ui")
    )
  )
)

server <- function(input, output, session) {
  size <- 8
  game_state <- reactiveValues(grid = generate_takuzu_grid(size))

  # Observateur pour mettre à jour l'affichage
  observe({
    grid <- game_state$grid
    output$takuzu_grid_ui <- renderUI({
      tagList(
        lapply(1:size, function(i) {
          fluidRow(
            lapply(1:size, function(j) {
              cell_red <- check_rule_1(grid, i - 1, j - 1)
              row_or_col_red <- check_rule_2(grid, i - 1, j - 1)
              duplicate_grid <- check_rule_3(grid)

              # Appliquer les couleurs selon les règles
              if (cell_red) {
                color <- "red"
              } else if (row_or_col_red) {
                color <- "lightcoral"
              } else if (duplicate_grid) {
                color <- "orange"
              } else {
                color <- "white"
              }

              actionButton(
                inputId = paste0("cell_", i, "_", j),
                label = as.character(grid[i, j]),
                style = paste("width: 40px; height: 40px; font-size: 18px; margin: 2px; background-color:", color, ";")
              )
            })
          )
        })
      )
    })
  })

  # Observer chaque cellule et mettre à jour dynamiquement
  observe({
    lapply(1:size, function(i) {
      lapply(1:size, function(j) {
        cell_id <- paste0("cell_", i, "_", j)
        observeEvent(input[[cell_id]], {
          # Cycle des valeurs : "" → "0" → "1" → ""
          current_value <- game_state$grid[i, j]
          new_value <- ifelse(current_value == "", "0", ifelse(current_value == "0", "1", ""))
          game_state$grid[i, j] <- new_value
        }, ignoreInit = TRUE)
      })
    })
  })

  # Bouton "Nouvelle Partie"
  observeEvent(input$new_game, {
    game_state$grid <- generate_takuzu_grid(size)
  })
}

shinyApp(ui, server)

