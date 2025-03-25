library(shiny)
library(Rcpp)

# C++ function to check Takuzu rule violation
cppFunction('
bool checkRule(CharacterMatrix grid, int i, int j) {
  int size = grid.nrow();
  std::string value = Rcpp::as<std::string>(grid(i, j));

  if (value == "") return false;  // Ignore empty cells

  // Check horizontal rule
  if (j > 1 && grid(i, j-1) == value && grid(i, j-2) == value) return true;
  if (j < size - 2 && grid(i, j+1) == value && grid(i, j+2) == value) return true;
  if (j > 0 && j < size - 1 && grid(i, j-1) == value && grid(i, j+1) == value) return true;

  // Check vertical rule
  if (i > 1 && grid(i-1, j) == value && grid(i-2, j) == value) return true;
  if (i < size - 2 && grid(i+1, j) == value && grid(i+2, j) == value) return true;
  if (i > 0 && i < size - 1 && grid(i-1, j) == value && grid(i+1, j) == value) return true;

  return false;
}
')

# Function to generate a starting grid with "", "0" and "1"
generate_takuzu_grid <- function(size) {
  values <- c("0", "1", "")  # Possible values
  matrix(sample(values, size^2, replace = TRUE, prob = c(0.4, 0.4, 0.2)), nrow = size)
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
              color <- if (checkRule(grid, i-1, j-1)) "red" else "white"
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
