library(shiny)
library(Rcpp)

# C++ functions to check Takuzu rule violation
#check if we don't have more than two consecutive 0 or 1s
cppFunction('
bool checkRule1(CharacterMatrix grid, int i, int j) {
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

#check if we  have the same number of 1 and 0 per collum or row
cppFunction('bool checkRule2(CharacterMatrix grid, int i, int j) {
  int size = grid.nrow();
  int count0_row = 0, count1_row = 0;
  int count0_col = 0, count1_col = 0;
  bool row_full = true, col_full = true;

  // Check row
  for (int col = 0; col < size; col++) {
    std::string val = Rcpp::as<std::string>(grid(i, col));
    if (val == "") row_full = false;
    else if (val == "0") count0_row++;
    else if (val == "1") count1_row++;
  }
  if (row_full && count0_row != count1_row) return true;

  // Check column
  for (int row = 0; row < size; row++) {
    std::string val = Rcpp::as<std::string>(grid(row, j));
    if (val == "") col_full = false;
    else if (val == "0") count0_col++;
    else if (val == "1") count1_col++;
  }
  if (col_full && count0_col != count1_col) return true;

  return false;
}
' )

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
        if (checkRule1(grid, i-1, j-1)) {
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
              cell_red <- checkRule1(grid, i-1, j-1)
              row_or_col_red <- checkRule2(grid, i-1, j-1)

              # Appliquer les couleurs selon les règles
              if (cell_red) {
                color <- "red"
              } else if (row_or_col_red) {
                color <- "lightcoral"  # Couleur plus douce pour signaler un déséquilibre de 0s/1s
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


