source("global.R")

fluidPage(
  titlePanel("Takuzu Game"),

  sidebarLayout(
    sidebarPanel(
      h3("Game Rules"),
      p("Takuzu (also called Binairo) is a logic puzzle game where you must fill the grid using 0s and 1s, following these rules:"),
      tags$ul(
        tags$li("Each row and each column must contain an equal number of 0s and 1s."),
        tags$li("No more than two consecutive 0s or two 1s in the same row or column."),
        tags$li("No two rows or two columns can be identical."),
        tags$li("Click a cell to toggle its value (0 ↔ 1).")
      ),
      br(),
      actionButton("new_game", "New Game", class = "btn-primary")
    ),

    mainPanel(
      uiOutput("takuzu_grid_ui") # Dynamic grid
    )
  )
)