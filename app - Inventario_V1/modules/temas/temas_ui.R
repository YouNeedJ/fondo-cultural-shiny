# app/modules/temas/temas_ui.R

temas_ui <- function(id) {
  ns <- NS(id)

  tagList(
    h4("Gestión de Temas"),

    fluidRow(
      column(
        6,
        textInput(ns("codigo"), "Código", placeholder = " "),
        textInput(ns("tema"), "Tema", placeholder = " "),
        br(),
        actionButton(ns("buscar"), "Buscar", class = "btn-primary"),
        actionButton(ns("guardar"), "Guardar", class = "btn-success"),
        actionButton(ns("eliminar"), "Eliminar", class = "btn-danger")
      )
    )
  )
}
