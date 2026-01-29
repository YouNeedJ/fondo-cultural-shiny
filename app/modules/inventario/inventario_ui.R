# app/modules/inventario/inventario_ui.R

inventario_ui <- function(id) {
  ns <- NS(id)

  bslib::navset_tab(

    nav_panel(
      title = "Archivar libro",

      fluidRow(
        column(
          12,
          div(
            class = "text-center my-4",
            h4("Archivar libro", class = "fw-bold"),
            p("Registrar nuevos libros o sumar cantidad por ISBN", class = "text-muted")
          )
        )
      ),

      fluidRow(
        column(6, textInput(ns("isbn"), "ISBN", placeholder = "978...")),
        column(6, textInput(ns("titulo"), "Título", placeholder = "Nombre del libro"))
      ),

      fluidRow(
        column(4, numericInput(ns("precio"), "Precio", value = 0, min = 0)),
        column(4, numericInput(ns("cant_agregar"), "Cantidad de libros a agregar", value = 1, min = 1)),
        column(4, textInput(ns("editorial"), "Editorial", placeholder = "Editorial"))
      ),

      fluidRow(
        column(6, textInput(ns("autor"), "Autor", placeholder = "Autor")),
        column(6, div(class="pt-4",
          actionButton(ns("guardar"), "Guardar / Sumar", class = "btn btn-success w-100")
        ))
      ),

      hr(),

      div(class="text-muted small",
          "Nota: si el ISBN ya existe, se suma a CANTIDAD BODEGA. Si es nuevo, se pedirá la bodega (popup)."
      )
    ),

    nav_panel(
      title = "Cargar desde PDF (IA)",
      div(class="text-center my-4",
          h4("Cargar desde PDF (IA)", class="fw-bold"),
          p("Pendiente: extracción de texto + OpenAI + confirmaciones de precio/bodega", class="text-muted")
      )
    ),

    nav_panel(
      title = "Estadísticas",
      div(class="text-center my-4",
          h4("Estadísticas", class="fw-bold"),
          p("Pendiente: gráficas y tablas solicitadas", class="text-muted")
      )
    )
  )
}
