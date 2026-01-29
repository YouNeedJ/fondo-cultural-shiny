# app/modules/inventario/inventario_server.R

inventario_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    observeEvent(input$guardar, {

      isbn <- trimws(input$isbn)
      if (isbn == "") {
        showNotification("ISBN es obligatorio", type = "error")
        return()
      }

      existente <- get_libro_por_isbn(isbn)

      # Si es nuevo, pedir bodega (popup)
      if (is.null(existente)) {

        bodegas <- get_bodegas()

        showModal(
          modalDialog(
            title = "Libro nuevo: seleccionar bodega",
            selectizeInput(session$ns("bodega_1"), "Bodega (obligatoria)", choices = bodegas, options = list(create = TRUE)),
            selectizeInput(session$ns("bodega_2"), "Bodega adicional (opcional)", choices = bodegas, options = list(create = TRUE)),
            footer = tagList(
              modalButton("Cancelar"),
              actionButton(session$ns("confirmar_guardado_nuevo"), "Confirmar", class="btn btn-primary")
            )
          )
        )

        return()
      }

      # Si existe: sumar directo sin popup (por ahora)
      res <- archivar_libro_manual(
        isbn = isbn,
        titulo = input$titulo,
        precio = input$precio,
        cantidad_agregar = input$cant_agregar,
        editorial = input$editorial,
        autor = input$autor,
        bodega = NA
      )

      showNotification(paste("OK:", res$action, "-", res$isbn), type = "message")
    })

    # Confirmación libro nuevo (popup)
    observeEvent(input$confirmar_guardado_nuevo, {
      removeModal()

      res <- archivar_libro_manual(
        isbn = trimws(input$isbn),
        titulo = input$titulo,
        precio = input$precio,
        cantidad_agregar = input$cant_agregar,
        editorial = input$editorial,
        autor = input$autor,
        bodega = input$bodega_1,
        bodega_adicional = input$bodega_2
      )

      showNotification(paste("Creado:", res$isbn), type = "message")
    })
  })
}
