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

        # showModal(
          # modalDialog(
            # title = "Libro nuevo: seleccionar bodega",
            # selectizeInput(session$ns("bodega_1"), "Bodega (obligatoria)", choices = bodegas, options = list(create = TRUE)),
            # selectizeInput(session$ns("bodega_2"), "Bodega adicional (opcional)", choices = bodegas, options = list(create = TRUE)),
            # footer = tagList(
              # modalButton("Cancelar"),
              # actionButton(session$ns("confirmar_guardado_nuevo"), "Confirmar", class="btn btn-primary")
            # )
          # )
        # )
		
		showModal(
		  modalDialog(
			title = "Libro nuevo: seleccionar bodega",
			selectInput(session$ns("bodega_1"),"Bodega (obligatoria)",choices = bodegas),
			selectInput(session$ns("bodega_2"),"Bodega adicional (opcional)",choices = c("", bodegas)),
			footer = tagList(
			  modalButton("Cancelar"),
			  actionButton(session$ns("confirmar_guardado_nuevo"),"Confirmar",class = "btn btn-primary")
			)
		  )
		)


        return()
      }

      # Si existe: sumar directo sin popup (por ahora)
		# Si existe
		actual <- existente[1, , drop = FALSE]

		cant_actual <- as.numeric(actual[["CANTIDAD BODEGA"]])
		if (is.na(cant_actual)) cant_actual <- 0

		cant_nueva <- cant_actual + as.numeric(input$cant_agregar)

		if (cant_nueva < 0) {
		  showNotification(
			paste("No se puede retirar más libros de los disponibles (", cant_actual, ")"),
			type = "error"
		  )
		  return()
		}

		bodegas <- get_bodegas()

		showModal(
		  modalDialog(
			title = "Libro existente encontrado",

			p(paste("Cantidad actual en bodega:", cant_actual)),
			p(paste("Cambio solicitado:", input$cant_agregar)),
			p(paste("Cantidad final:", cant_nueva)),

			numericInput(
			  session$ns("precio_popup"),
			  "Precio actual",
			  value = actual$PRECIO,
			  min = 0
			),

			
			selectInput(session$ns("bodega_1_popup"),"Bodega principal",choices = bodegas,selected = actual$BODEGA),

			selectInput(session$ns("bodega_2_popup"),"Bodega adicional",choices = c("", bodegas),selected = actual[["BODEGA ADICIONAL"]]),

			footer = tagList(
			  modalButton("Cancelar"),
			  actionButton(
				session$ns("confirmar_update"),
				"Confirmar",
				class = "btn btn-primary"
			  )
			)
		  )
		)

		return()
		
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
	
	observeEvent(input$confirmar_update, {

	  removeModal()

	  archivar_libro_manual(
		isbn = trimws(input$isbn),
		titulo = input$titulo,
		precio = input$precio_popup,
		cantidad_agregar = input$cant_agregar,
		editorial = input$editorial,
		autor = input$autor,
		bodega = input$bodega_1_popup,
		bodega_adicional = input$bodega_2_popup
	  )

	  showNotification("Inventario actualizado correctamente", type = "message")
	})
	
	observeEvent(input$isbn, {

	  isbn <- trimws(input$isbn)
	  if (isbn == "") return()

	  libro <- get_libro_por_isbn(isbn)

	  if (!is.null(libro)) {

		updateTextInput(session, "titulo",
		  value = libro$TITULO)

		updateNumericInput(session, "precio",
		  value = libro$PRECIO)

		updateTextInput(session, "editorial",
		  value = libro$EDITORIAL)

		updateTextInput(session, "autor",
		  value = libro$AUTOR)

		showNotification(
		  "Libro existente cargado automáticamente",
		  type = "message"
		)
	  }
	})

  })
}
