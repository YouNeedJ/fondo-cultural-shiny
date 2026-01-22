# app/modules/clientes/clientes_server.R

library(plotly)

source("services/clientes_service.R")

clientes_server <- function(id) {
  moduleServer(id, function(input, output, session) {
  
	df_filtrado <- reactiveVal(data.frame())


    ns <- session$ns
	
	# --------------------------------------------------
    # Cargar ciudades para autocomplete
    # --------------------------------------------------
# --------------------------------------------------
# Cargar ciudades (formulario y filtros)
# --------------------------------------------------
	observe({

		ciudades <- get_ciudades()

		# Formulario (Gestión de Clientes)
		updateSelectizeInput(
			session,
			"ciudad",
			choices = ciudades,
			server = TRUE
		)

		# Filtros (Análisis de Clientes)
		updateSelectizeInput(
			session,
			"filtro_ciudad",
			choices = ciudades,
			server = TRUE
		)

	})



    # --------------------------------------------------
    # Buscar cliente por ID
    # --------------------------------------------------
    observeEvent(input$buscar, {

      req(input$id_cliente)

      cliente <- get_cliente_por_id(input$id_cliente)

      if (is.null(cliente)) {
        showNotification(
          "Cliente no encontrado. Puede crear uno nuevo.",
          type = "message"
        )

		updateTextInput(session, "id_cliente", value = NA)
		updateTextInput(session, "nombre", value = NA)
		updateTextInput(session, "direccion", value = NA)
		updateTextInput(session, "correo", value = NA)

        updateSelectizeInput(session, "ciudad", selected = character(0))
        updateSelectInput(session, "tipo_persona", selected = "CLIENTE")
        updateNumericInput(session, "descuento", value = 0)

      } else {
        updateTextInput(session, "nombre", value = cliente$CLIENTES)
        updateTextInput(session, "direccion", value = cliente$DIRECCION)
        updateTextInput(session, "correo", value = cliente$CORREO)
        updateSelectizeInput(session, "ciudad", selected = cliente$CIUDAD)
        updateSelectInput(session, "tipo_persona", selected = cliente$TIPO_PERSONA)
        updateNumericInput(session, "descuento", value = cliente$DESC_)
      }

    })

    # --------------------------------------------------
    # Guardar / Actualizar cliente
    # --------------------------------------------------
	observeEvent(input$guardar, {

	  req(
		input$id_cliente,
		input$nombre,
		input$ciudad,
		input$tipo_persona
	  )

	  df <- data.frame(
		ID_CLIENTE   = trimws(input$id_cliente),
		CLIENTES     = toupper(trimws(input$nombre)),
		DIRECCION    = toupper(trimws(input$direccion)),
		CORREO       = trimws(input$correo),
		CIUDAD       = toupper(trimws(input$ciudad)),
		TIPO_PERSONA = input$tipo_persona,
		DESC_        = input$descuento,
		stringsAsFactors = FALSE
	  )

	  res <- save_cliente(df)

	  if (res$action == "insert") {
		showNotification("Cliente creado correctamente", type = "message")
	  } else {
		showNotification("Cliente actualizado correctamente", type = "message")
	  }

	  # Limpiar formulario
		updateTextInput(session, "id_cliente", value = NA)
		updateTextInput(session, "nombre", value = NA)
		updateTextInput(session, "direccion", value = NA)
		updateTextInput(session, "correo", value = NA)

	  updateSelectizeInput(session, "ciudad", selected = character(0))
	  updateSelectInput(session, "tipo_persona", selected = "CLIENTE")
	  updateNumericInput(session, "descuento", value = 0)
	})


	
	observeEvent(input$aplicar_filtros, {

	  df <- search_clientes(
		ciudades = input$filtro_ciudad,
		tipos    = input$filtro_tipo
	  )

	  # si viene _id, lo quitamos para que la tabla sea más limpia
	  if (ncol(df) > 0 && "_id" %in% names(df)) {
		df$`_id` <- NULL
	  }

	  # Renombrar SOLO si existen las columnas (evita errores)
	  ren_map <- c(
		"Cliente" = "CLIENTES",
		"Identificación" = "ID_CLIENTE",
		"Dirección" = "DIRECCION",
		"Correo" = "CORREO",
		"Ciudad" = "CIUDAD",
		"Tipo" = "TIPO_PERSONA",
		"Descuento (%)" = "DESC_"
	  )

	  for (nuevo in names(ren_map)) {
		viejo <- ren_map[[nuevo]]
		if (viejo %in% names(df)) names(df)[names(df) == viejo] <- nuevo
	  }

	  df_filtrado(df)
	})




    # --------------------------------------------------
    # Gráfica: Clientes por ciudad
    # --------------------------------------------------
    output$plot_ciudad <- renderPlotly({

      df <- clientes_por_ciudad()
      req(nrow(df) > 0)

      plot_ly(
        df,
        x = ~`_id`,
        y = ~total,
        type = "bar"
      ) %>%
        layout(
          title = "Clientes por ciudad",
          xaxis = list(title = "Ciudad"),
          yaxis = list(title = "Número de clientes")
        )
    })

    # --------------------------------------------------
    # Gráfica: Clientes por tipo
    # --------------------------------------------------
    output$plot_tipo <- renderPlotly({

      df <- clientes_por_tipo()
      req(nrow(df) > 0)

      plot_ly(
        df,
        labels = ~`_id`,
        values = ~total,
        type = "pie"
      ) %>%
        layout(
          title = "Clientes por tipo de cliente"
        )
    })
	
	# --------------------------------------------------
	# Tabla de resultados (lee del estado reactivo)
	# --------------------------------------------------
	output$tabla_clientes <- DT::renderDT({
	  df <- df_filtrado()

	  DT::datatable(
		df,
		class = "stripe hover compact",
		options = list(
		  pageLength = 8,
		  lengthChange = FALSE,
		  autoWidth = TRUE
		),
		rownames = FALSE
	  )
	})


	# ----------------------------
	# Eliminar: modal confirmación
	# ----------------------------
	observeEvent(input$eliminar, {

	  id <- trimws(input$id_cliente)
	  req(id)

	  showModal(
		modalDialog(
		  title = "Confirmar eliminación",
		  paste("Eliminar el cliente con ID:", id, "?"),
		  footer = tagList(
			modalButton("Cancelar"),
			actionButton(
			  ns("confirmar_eliminar"),
			  "Sí, eliminar",
			  class = "btn-danger"
			)
		  )
		)
	  )
	})

	# ----------------------------
	# Eliminar: ejecutar
	# ----------------------------
	observeEvent(input$confirmar_eliminar, {

	  removeModal()

	  id <- trimws(input$id_cliente)
	  req(id)

	  tryCatch({

		delete_cliente_por_id(id)

		showNotification("Cliente eliminado correctamente", type = "message")

		# Limpiar formulario
		updateTextInput(session, "id_cliente", value = NA)
		updateTextInput(session, "nombre", value = NA)
		updateTextInput(session, "direccion", value = NA)
		updateTextInput(session, "correo", value = NA)

		updateSelectizeInput(session, "ciudad", selected = character(0))
		updateSelectInput(session, "tipo_persona", selected = "CLIENTE")
		updateNumericInput(session, "descuento", value = 0)

		# Refrescar tabla
		df <- search_clientes(
		  ciudades = input$filtro_ciudad,
		  tipos    = input$filtro_tipo
		)

		if (ncol(df) > 0 && "_id" %in% names(df)) df$`_id` <- NULL

		ren_map <- c(
		  "Cliente" = "CLIENTES",
		  "Identificación" = "ID_CLIENTE",
		  "Dirección" = "DIRECCION",
		  "Correo" = "CORREO",
		  "Ciudad" = "CIUDAD",
		  "Tipo" = "TIPO_PERSONA",
		  "Descuento (%)" = "DESC_"
		)

		for (nuevo in names(ren_map)) {
		  viejo <- ren_map[[nuevo]]
		  if (viejo %in% names(df)) names(df)[names(df) == viejo] <- nuevo
		}

		df_filtrado(df)

	  }, error = function(e) {
		showNotification(
		  paste("Error al eliminar:", e$message),
		  type = "error"
		)
	  })
	})



  })
}
