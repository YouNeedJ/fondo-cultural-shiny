# app/modules/inventario/inventario_server.R
library(pdftools)
library(openxlsx)


inventario_server <- function(id) {
	moduleServer(id, function(input, output, session) {
		
		ns <- session$ns

		libros_pdf <- reactiveVal(NULL)
		indice_pdf <- reactiveVal(1)



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

			# Limpiar formulario
			updateTextInput(session, "isbn", value = "")
			updateTextInput(session, "titulo", value = "")
			updateNumericInput(session, "precio", value = 0)
			updateNumericInput(session, "cant_agregar", value = 1)
			updateTextInput(session, "editorial", value = "")
			updateTextInput(session, "autor", value = "")

		})

		observeEvent(input$confirmar_update, {

			removeModal()

			isbn <- trimws(input$isbn)

			# Reconsultar para mensaje claro
			libro <- get_libro_por_isbn(isbn)
			cant_actual <- as.numeric(libro[["CANTIDAD BODEGA"]])
			if (is.na(cant_actual)) cant_actual <- 0

			cambio <- as.numeric(input$cant_agregar)
			if (is.na(cambio)) cambio <- 0

			cant_final <- cant_actual + cambio

			# Guardar
			archivar_libro_manual(
				isbn = isbn,
				titulo = input$titulo,
				precio = input$precio,
				cantidad_agregar = input$cant_agregar,
				editorial = input$editorial,
				autor = input$autor,
				bodega = input$bodega_1_popup,
				bodega_adicional = input$bodega_2_popup
			)

			# Mensaje
			if (cambio < 0) {
				showNotification(
					paste0("Se retiraron ", abs(cambio), " libros. Tenías ", cant_actual, " y quedaron ", cant_final, "."),
					type = "message"
				)
			} else {
				showNotification(
					paste0("Se agregaron ", cambio, " libros. Tenías ", cant_actual, " y quedaron ", cant_final, "."),
					type = "message"
				)
			}

			# Limpiar formulario
			updateTextInput(session, "isbn", value = "")
			updateTextInput(session, "titulo", value = "")
			updateNumericInput(session, "precio", value = 0)
			updateNumericInput(session, "cant_agregar", value = 1)
			updateTextInput(session, "editorial", value = "")
			updateTextInput(session, "autor", value = "")
		})


		observeEvent(input$isbn, {

			isbn <- normalizar_isbn(input$isbn)

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

		observeEvent(input$eliminar, {

			req(input$isbn)

			showModal(modalDialog(
				title = "Confirmar eliminación",
				paste("¿Eliminar libro ISBN", input$isbn, "?"),
				footer = tagList(
					modalButton("Cancelar"),
					actionButton(ns("confirmar_eliminar"),
					"Eliminar",
					class="btn btn-danger")
				)
			))

		})

		observeEvent(input$confirmar_eliminar, {

			eliminar_libro_por_isbn(input$isbn)

			removeModal()

			showNotification("Libro eliminado", type="message")

		})

		observeEvent(input$procesar_pdf, {

			req(input$pdf_file)

			showModal(modalDialog(
				title = "Procesando PDF",
				tagList(
					p("Leyendo archivo PDF..."),
					p("Consultando IA para extraer libros..."),
					br(),
					strong("Esto puede tardar algunos segundos.")
				),
				footer = NULL,
				easyClose = FALSE
			))


			texto <- pdftools::pdf_text(input$pdf_file$datapath)
			texto <- paste(texto, collapse = "\n")

			# limpiamos líneas vacías
			lineas <- strsplit(texto, "\n")[[1]]
			lineas <- trimws(lineas)
			lineas <- lineas[lineas != ""]
			texto <- paste(lineas, collapse = "\n")

			prompt <- paste0(
				"Extrae libros del texto.\n",
				"Devuelve SOLO JSON.\n",
				"Formato:\n",
				"[{\"isbn\":\"\",\"titulo\":\"\",\"autor\":\"\",\"cantidad\":0}]\n\n",
				"Texto:\n",
				texto
			)

			respuesta <- tryCatch(
				openai_text(prompt, temperature = 0),
				error = function(e) {
					showNotification(
						"Error consultando IA. Intente nuevamente.",
						type = "error"
					)
					return(NULL)
				}
			)

			if (is.null(respuesta)) {
				removeModal()
				return()
			}


			removeModal()  # <- cerrar modal de espera

			showModal(modalDialog(
				title = "Respuesta IA",
				tags$textarea(style="width:100%;height:400px;", respuesta),
				easyClose = TRUE,
				footer = modalButton("Cerrar")
			))

			# quitar bloque ```json si viene
			respuesta_limpia <- gsub("```json|```", "", respuesta)
			respuesta_limpia <- trimws(respuesta_limpia)

			libros <- jsonlite::fromJSON(respuesta_limpia)

			# cantidad a numérico
			libros$cantidad <- suppressWarnings(
				as.numeric(gsub("[^0-9]", "", libros$cantidad))
			)

			libros$cantidad[is.na(libros$cantidad) | libros$cantidad == 0] <- 1


			# ISBN limpio (SIN guiones, solo números y X)
			libros$isbn <- sapply(libros$isbn, normalizar_isbn)

			# quitar vacíos y duplicados por ISBN
			libros <- libros[libros$isbn != "", ]
			libros <- libros[!duplicated(libros$isbn), ]

			# guardar en memoria del módulo
			libros_pdf(libros)
			indice_pdf(1)


			# mostrar el primer libro
			if (nrow(libros) > 0) {

				libro <- libros[1, ]
				bodegas <- get_bodegas()

				#  buscar si ya existe en BD
				isbn_busqueda <- normalizar_isbn(libro$isbn)
				existente <- get_libro_por_isbn(isbn_busqueda)

				# valores por defecto
				titulo_val <- libro$titulo
				autor_val <- libro$autor
				precio_val <- 0
				editorial_val <- ""
				bodega1_val <- NULL
				bodega2_val <- ""

				# si existe, usar datos de BD
				if (!is.null(existente)) {
					titulo_val <- existente$TITULO
					autor_val <- existente$AUTOR
					precio_val <- existente$PRECIO
					editorial_val <- existente$EDITORIAL
					bodega1_val <- existente$BODEGA
					bodega2_val <- existente[["BODEGA ADICIONAL"]]
				}


				showModal(
					modalDialog(
						title = "Libro detectado (PDF)",

						textInput(ns("pdf_isbn"), "ISBN", value = libro$isbn),
						textInput(ns("pdf_titulo"), "Título", value = titulo_val),
						textInput(ns("pdf_autor"), "Autor", value = autor_val),

						numericInput(
							ns("pdf_cantidad"),
							"Cantidad a agregar",
							value = libro$cantidad,
							min = 0
						),

						numericInput(ns("pdf_precio"), "Precio", value = precio_val),
						textInput(ns("pdf_editorial"), "Editorial", value = editorial_val),

						selectInput(
							ns("pdf_bodega1"),
							"Bodega principal",
							choices = bodegas,
							selected = bodega1_val
						),

						selectInput(
							ns("pdf_bodega2"),
							"Bodega adicional",
							choices = c("", bodegas),
							selected = bodega2_val
						),
						footer = tagList(
							modalButton("Cancelar"),
							actionButton(
								ns("confirmar_pdf_libro"),
								"Guardar libro",
								class = "btn btn-success"
							)
						),

						size = "l",
						easyClose = FALSE
					)
				)

				return()  # <-- CLAVE: evita que se muestre otro modal encima
			}

			showNotification("No se detectaron libros en el PDF.", type = "error")



		})

		observeEvent(input$confirmar_pdf_libro, {

			archivar_libro_manual(
				isbn = input$pdf_isbn,
				titulo = input$pdf_titulo,
				precio = input$pdf_precio,
				cantidad_agregar = input$pdf_cantidad,
				editorial = input$pdf_editorial,
				autor = input$pdf_autor,
				bodega = input$pdf_bodega1,
				bodega_adicional = input$pdf_bodega2
			)

			libros <- libros_pdf()
			i <- indice_pdf() + 1

			if (i > nrow(libros)) {
				removeModal()

				showModal(modalDialog(
					title = "Factura procesada",
					paste("Se procesaron", nrow(libros), "libros."),
					easyClose = TRUE,
					footer = modalButton("Cerrar")
				))

				return()
			}

			indice_pdf(i)

			libro_pdf <- libros[i, ]
			bodegas <- get_bodegas()

			isbn_busqueda <- normalizar_isbn(libro_pdf$isbn)
			existente <- get_libro_por_isbn(isbn_busqueda)


			# valores por defecto (libro nuevo)
			titulo_val <- libro_pdf$titulo
			autor_val <- libro_pdf$autor
			precio_val <- 0
			editorial_val <- ""
			bodega1_val <- NULL
			bodega2_val <- ""

			# si existe, cargar datos actuales
			if (!is.null(existente)) {
				titulo_val <- existente$TITULO
				autor_val <- existente$AUTOR
				precio_val <- existente$PRECIO
				editorial_val <- existente$EDITORIAL
				bodega1_val <- existente$BODEGA
				bodega2_val <- existente[["BODEGA ADICIONAL"]]
			}

			showModal(
				modalDialog(
					title = paste("Libro", i, "de", nrow(libros)),

					textInput(ns("pdf_isbn"), "ISBN", value = libro_pdf$isbn),
					textInput(ns("pdf_titulo"), "Título", value = titulo_val),
					textInput(ns("pdf_autor"), "Autor", value = autor_val),

					numericInput(
						ns("pdf_cantidad"),
						"Cantidad a agregar",
						value = libro_pdf$cantidad,
						min = 0
					),

					numericInput(ns("pdf_precio"), "Precio", value = precio_val),
					textInput(ns("pdf_editorial"), "Editorial", value = editorial_val),

					selectInput(
						ns("pdf_bodega1"),
						"Bodega principal",
						choices = bodegas,
						selected = bodega1_val
					),

					selectInput(
						ns("pdf_bodega2"),
						"Bodega adicional",
						choices = c("", bodegas),
						selected = bodega2_val
					),

					footer = tagList(
						modalButton("Cancelar"),
						actionButton(ns("confirmar_pdf_libro"),
						"Guardar libro",
						class = "btn btn-success")
					),

					size = "l",
					easyClose = FALSE
				)
			)

		})

		inventario_data <- reactive({
			get_inventario_completo()
		})

		totales <- reactive({
			df <- inventario_data()

			list(
				activos = sum(df[["TOTAL ACTIVOS  $"]], na.rm = TRUE),
				clientes = sum(df[["TOTAL CLIENTES $"]], na.rm = TRUE),
				disponible = sum(df[["DISPONIBLE EN $"]], na.rm = TRUE)
			)
		})

		output$stat_activos <- renderText({
			paste0("$ ", format(totales()$activos, big.mark = ","))
		})

		output$stat_clientes <- renderText({
			paste0("$ ", format(totales()$clientes, big.mark = ","))
		})

		output$stat_disponible <- renderText({
			paste0("$ ", format(totales()$disponible, big.mark = ","))
		})

		output$grafico_finanzas <- renderPlot({
			t <- totales()

			valores <- c(t$disponible, t$clientes)
			nombres <- c("Disponible", "Clientes")

			barplot(
				valores,
				names.arg = nombres,
				col = c("#198754", "#ffc107"),
				main = "Distribución financiera",
				ylab = "Monto ($)"
			)
		})


		# % libros sin inventario
		output$porc_sin_inv <- renderText({

			df <- inventario_data()

			total <- nrow(df)
			if (total == 0) return("0 %")

			sin_inv <- nrow(df[df[["TOTAL LIBROS"]] == 0, ])

			paste0(round(sin_inv / total * 100, 1), " %")
		})
		
		# % libros NO disponibles
		porc_no_disponibles <- reactive({
			df <- inventario_data()
			total <- nrow(df)
			if (total == 0) return(0)

			no_disp <- nrow(df[df[["TOTAL DISPONIBLE"]] == 0, ])
			round(no_disp / total * 100, 1)
		})

		# % libros en riesgo (>50% en clientes)
		porc_riesgo <- reactive({
			df <- inventario_data()
			total <- nrow(df)
			if (total == 0) return(0)

			no_disp <- nrow(
				df[df[["TOTAL DISPONIBLE"]] == 0 & df[["TOTAL LIBROS"]] > 0, ]
			)

			round(no_disp / total * 100, 3)
		})

		
		# % libros NO disponibles
		output$box_no_disp <- renderText({
			paste0(porc_no_disponibles(), " %")
		})

		# % libros en riesgo (>50% en clientes)
		output$box_riesgo <- renderText({
			paste0(porc_riesgo(), " %")
		})



		output$tabla_sin_inv <- DT::renderDT({
			DT::datatable(
				inventario_data()[inventario_data()[["TOTAL LIBROS"]] == 0, c("ISBN","TITULO")],
				options = list(pageLength = 5, autoWidth = TRUE),
				rownames = FALSE
			)
		})

		output$descargar_sin_inv <- downloadHandler(
			filename = function() {
				"libros_sin_inventario.xlsx"
			},
			content = function(file) {
				df <- inventario_data()
				df <- df[df[["TOTAL LIBROS"]] == 0, c("ISBN", "TITULO")]
				df <- fix_encoding(df)
				openxlsx::write.xlsx(df, file)
			}
		)
		
		output$tabla_no_disp <- DT::renderDT({
			df <- inventario_data()
			df_filtrado <- df[df[["TOTAL DISPONIBLE"]] == 0 & df[["TOTAL LIBROS"]] > 0,	c("ISBN", "TITULO")]

			DT::datatable(
				df_filtrado,
				rownames = FALSE,
				options = list(pageLength = 5, autoWidth = TRUE)
			)
		})


		output$descargar_no_disp <- downloadHandler(
			filename = function() {
				"libros_no_disponibles.xlsx"
			},
			content = function(file) {
				df <- inventario_data()
				df <- df[df[["TOTAL DISPONIBLE"]] == 0, c("ISBN", "TITULO")]
				df <- fix_encoding(df)
				openxlsx::write.xlsx(df, file)
			}
		)


		clientes_mayor_50 <- reactive({
			df <- inventario_data()
			df[df[["TOTAL DISPONIBLE"]] == 0 & df[["TOTAL LIBROS"]] > 0, ]
		})

		output$tabla_clientes50 <- DT::renderDT({
			DT::datatable(
				clientes_mayor_50()[, c("ISBN", "TITULO")],
				rownames = FALSE,
				options = list(pageLength = 5, autoWidth = TRUE)
			)
		})

		output$descargar_clientes50 <- downloadHandler(
			filename = function() "libros_en_riesgo.xlsx",
			content = function(file) {
				df <- clientes_mayor_50()[, c("ISBN", "TITULO")]
				df <- fix_encoding(df)
				openxlsx::write.xlsx(df, file)
			}
		)

		
		
	})
}
