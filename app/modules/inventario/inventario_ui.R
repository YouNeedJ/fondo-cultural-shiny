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
				column(4,textInput(ns("isbn"), "ISBN", placeholder = "978...",width="100%")),
				column(4,textInput(ns("titulo"), "Título", placeholder = "Nombre del libro",width="100%")),
				column(4,numericInput(ns("cant_agregar"), "Cantidad de libros a agregar o quitar", value = 1, min = 1,width="100%")),
				column(4,numericInput(ns("precio"), "Precio", value = 0, min = 0,width="100%")),
				column(4,textInput(ns("editorial"), "Editorial", placeholder = "Editorial",width="100%")),
				column(4,textInput(ns("autor"), "Autor", placeholder = "Autor",width="100%"))
			),

			fluidRow(
				column(4, div(class="pt-4",
					actionButton(ns("guardar"), "Guardar / Sumar", class = "btn btn-success w-100")
				)),
				column(4, ),
				column(4, div(class="pt-4",
					actionButton(ns("eliminar"), "Eliminar libro",
					class = "btn btn-danger w-100")
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
				p("Suba factura o lista de libros en PDF", class="text-muted")
			),

			fluidRow(
				column(4),
				column(4,
					fileInput(ns("pdf_file"),
					"Seleccione PDF",
					accept = ".pdf",
					width = "100%"),
					br(),
					actionButton(ns("procesar_pdf"),
					"Leer PDF",
					class = "btn btn-primary w-100")
				),
				column(4)
			)
		),
		
		nav_panel(
			title = "Estadísticas",
			
			div(class="text-center my-4",
				h4("Estadisticas Basicas", class="fw-bold"),
				p("Estado actual del inventario", class="text-muted")
			),

			# ==== TARJETAS SUPERIORES ====
			fluidRow(
				column(4,
					bslib::card(
						class = "text-center",
						h6("Total activos", class = "text-muted"),
						h2(textOutput(ns("stat_activos")), class = "fw-bold text-primary")
					)
				),
				column(4,
					bslib::card(
						class = "text-center",
						h6("Activos en clientes", class = "text-muted"),
						h2(textOutput(ns("stat_clientes")), class = "fw-bold text-warning")
					)
				),
				column(4,
					bslib::card(
						class = "text-center",
						h6("Activos disponible", class = "text-muted"),
						h2(textOutput(ns("stat_disponible")), class = "fw-bold text-success")
					)
				)
			),

			br(),

			bslib::card(
				h5("Distribución financiera"),
				plotOutput(ns("grafico_finanzas"), height = "300px")
			),

			br(),

			fluidRow(
				column(6,
					bslib::card(
						h6("Libros sin inventario (no se tiene existencia en bodega ni en cliente)", class = "text-muted"),
						h2(textOutput(ns("porc_sin_inv")), class = "text-danger"),
						downloadButton(ns("descargar_sin_inv"), "Descargar",
								   class = "btn btn-outline-danger mb-2"),
						DT::DTOutput(ns("tabla_sin_inv"))
					)
				),
				column(6,
					bslib::card(
						h6("Libros no disponibles (no se tienen en bodega)", class = "text-muted"),
						h2(textOutput(ns("box_no_disp")), class = "text-danger"),
						downloadButton(ns("descargar_no_disp"), "Descargar",
								   class = "btn btn-outline-danger mb-2"),
						DT::DTOutput(ns("tabla_no_disp"))
					)
				)
			),

			br(),

			bslib::card(
				h6("Libros en riesgo (no se tiene en bodega pero si en cliente)", class = "text-muted"),
				h2(textOutput(ns("box_riesgo")), class = "text-warning"),
				downloadButton(ns("descargar_clientes50"), "Descargar",class = "btn btn-outline-warning mb-2"),
				DT::DTOutput(ns("tabla_clientes50"))
			)
		)
	)
}
