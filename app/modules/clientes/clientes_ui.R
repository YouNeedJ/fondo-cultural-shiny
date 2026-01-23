# app/modules/clientes/clientes_ui.R

clientes_ui <- function(id) {
	ns <- NS(id)

	bslib::navset_tab(

		nav_panel(
			title = "Gestión de Clientes",

			h3("Gestión de Clientes"),
			p("Crear, consultar y actualizar información de clientes"),

			fluidRow(
				column(
					12,

					textInput(ns("id_cliente"), "Identificación",placeholder=" "),
					textInput(ns("nombre"), "Nombre del Cliente",placeholder=" "),
					textInput(ns("direccion"), "Dirección",placeholder=" "),
					textInput(ns("correo"), "Correo electrónico",placeholder=" "),

					selectizeInput(
					  ns("ciudad"),
					  "Ciudad",
					  choices = NULL,
					  options = list(create = TRUE, placeholder = "Seleccione o escriba una ciudad")
					),

					selectInput(
						ns("tipo_persona"),
						"Tipo de cliente",
						choices = c("CLIENTE", "VENDEDOR")
					),

					numericInput(
						ns("descuento"),
						"Descuento (%)",
						value = 0,
						min = 0,
						max = 100
					),

					br(),

					actionButton(
						ns("buscar"),
						"Buscar",
						icon = icon("search"),
						class = "btn-primary"
					),

					actionButton(
						ns("guardar"),
						"Guardar",
						icon = icon("save"),
						class = "btn-success"
					),
					
					actionButton(
						ns("eliminar"),
						"Eliminar",
						icon = icon("trash"),
						class = "btn-danger"
					)

				)
			)
		),

		nav_panel(
			title = "Análisis de Clientes",

			h3("Análisis de Clientes"),
			p("Distribución y comportamiento de la base de clientes"),
			
			fluidRow(
				column(6, plotlyOutput(ns("plot_ciudad"))),
				column(6, plotlyOutput(ns("plot_tipo")))
			),
			
			hr(),

# --- Filtros ---
			fluidRow(
				column(
					3,
					h5("Filtros"),

					selectizeInput(
						ns("filtro_ciudad"),
						"Ciudad",
						choices = NULL,
						multiple = TRUE,
						options = list(placeholder = "Seleccione una o más ciudades")
					),

					selectInput(
						ns("filtro_tipo"),
						"Tipo de cliente",
						choices = c("CLIENTE", "VENDEDOR"),
						multiple = TRUE
					),

					actionButton(
						ns("aplicar_filtros"),
						"Aplicar filtros",
						icon = icon("filter"),
						class = "btn-primary"
					)
				)
			),

			br(),

			# --- Tabla colapsable ---
			bslib::accordion(
				bslib::accordion_panel(
					title = "Ver tabla de resultados",
					DT::DTOutput(ns("tabla_clientes"))
				)
			)


			
		)
	)
}
