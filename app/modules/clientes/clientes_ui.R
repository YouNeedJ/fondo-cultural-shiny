# app/modules/clientes/clientes_ui.R

clientes_ui <- function(id) {
  ns <- NS(id)

  bslib::navset_tab(
  
   id = ns("tabs_clientes"),

    nav_panel(
      title = "Gestión de Clientes",

      h3("Gestión de Clientes"),
      p("Crear, consultar y actualizar información de clientes"),

      fluidRow(
		
        

          textInput(ns("id_cliente"), "Identificación", width="100%",placeholder = " "),
          textInput(ns("nombre"), "Nombre del Cliente", width="100%",placeholder = " "),
          textInput(ns("direccion"), "Dirección", width="100%",placeholder = " "),
          textInput(ns("correo"), "Correo electrónico", width="100%",placeholder = " "),

			selectizeInput(
			  ns("ciudad"),
			  "Ciudad",
			  choices = get_ciudades(),
			  options = list(
				create = TRUE,
				placeholder = "Seleccione una ciudad"
			  ),
			  width="33%"
			),


          selectInput(
            ns("tipo_persona"),
            "Tipo de cliente",
            choices = c("CLIENTE", "VENDEDOR"),
			width="33%"
          ),

          numericInput(
            ns("descuento"),
            "Descuento (%)",
            value = 0,
            min = 0,
            max = 100,
			width="33%"
          ),

          br(),
		
		fluidRow(
		column(4,
          actionButton(
            ns("buscar"),
            "Buscar",
            icon = icon("search"),
              class = "btn btn-primary",
			width="100%"
          )
		),
		
		column(4,
          actionButton(
            ns("guardar"),
            "Guardar",
            icon = icon("save"),
            class = "btn btn-success me-3",
			width="100%"
          )
		),
		
		column(4,
          actionButton(
            ns("eliminar"),
            "Eliminar",
            icon = icon("trash"),
            class = "btn-danger me-3",
			width="100%"
          )
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

      fluidRow(
        
          h5("Filtros"),
			column(6,
			selectizeInput(
			  ns("filtro_ciudad"),
			  "Ciudad",
			  choices = get_ciudades(),
			  multiple = TRUE,
			  options = list(
				placeholder = "Seleccione una o más ciudades"
			  ),
			  width="100%"
			)),
		column(6,
          selectInput(
            ns("filtro_tipo"),
            "Tipo de cliente",
            choices = c("CLIENTE", "VENDEDOR"),
            multiple = TRUE,
			width="100%"
          )
		),
	),
	fluidRow(
	column(4),
	column(4,
          actionButton(
            ns("aplicar_filtros"),
            "Aplicar filtros",
            icon = icon("filter"),
            class = "btn-primary",
			width="100%"
          )
	),
	column(4)

     
	),

      br(),

      bslib::accordion(
        bslib::accordion_panel(
          title = "Ver tabla de resultados",
          DT::DTOutput(ns("tabla_clientes"))
        )
      )
    )
  )
}
