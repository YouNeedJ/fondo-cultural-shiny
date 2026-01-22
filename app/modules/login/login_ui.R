# app/modules/login/login_ui.R
# UI del módulo de login

login_ui <- function(id) {
	ns <- NS(id)

	fluidPage(
	
		div(
			class = "login-logo",
			tags$img(
				src = "logo.png",
				alt = "Fondo Cultural",
				height = "90px"
			)
		),
		div(
			style = "max-width:400px; margin:100px auto;",

			h3("Bienvenido", style = "text-align:center;"),
			br(),  

			textInput(
				ns("username"),
				"Usuario",
				width = "100%"
			),

			passwordInput(
				ns("password"),
				"Contrasena",
				width = "100%"
			),

			br(),

			actionButton(
				ns("login_btn"),
				"Ingresar",
				class = "btn btn-success btn-block"
			),

			div(
				class = "login-message",
				textOutput(ns("login_message"))
			)
		)
	)
}
