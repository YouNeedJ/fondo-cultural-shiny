# app/ui/app_shell.R
# Shell principal de la aplicación (usuario logueado)

app_shell_ui <- function(app_state) {

  fluidPage(

    # -------------------------
    # Header
    # -------------------------
    div(
      class = "app-header",
      div(
        class = "app-header-left",
        tags$img(
          src = "logo.png",
          height = "80px",
          style = "margin-right:10px;"
        )#,
        # span(
          # class = "app-title",
          # "Fondo Cultural"
        # )
      ),
      div(
        class = "app-header-right",
        actionButton(
          "logout",
          "Cerrar sesión",
          class = "btn btn-outline-light"
        )
      )
    ),

    # -------------------------
    # Layout principal
    # -------------------------
	sidebarLayout(

		sidebarPanel(
			width = 3,
			h4("Menú"),

			tags$ul(
				class = "nav nav-pills nav-stacked",
				tags$li(
					class = "active",
					tags$a("Clientes")
				)
			)
		),


		mainPanel(
			width = 9,
			clientes_ui("clientes")
		)

	)
  )
}
