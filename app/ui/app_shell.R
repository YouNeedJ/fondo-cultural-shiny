app_shell_ui <- function(app_state) {
  tagList(
    div(
      class = "bg-primary text-white p-3 d-flex justify-content-between align-items-center",

      div(
        class = "d-flex align-items-center",
        img(src = "logo.png", height = "50px", class = "me-2"),
        h4("Fondo Cultural", class = "mb-0")
      ),

      actionButton("logout", "Cerrar sesión", class = "btn btn-outline-light")
    ),

    div(class = "container-fluid mt-3",
		tags$ul(
	  class = "nav nav-pills mb-3",
	  tags$li(class = "nav-item",
		actionLink("nav_clientes", "Clientes", class = "nav-link")
	  ),
	  tags$li(class = "nav-item",
		actionLink("nav_temas", "Temas", class = "nav-link")
	  )
	),


      div(id = "main_content")
    )
  )
}
