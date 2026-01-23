login_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(
      class = "container min-vh-100 d-flex flex-column align-items-center justify-content-center",

      # logo
      img(src = "logo.png", height = "90px", class = "mb-4"),

      # card de login
      div(
        class = "card shadow-sm p-4",
        style = "max-width: 400px; width: 100%;",

        h3("Bienvenido", class = "text-center mb-3"),

        textInput(ns("username"), "Usuario"),
        passwordInput(ns("password"), "Contraseña"),

        actionButton(
          ns("login_btn"),
          "Ingresar",
          class = "btn btn-success w-100 mt-3"
        ),

        div(class = "text-danger mt-2 text-center", textOutput(ns("login_message")))
      )
    )
  )
}
