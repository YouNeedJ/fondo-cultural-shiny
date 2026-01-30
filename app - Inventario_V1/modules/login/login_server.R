# app/modules/login/login_server.R
# Server del módulo de login

login_server <- function(id, app_state) {

  moduleServer(id, function(input, output, session) {

    message <- reactiveVal("")

    observeEvent(input$login_btn, {

      message("")

      req(input$username, input$password)

      res <- authenticate_user(
        username       = input$username,
        password_plain = input$password
      )

      if (!res$success) {

        msg <- switch(
          res$reason,
          "user_not_found"  = "El usuario no existe.",
          "wrong_password"  = "La contraseña es incorrecta.",
          "session_active"  = "Ya tienes una sesión activa en otro lugar.",
          "Error desconocido."
        )

        message(msg)
        return()
      }

      # Login exitoso actualizar estado global
      app_state$logged     <- TRUE
      app_state$user       <- res$user$usuario
      app_state$rol        <- res$user$rol
      app_state$session_id <- res$session$session_id
    })

    output$login_message <- renderText({
      message()
    })
	
	
  })
}
