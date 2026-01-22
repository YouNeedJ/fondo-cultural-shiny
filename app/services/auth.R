authenticate_user <- function(username, password_plain) {

library(sodium)

#source("services/mongo.R")
source("services/session.R")


  usuarios <- get_collection("Usuarios")

  # 1. Buscar usuario
  user <- usuarios$find(
    query = jsonlite::toJSON(list(Usuario = username), auto_unbox = TRUE),
    limit = 1
  )

  if (nrow(user) == 0) {
    return(list(
      success = FALSE,
      reason  = "user_not_found"
    ))
  }

  # 2. Validar contraseña
  if (!password_verify(user$Contrasena, password_plain)) {
    return(list(
      success = FALSE,
      reason  = "wrong_password"
    ))
  }

  # 3. Verificar sesión existente
  if (user$Estado == 1) {

    if (!is_session_expired(user$last_activity)) {
      return(list(
        success = FALSE,
        reason  = "session_active"
      ))
    }

    # Limpiar sesión zombie
    usuarios$update(
      query = jsonlite::toJSON(list(Usuario = username), auto_unbox = TRUE),
      update = jsonlite::toJSON(
        list(
          "$set" = list(
            Estado = 0,
            session_id = NULL,
            last_activity = NULL
          )
        ),
        auto_unbox = TRUE
      )
    )
  }

  # 4. Crear nueva sesión
  session <- create_session(username)

  usuarios$update(
    query = jsonlite::toJSON(list(Usuario = username), auto_unbox = TRUE),
    update = jsonlite::toJSON(
      list(
        "$set" = list(
          Estado = 1,
          session_id = session$session_id,
          last_activity = session$last_activity
        )
      ),
      auto_unbox = TRUE
    )
  )

  # 5. Resultado
  list(
    success = TRUE,
    reason  = "ok",
    user = list(
      usuario = user$Usuario,
      rol     = user$Rol
    ),
    session = session
  )
}
