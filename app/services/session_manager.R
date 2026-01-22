# app/services/session_manager.R
# Manejo de ciclo de vida de sesión

#source("services/mongo.R")
source("services/session.R")

# Cierra la sesión de un usuario
logout_user <- function(username) {

  usuarios <- get_collection("Usuarios")

  usuarios$update(
    query = jsonlite::toJSON(list(Usuario = username), auto_unbox = TRUE),
    update = '{"$set":{"Estado":0,"session_id":null,"last_activity":null}}'
  )

  invisible(TRUE)
}

# Actualiza actividad de la sesión (heartbeat)
touch_session <- function(username, session_id) {

  usuarios <- get_collection("Usuarios")

  usuarios$update(
    query = jsonlite::toJSON(
      list(
        Usuario = username,
        session_id = session_id,
        Estado = 1
      ),
      auto_unbox = TRUE
    ),
    update = jsonlite::toJSON(
      list(
        "$set" = list(
          last_activity = Sys.time()
        )
      ),
      auto_unbox = TRUE
    )
  )

  invisible(TRUE)
}

# Verifica si una sesión sigue siendo válida
is_session_valid <- function(username, session_id) {

  usuarios <- get_collection("Usuarios")

  user <- usuarios$find(
    query = jsonlite::toJSON(
      list(
        Usuario = username,
        session_id = session_id,
        Estado = 1
      ),
      auto_unbox = TRUE
    ),
    limit = 1
  )

  if (nrow(user) == 0) return(FALSE)

  !is_session_expired(user$last_activity)
}
