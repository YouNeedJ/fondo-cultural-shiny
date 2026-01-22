# app/services/session.R
# Manejo de sesiones de usuario (sin UI)

library(uuid)

# Tiempo máximo de inactividad (segundos)
SESSION_TIMEOUT <- 30 * 60  # 30 minutos

# Genera una nueva sesión
create_session <- function(usuario) {
  list(
    session_id = UUIDgenerate(),
    last_activity = Sys.time()
  )
}

# Determina si una sesión está expirada
is_session_expired <- function(last_activity) {
  if (is.null(last_activity)) return(TRUE)

  difftime(Sys.time(), as.POSIXct(last_activity), units = "secs") > SESSION_TIMEOUT
}
