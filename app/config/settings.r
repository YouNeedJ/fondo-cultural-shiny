# app/config/settings.R
# Configuración global de la aplicación
# - En local: carga .env si existe
# - En producción (DO): usa variables de entorno del sistema

# -----------------------------------------
# 1. Cargar .env SOLO si existe (local)
# -----------------------------------------
if (file.exists("../.env")) {
  library(dotenv)
  load_dot_env("../.env")
}

# -----------------------------------------
# 2. Leer configuración
# -----------------------------------------
CONFIG <- list(
  app = list(
    env = Sys.getenv("APP_ENV", unset = NA)
  ),

  mongo = list(
    uri = Sys.getenv("MONGO_URI", unset = NA),
    db  = Sys.getenv("MONGO_DB", unset = NA)
  ),

  openai = list(
    api_key = Sys.getenv("OPENAI_API_KEY", unset = NA),
    model   = Sys.getenv("OPENAI_MODEL", "gpt-4.1-mini")
  )
)

# -----------------------------------------
# 3. Validación estricta
# -----------------------------------------
missing <- c()

if (is.na(CONFIG$app$env))           missing <- c(missing, "APP_ENV")
if (is.na(CONFIG$mongo$uri))         missing <- c(missing, "MONGO_URI")
if (is.na(CONFIG$mongo$db))          missing <- c(missing, "MONGO_DB")
if (is.na(CONFIG$openai$api_key))    missing <- c(missing, "OPENAI_API_KEY")

if (length(missing) > 0) {
  stop(
    "Faltan variables de entorno: ",
    paste(missing, collapse = ", ")
  )
}
