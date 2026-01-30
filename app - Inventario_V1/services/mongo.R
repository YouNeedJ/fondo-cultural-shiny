# app/services/mongo.R
# Servicio centralizado de acceso a MongoDB

library(mongolite)

.mongo_cfg <- NULL

# Inicializa la configuración de Mongo (una sola vez)
init_mongo <- function(config) {
  if (!is.null(.mongo_cfg)) return(invisible(TRUE))

  .mongo_cfg <<- list(
    uri = config$mongo$uri,
    db  = config$mongo$db
  )

  invisible(TRUE)
}

# Obtiene una colección
get_collection <- function(collection_name) {
  if (is.null(.mongo_cfg)) {
    stop("Mongo no inicializado. Llama init_mongo(CONFIG) primero.")
  }

  mongo(
    collection = collection_name,
    db  = .mongo_cfg$db,
    url = .mongo_cfg$uri
  )
}
