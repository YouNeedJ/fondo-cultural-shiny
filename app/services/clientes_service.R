# app/services/clientes_service.R
# Servicio de negocio para Clientes
# NO contiene Shiny
# USA mongo.R

library(stringi)

source("services/mongo.R")

# --------------------------------------------------
# Utilidades internas
# --------------------------------------------------

normalize_text <- function(x) {
  if (is.null(x) || is.na(x)) return(NA)
  x <- trimws(x)
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  toupper(x)
}

# --------------------------------------------------
# Obtener colección
# --------------------------------------------------

.get_clientes_collection <- function() {
  get_collection("clientes")
}

# --------------------------------------------------
# Buscar cliente por ID_CLIENTE
# --------------------------------------------------

get_cliente_por_id <- function(id_cliente) {
  clientes <- .get_clientes_collection()

  res <- clientes$find(
    query = sprintf('{"ID_CLIENTE":"%s"}', id_cliente),
    limit = 1
  )

  if (nrow(res) == 0) return(NULL)

  # Convertir descuento a %
  res$DESC_ <- res$DESC_ * 100
  res
}

# --------------------------------------------------
# Crear o actualizar cliente
# --------------------------------------------------

save_cliente <- function(data) {
  clientes <- .get_clientes_collection()

  # Normalizaciones
  data$CIUDAD <- normalize_text(data$CIUDAD)
  data$TIPO_PERSONA <- normalize_text(data$TIPO_PERSONA)

  # Descuento: UI (0-100) -> Mongo (0-1)
  data$DESC_ <- as.numeric(data$DESC_) / 100

  # Validación
  if (!data$TIPO_PERSONA %in% c("CLIENTE", "VENDEDOR")) {
    stop("Tipo de cliente no válido")
  }

  # Convertir a data.frame (CLAVE)
  df <- as.data.frame(data, stringsAsFactors = FALSE)

  existente <- clientes$find(
    query = sprintf('{"ID_CLIENTE":"%s"}', data$ID_CLIENTE),
    limit = 1
  )

  if (nrow(existente) == 0) {
    clientes$insert(df)
    return(list(action = "insert"))
  } else {
	clientes$update(
		query = sprintf('{"ID_CLIENTE":"%s"}', data$ID_CLIENTE),
		update = jsonlite::toJSON(
			list("$set" = as.list(df[ , setdiff(names(df), "ID_CLIENTE"), drop = FALSE])),
			auto_unbox = TRUE
		)
	)

    return(list(action = "update"))
  }
}


# --------------------------------------------------
# Listado único de ciudades (para autocomplete)
# --------------------------------------------------

get_ciudades <- function() {
  clientes <- .get_clientes_collection()

  res <- clientes$aggregate('[
    { "$group": { "_id": "$CIUDAD" } },
    { "$sort": { "_id": 1 } }
  ]')

  if (nrow(res) == 0) return(character(0))
  res$`_id`
}

# --------------------------------------------------
# Gráficas
# --------------------------------------------------

clientes_por_ciudad <- function() {
  clientes <- .get_clientes_collection()

  clientes$aggregate('[
    { "$group": { "_id": "$CIUDAD", "total": { "$sum": 1 } } },
    { "$sort": { "total": -1 } }
  ]')
}

clientes_por_tipo <- function() {
  clientes <- .get_clientes_collection()

  clientes$aggregate('[
    { "$group": { "_id": "$TIPO_PERSONA", "total": { "$sum": 1 } } },
    { "$sort": { "total": -1 } }
  ]')
}

search_clientes <- function(ciudades = NULL, tipos = NULL) {

  clientes <- .get_clientes_collection()
  query <- list()

  if (!is.null(ciudades) && length(ciudades) > 0) {
    query$CIUDAD <- list(
      "$in" = unname(as.list(ciudades))
    )
  }

  if (!is.null(tipos) && length(tipos) > 0) {
    query$TIPO_PERSONA <- list(
      "$in" = unname(as.list(tipos))
    )
  }

  if (length(query) == 0) {
    clientes$find()
  } else {
    clientes$find(
      query = jsonlite::toJSON(query, auto_unbox = TRUE)
    )
  }
}


# Eliminar cliente por ID_CLIENTE
delete_cliente_por_id <- function(id_cliente) {
	clientes <- .get_clientes_collection()

	if (is.null(id_cliente) || is.na(id_cliente) || trimws(id_cliente) == "") {
		stop("ID_CLIENTE es requerido")
	}

	res <- clientes$remove(
		query = jsonlite::toJSON(list(ID_CLIENTE = id_cliente), auto_unbox = TRUE)
	)

	# mongolite devuelve un objeto con métricas; devolvemos algo simple
	list(success = TRUE, removed = TRUE, raw = res)
}


update_cliente_por_id <- function(id_cliente, data) {
  clientes <- .get_clientes_collection()

  if (is.null(id_cliente) || trimws(id_cliente) == "") {
    stop("ID_CLIENTE es requerido para actualizar")
  }

  clientes$update(
    query  = jsonlite::toJSON(list(ID_CLIENTE = id_cliente), auto_unbox = TRUE),
    update = jsonlite::toJSON(list("$set" = data), auto_unbox = TRUE)
  )

  TRUE
}

