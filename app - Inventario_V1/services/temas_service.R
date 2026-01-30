# app/services/temas_service.R
# CRUD para colección "temas" (Mongo)
# - get_tema_por_codigo()
# - existe_tema()
# - save_tema() -> inserta o actualiza
# - delete_tema_por_codigo()
# - list_temas()

library(jsonlite)
source("services/mongo.R")

.get_temas_collection <- function() {
  get_collection("temas")
}

get_tema_por_codigo <- function(codigo) {
  temas <- .get_temas_collection()

  codigo <- as.integer(codigo)
  if (is.na(codigo)) return(NULL)

  df <- temas$find(
    query = toJSON(list(CODIGO = codigo), auto_unbox = TRUE),
    limit = 1
  )

  if (nrow(df) == 0) return(NULL)
  df
}

existe_tema <- function(codigo) {
  !is.null(get_tema_por_codigo(codigo))
}

save_tema <- function(df) {
  # df debe ser data.frame de 1 fila con CODIGO (int) y TEMA (string)
  temas <- .get_temas_collection()

  if (!is.data.frame(df) || nrow(df) != 1) {
    stop("save_tema espera un data.frame de 1 fila")
  }

  if (!("CODIGO" %in% names(df)) || !("TEMA" %in% names(df))) {
    stop("Faltan columnas: CODIGO y/o TEMA")
  }

  df$CODIGO <- as.integer(df$CODIGO)
  if (is.na(df$CODIGO)) stop("CODIGO debe ser numérico")

  df$TEMA <- toupper(trimws(df$TEMA))
  if (is.na(df$TEMA) || df$TEMA == "") stop("TEMA es requerido")

  if (!existe_tema(df$CODIGO)) {
    # INSERT
    temas$insert(df)
    return(list(success = TRUE, action = "insert"))
  }

  # UPDATE (no tocamos CODIGO, solo TEMA)
  temas$update(
    query = toJSON(list(CODIGO = df$CODIGO), auto_unbox = TRUE),
    update = toJSON(list("$set" = list(TEMA = df$TEMA)), auto_unbox = TRUE)
  )

  list(success = TRUE, action = "update")
}

delete_tema_por_codigo <- function(codigo) {
  temas <- .get_temas_collection()

  codigo <- as.integer(codigo)
  if (is.na(codigo)) stop("CODIGO debe ser numérico")

  temas$remove(
    query = toJSON(list(CODIGO = codigo), auto_unbox = TRUE)
  )

  list(success = TRUE, action = "delete")
}

list_temas <- function(limit = 500) {
  temas <- .get_temas_collection()

  df <- temas$find(
    query = "{}",
    sort = '{"CODIGO": 1}',
    limit = limit
  )

  # para tablas (opcional): quitamos _id si viene
  if (ncol(df) > 0 && "_id" %in% names(df)) df$`_id` <- NULL
  df
}
