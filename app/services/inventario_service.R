# app/services/inventario_service.R
# Lógica de negocio Inventario (NO Shiny)

# Requiere mongo.R ya inicializado (init_mongo(CONFIG))
# Colecciones esperadas: "inventario" y "bodegas" (confirmaremos)

.get_inventario_collection <- function() {
  get_collection("inventario")
}

.get_bodegas_collection <- function() {
  get_collection("bodegas")
}

# Devuelve vector de bodegas (strings) para selectize
get_bodegas <- function() {
  bodegas <- .get_bodegas_collection()

  res <- bodegas$find(fields = '{"Bodega": 1, "_id": 0}')

  if (nrow(res) == 0) return(character(0))

  x <- res$Bodega
  x <- x[!is.na(x)]
  x <- trimws(x)
  x <- x[x != ""]
  sort(unique(x))
}

# Buscar un libro por ISBN
get_libro_por_isbn <- function(isbn) {
  inv <- .get_inventario_collection()
  res <- inv$find(query = jsonlite::toJSON(list(ISBN = isbn), auto_unbox = TRUE), limit = 1)
  if (nrow(res) == 0) return(NULL)
  res
}

# Calcula campos derivados (según tu regla)
.recalcular_campos <- function(precio, total_libros, libro_en_cliente, total_disponible) {
  precio <- as.numeric(precio)
  total_libros <- as.numeric(total_libros)
  libro_en_cliente <- as.numeric(libro_en_cliente)
  total_disponible <- as.numeric(total_disponible)

  list(
    "TOTAL ACTIVOS  $" = precio * total_libros,
    "TOTAL CLIENTES $" = precio * libro_en_cliente,
    "DISPONIBLE EN $"  = precio * total_disponible
  )
}

# Archivar libro manual:
# - Si existe por ISBN: suma a "CANTIDAD BODEGA"
# - Si no existe: inserta nuevo con MOSTRARIO/IMPERFECTOS = 0
archivar_libro_manual <- function(isbn, titulo, precio, cantidad_agregar, editorial, autor,
                                  bodega, bodega_adicional = NULL) {

  if (is.null(isbn) || trimws(isbn) == "") stop("ISBN es obligatorio")
  if (is.null(titulo) || trimws(titulo) == "") stop("TITULO es obligatorio")

  cantidad_agregar <- as.numeric(cantidad_agregar)
  #if (is.na(cantidad_agregar) || cantidad_agregar <= 0) stop("Cantidad a agregar debe ser > 0")
  if(is.na(cantidad_agregar)) {stop("Cantidad inválida")}

  inv <- .get_inventario_collection()
  existente <- inv$find(query = jsonlite::toJSON(list(ISBN = isbn), auto_unbox = TRUE), limit = 1)

  if (nrow(existente) == 0) {
    # NUEVO
    # Campos en cero cuando es nuevo:
    mostrario <- 0
    imperfecto <- 0
    imperfecto_mostrario <- 0

    cant_bodega <- cantidad_agregar
    total_libros <- cant_bodega
    libro_en_cliente <- 0
    total_disponible <- cant_bodega

    derivados <- .recalcular_campos(precio, total_libros, libro_en_cliente, total_disponible)

    
	
	precio_num <- suppressWarnings(as.numeric(precio))
	if (is.na(precio_num)) {
	  precio_num <- as.numeric(actual[["PRECIO"]])
	}
	doc <- list(
	  "ISBN" = isbn,
	  "TITULO" = titulo,
	  "PRECIO" = precio_num,
	  "BODEGA" = bodega,
	  "BODEGA ADICIONAL" = if (!is.null(bodega_adicional) && trimws(bodega_adicional) != "") bodega_adicional else "",
	  "MOSTRARIO" = mostrario,
	  "LIBRO IMPERFECTO" = imperfecto,
	  "LIBRO IMPERFECTO MOSTRARIO" = imperfecto_mostrario,
	  "CANTIDAD BODEGA" = cant_bodega,
	  "TOTAL LIBROS" = total_libros,
	  "LIBRO EN CLIENTE" = libro_en_cliente,
	  "TOTAL DISPONIBLE" = total_disponible,
	  "EDITORIAL" = editorial,
	  "AUTOR" = autor
	)


    doc <- c(doc, derivados)

    inv$insert(jsonlite::toJSON(doc, auto_unbox = TRUE))
    return(list(action = "insert", isbn = isbn))
  }

  # EXISTE: sumar a CANTIDAD BODEGA (y recalcular)
  actual <- existente[1, , drop = FALSE]

  cant_actual <- as.numeric(actual[["CANTIDAD BODEGA"]])
  if (is.na(cant_actual)) cant_actual <- 0

  cant_nueva <- cant_actual + cantidad_agregar

  # Mantener MOSTRARIO/IMPERFECTOS como están (no tocar)
  mostrario <- as.numeric(actual[["MOSTRARIO"]]); if (is.na(mostrario)) mostrario <- 0
  imperfecto <- as.numeric(actual[["LIBRO IMPERFECTO"]]); if (is.na(imperfecto)) imperfecto <- 0
  imperfecto_mostrario <- as.numeric(actual[["LIBRO IMPERFECTO MOSTRARIO"]]); if (is.na(imperfecto_mostrario)) imperfecto_mostrario <- 0
  libro_en_cliente <- as.numeric(actual[["LIBRO EN CLIENTE"]]); if (is.na(libro_en_cliente)) libro_en_cliente <- 0

	total_libros_actual <- as.numeric(actual[["TOTAL LIBROS"]])
	if (is.na(total_libros_actual)) total_libros_actual <- 0

	total_disponible_actual <- as.numeric(actual[["TOTAL DISPONIBLE"]])
	if (is.na(total_disponible_actual)) total_disponible_actual <- 0

	total_libros <- total_libros_actual + cantidad_agregar
	total_disponible <- total_disponible_actual + cantidad_agregar


  derivados <- .recalcular_campos(actual[["PRECIO"]], total_libros, libro_en_cliente, total_disponible)

	update_set <- data.frame(
	  "TITULO" = ifelse(length(titulo) == 0, "", titulo),
	  "PRECIO" = as.numeric(precio)[1],
	  "EDITORIAL" = ifelse(length(editorial) == 0, "", editorial),
	  "AUTOR" = ifelse(length(autor) == 0, "", autor),
	  "BODEGA" = ifelse(length(bodega) == 0, "", bodega),
	  "BODEGA ADICIONAL" = ifelse(
		is.null(bodega_adicional) ||
		length(bodega_adicional) == 0 ||
		trimws(bodega_adicional) == "",
		"",
		bodega_adicional
	  ),
	  "CANTIDAD BODEGA" = cant_nueva,
	  "TOTAL LIBROS" = total_libros,
	  "TOTAL DISPONIBLE" = total_disponible,
	  stringsAsFactors = FALSE
	)



  update_set <- c(update_set, derivados)

	inv$update(
	  query  = jsonlite::toJSON(list(ISBN = isbn), auto_unbox = TRUE),
	  update = jsonlite::toJSON(
		list("$set" = if (is.data.frame(update_set))
		  as.list(update_set[1, ])
		else
		  update_set
		),
		auto_unbox = TRUE
	  ),
	  upsert = FALSE
	)


  return(list(action = "sum_bodega", isbn = isbn))
}
