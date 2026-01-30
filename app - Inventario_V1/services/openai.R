# app/services/openai.R
# Servicio centralizado de acceso a OpenAI (API /responses)

library(httr2)
library(jsonlite)

.openai_cfg <- NULL

# Inicialización del servicio
init_openai <- function(config) {
  if (!is.null(.openai_cfg)) return(invisible(TRUE))

  .openai_cfg <<- list(
    api_key = config$openai$api_key,
    model   = config$openai$model
  )

  invisible(TRUE)
}

# Llamada simple a OpenAI
openai_text <- function(prompt, temperature = 0.2) {
  if (is.null(.openai_cfg)) {
    stop("OpenAI no inicializado. Llama init_openai(CONFIG) primero.")
  }

  req <- request("https://api.openai.com/v1/responses") |>
    req_headers(
      Authorization = paste("Bearer", .openai_cfg$api_key),
      `Content-Type` = "application/json"
    ) |>
    req_body_json(list(
      model = .openai_cfg$model,
      input = prompt,
      temperature = temperature
    ))

  resp <- req_perform(req)
  body <- resp_body_json(resp)

  if (!is.null(body$output) &&
      length(body$output) > 0 &&
      !is.null(body$output[[1]]$content) &&
      length(body$output[[1]]$content) > 0 &&
      !is.null(body$output[[1]]$content[[1]]$text)) {

    return(body$output[[1]]$content[[1]]$text)
  }

  NULL
}
