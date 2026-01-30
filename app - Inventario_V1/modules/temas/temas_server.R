# app/modules/temas/temas_server.R

source("app/services/temas_service.R")

temas_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    limpiar_formulario <- function() {
      updateTextInput(session, "codigo", value = NA)
      updateTextInput(session, "tema", value = NA)
    }

    # Buscar tema
    observeEvent(input$buscar, {

      req(input$codigo)

      tema <- get_tema_por_codigo(input$codigo)

      if (is.null(tema)) {
        showNotification("Tema no encontrado", type = "warning")
        limpiar_formulario()
        updateTextInput(session, "codigo", value = input$codigo)
        return()
      }

      updateTextInput(session, "codigo", value = tema$CODIGO)
      updateTextInput(session, "tema", value = tema$TEMA)
    })

    # Guardar (crear o actualizar)
    observeEvent(input$guardar, {

      req(input$codigo, input$tema)

      df <- data.frame(
        CODIGO = as.integer(input$codigo),
        TEMA   = input$tema,
        stringsAsFactors = FALSE
      )

      res <- save_tema(df)

      if (res$action == "insert") {
        showNotification("Tema creado correctamente", type = "message")
      } else {
        showNotification("Tema actualizado correctamente", type = "message")
      }

      limpiar_formulario()
    })

    # Eliminar
    observeEvent(input$eliminar, {

      req(input$codigo)

      showModal(
        modalDialog(
          title = "Confirmar eliminación",
          paste("¿Eliminar el tema con código", input$codigo, "?"),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton("confirmar_eliminar_tema", "Eliminar", class = "btn-danger")
          )
        )
      )
    })

    observeEvent(input$confirmar_eliminar_tema, {

      removeModal()

      delete_tema_por_codigo(input$codigo)

      showNotification("Tema eliminado correctamente", type = "message")
      limpiar_formulario()
    })

  })
}
