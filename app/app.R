options(encoding = "UTF-8")
Sys.setlocale("LC_ALL", "Spanish_Spain.1252")


# app/app.R
library(shiny)
library(bslib)

# --------------------------------------------------
# 1. Cargar configuración
# --------------------------------------------------
source("config/settings.R")

# --------------------------------------------------
# 2. Cargar services
# --------------------------------------------------
source("services/mongo.R")
source("services/openai.R")
source("services/session.R")
source("services/auth.R")
source("services/session_manager.R")


# --------------------------------------------------
# 3. Cargar módulos
# --------------------------------------------------
source("modules/login/login_ui.R")
source("modules/login/login_server.R")
source("ui/app_shell.R")

source("modules/clientes/clientes_ui.R")
source("modules/clientes/clientes_server.R")

# Inicializar servicios
init_mongo(CONFIG)
init_openai(CONFIG)

# --------------------------------------------------
# 4. UI base
# --------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",  # <- tema Bootstrap 5 bonito
    base_font = font_google("Inter")
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  uiOutput("app_ui")
)



# --------------------------------------------------
# 5. Server
# --------------------------------------------------
server <- function(input, output, session) {

	app_state <- reactiveValues(
		logged = FALSE,
		user = NULL,
		session_id = NULL,
		rol = NULL
	)
	
	
	
	output$app_ui <- renderUI({
	  if (!app_state$logged) {
		login_ui("login")
	  } else {
		app_shell_ui(app_state)
	  }
	})


	# Login server
	login_server("login", app_state)

	# Logout
	observeEvent(input$logout, {

		if (!is.null(app_state$user)) {
			logout_user(app_state$user)
		}

		app_state$logged     <- FALSE
		app_state$user       <- NULL
		app_state$rol        <- NULL
		app_state$session_id <- NULL
	})
	
	# --------------------------------------------------
	# Activar módulos cuando el usuario está logueado
	# --------------------------------------------------
	observe({
	  if (isTRUE(app_state$logged)) {
		clientes_server("clientes")
	  }
	})


}

shinyApp(ui, server)