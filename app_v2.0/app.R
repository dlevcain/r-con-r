# ============================================================
# Application entry point
# This file acts as the system bootstrap
# It wires together global configuration, UI, and server logic
# ============================================================

# Load global configuration and dependencies
source("R/globals.R")

# Load UI definition
source("ui/main_ui.R")

# Load server logic
source("server/main_server.R")

# Launch Shiny application
shinyApp(ui = ui, server = server)