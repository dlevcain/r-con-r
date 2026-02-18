
source("R/globals.R")

source("ui/main_ui.R")

source("server/main_server.R")

shinyApp(ui = ui, server = server)
