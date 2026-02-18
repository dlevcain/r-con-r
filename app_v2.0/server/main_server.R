# ==============================================================================
# main_server.R
# Main Server Logic and Application Control
# ============================================================================== 

server <- function(input, output, session) {
  section_files <- c(
    "server/sections/core_server_setup.R",
    "server/sections/descriptive_outputs.R",
    "server/sections/sex_estimation_logic.R"
  )

  for (section_file in section_files) {
    source(section_file, local = environment())
  }
}
