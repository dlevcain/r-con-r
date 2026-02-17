# ============================================================
# Reference Sample Module
# UI definition
# ============================================================

reference_sample_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(
    "Reference sample",
    br(),
    div(
      style = "width: 600px; margin: 0 auto;",
      DT::dataTableOutput("responses")
    ),
    tags$hr()
  )
}