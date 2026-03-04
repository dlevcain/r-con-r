# ============================================================
# Reference Sample Module
# UI definition
# ============================================================

reference_sample_ui <- function(id){
  ns <- NS(id)

  tabPanel(
    "Reference sample",
    br(),
    sidebarPanel(
      width = 12,
      align = "center",
      fluidRow(
        width = 12,
        align = "right",

        column(
          width = 9,
          align = "left",
          selectInput(
            "zona",
            h5(strong("Anatomical Zone")),
            choices = list(
              "",
              "Maxillary" = "Maxillary",
              "Mandibular" = "Mandibular"
            ),
            selected = "Maxillary"
          ),
          br()
        ),

        column(
          width = 3,
          align = "right",
          actionButton("btn_img3", "Abstract", icon = icon("fas fa-images"), style = "margin-bottom: 10px;"),
          actionButton("btn_img1", "Diagram", icon = icon("fas fa-images"))
        ),

        div(
          style = "width: 600px; margin: 0 auto; text-align: left;",
          DT::dataTableOutput("responses")
        )
      )
    ),
    tags$hr()
  )
}
