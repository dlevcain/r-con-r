
home_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(
    "Home",
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
            choices = list( "",
              "Maxillary" = "Maxilar",
              "Mandibular" = "Mandibular"
            ),
            selected = ""
          ),
          br()
        ),
        
        column(
          width = 3,
          align = "right",
          actionButton("btn_img3", "Abstract", icon = icon("fas fa-images"),style = "margin-bottom: 10px;"),
          
          actionButton("btn_img1", "Diagram", icon = icon("fas fa-images"))
        ),
        
        div(
          tableOutput("tabla"),
          style = "text-align: left;"
        )
      )
    )
  )
}
