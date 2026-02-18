
general_description_ui <- function(id){
  ns <- NS(id)
  
  tabPanel("General Description",
           fluidRow(br(),
                    column(width = 6, align = "center",
                           h5(strong("Sex")),
                           plotlyOutput("plots_sexo")
                    ),
                    column(width = 6, align = "center",
                           h5(strong("Age - Sex")),
                           plotlyOutput("plots_edad_sexo")
                    )
           ),
           fluidRow(width = 12,
                    br(),
                    column(width = 6, align = "center",
                           h5(strong("Maxillary area - Sex")),
                           plotlyOutput("plots_ori_sexo")
                    ),
                    column(width = 6, align = "center",
                           h5(strong("Mandibular area - Sex")),
                           plotlyOutput("plots_evaluacion")
                    ),
                    br(),
                    br()
           ),
           fluidRow(width = 12,br(),br())
  )
}
