
description_of_distances_ui <- function(id){
  ns <- NS(id)
  
  tabPanel("Description of Distances",
           fluidRow(width = 12,
                    br(),
                    br(),
                    column(width = 1, align = "center"),
                    column(width = 5, align = "center",
                           h5(strong("Intercanine Distance")),
                           br(),
                           br(),
                           plotlyOutput("plots_IC")
                    ),
                    column(width = 6, align = "center",
                           h5(" "), 
                           br(),
                           br(),
                           DT::dataTableOutput("responses1",width = 500)
                    )
           ),
           fluidRow(width = 12,
                    br(),
                    br(),
                    column(width = 1, align = "center"),
                    column(width = 5, align = "center",
                           h5(strong("Mesiodistal Right")),
                           br(),
                           br(),
                           plotlyOutput("plots_MDD")
                    ),
                    column(width = 6, align = "center",
                           h5(" "),
                           br(),
                           br(),
                           DT::dataTableOutput("responses2",width = 500)
                    )
           ),
           fluidRow(width = 12,
                    br(),
                    br(),
                    column(width = 1, align = "center"),
                    column(width = 5, align = "center",
                           h5(strong("Mesiodistal Left")),
                           br(),
                           br(),
                           plotlyOutput("plots_MDI")
                    ),
                    column(width = 6, align = "center",
                           h5(" "),
                           br(),
                           br(),
                           DT::dataTableOutput("responses3",width = 500)
                    )
           ),
           fluidRow(width = 12,br(),br())
  )
}

