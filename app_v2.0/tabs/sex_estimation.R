# ============================================================
# Sex Estimation Module
# UI definition
# ============================================================

sex_estimation_ui <- function(id){
  ns <- NS(id)
  
  tabPanel("Sex estimation", value="Sex estimation",
           fluidRow(width = 12,
                    column(6,h3(strong("Number of measurements"))),
                    column(6,
                           selectInput("numMed","" , choices = list("0" = 0, "1" = 1, "2" = 2, "3" = 3,"4" = 4, "5" = 5, "6" = 6),selected = 0,selectize = FALSE)
                    )
           ),
           conditionalPanel(condition = "input.numMed == '1'",
                            br(),
                            fluidRow(width = 12,
                                     column(4,h3("Select measurement")),
                                     column(4,
                                            selectInput("medida","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,numericInput("valMed","",0))
                            )
           ),
           conditionalPanel(condition = "input.numMed == '2'",
                            br(),
                            fluidRow(width = 12,
                                     column(4,h4("1st measurement")),
                                     column(4,h4("2nd measurement"))
                            ),
                            fluidRow(width = 12,
                                     column(4,
                                            selectInput("medida2_1","" , choices = Choices ,selected = 0 ,selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida2_2","" , choices = Choices ,selected = 0 ,selectize = FALSE)
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("val2Med1","",0)),
                                     column(4,numericInput("val2Med2","",0))
                            )
           ),
           conditionalPanel(condition = "input.numMed == '3'",
                            br(),
                            fluidRow(width = 12,
                                     column(4,h4("1st measurement")),
                                     column(4,h4("2nd measurement")),
                                     column(4,h4("3rd measurement"))
                            ),
                            fluidRow(width = 12,
                                     column(4,
                                            selectInput("medida3_1","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida3_2","" , choices = Choices , selected = 0, selectize = FALSE)
                                     ),
                                     column(4,selectInput("medida3_3","" , choices = Choices ,selected = 0 ,selectize = FALSE)
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("val3Med1","",0)),
                                     column(4,numericInput("val3Med2","",0)),
                                     column(4,numericInput("val3Med3","",0))
                            )
           ),
           conditionalPanel(condition = "input.numMed == '4'",
                            br(),
                            fluidRow(width = 12,
                                     column(4,h4("1st measurement")),
                                     column(4,h4("2nd measurement")),
                                     column(4,h4("3rd measurement"))
                            ),
                            fluidRow(width = 12,
                                     column(4,
                                            selectInput("medida4_1","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida4_2","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida4_3","" , choices = Choices, selected = 0, selectize = FALSE)
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("val4Med1","",0)),
                                     column(4,numericInput("val4Med2","",0)),
                                     column(4,numericInput("val4Med3","",0))
                            ),
                            br(),
                            fluidRow(width = 12,
                                     column(4, h4("4th measurement"))
                            ),
                            fluidRow(width = 12,
                                     column(4,
                                            selectInput("medida4_4","" , choices = Choices, selected = 0, selectize = FALSE)
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("val4Med4","",0))
                            )
           ),
           conditionalPanel(condition = "input.numMed == '5'",
                            br(),
                            fluidRow(width = 12,
                                     column(4, h4("1st measurement")),
                                     column(4, h4("2nd measurement")),
                                     column(4, h4("3rd measurement"))
                            ),
                            fluidRow(width = 12,
                                     column(4,
                                            selectInput("medida5_1","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida5_2","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida5_3","" , choices = Choices, selected = 0, selectize = FALSE)
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("val5Med1","",0)),
                                     column(4,numericInput("val5Med2","",0)),
                                     column(4,numericInput("val5Med3","",0))
                            ),
                            br(),
                            fluidRow(width = 12,
                                     column(4, h4("4th measurement")),
                                     column(4, h4("5th measurement"))
                            ),
                            fluidRow(width = 12,
                                     column(4,
                                            selectInput("medida5_4","" , choices = Choices, selected = 0, selectize = FALSE)
                                     ),
                                     column(4,
                                            selectInput("medida5_5","" , choices = Choices, selected = 0, selectize = FALSE)
                                     )
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("val5Med4","",0)),
                                     column(4,numericInput("val5Med5","",0))
                            )
           ),
           conditionalPanel(condition = "input.numMed == '6'",
                            br(),
                            fluidRow(width = 12,
                                     column(4, wellPanel(h5(strong("Maxillary intercanine width")))),
                                     column(4, wellPanel(h5(strong("13's mesiodistal width")))),
                                     column(4, wellPanel(h5(strong("23's mesiodistal width"))))
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("ICMax","",0)),
                                     column(4,numericInput("MDMaxR","",0)),
                                     column(4,numericInput("MDMaxL","",0))
                            ),
                            br(),
                            fluidRow(width = 12,
                                     column(4, wellPanel(h5(strong("Mandibular intercanine width")))),
                                     column(4, wellPanel(h5(strong("33's mesiodistal width")))),
                                     column(4, wellPanel(h5(strong("43's mesiodistal width"))))
                            ),
                            fluidRow(width = 12,
                                     column(4,numericInput("ICMan","",0)),
                                     column(4,numericInput("MDManL","",0)),
                                     column(4,numericInput("MDManR","",0))
                            )
           ),
           conditionalPanel(condition = "input.numMed == '1' || input.numMed == '2' || input.numMed == '3' || input.numMed == '4' || input.numMed == '5' || input.numMed == '6'",
                            fluidRow(
                              br(),
                              column(12,
                                     actionButton("resultado","Find model"),
                                     align = "center", 
                                     style = "margin-bottom: 10px;", 
                                     style = "margin-top: -10px;"
                              )
                            )
           )
  )
}