results_ui <- function(id){
  ns <- NS(id)
  
  tabPanel(title= "Results", value = "Resultados", 
           conditionalPanel(condition = "input.resultado != 0",
                            conditionalPanel(condition = "input.numMed == '1'",
                                             conditionalPanel(condition = "input.medida != 'IC.Man'",
                                                              fluidRow(width = 12, align = "center",
                                                                       br(),
                                                                       wellPanel(h3(strong(htmlOutput("resultsText1_1_1"))))
                                                              ),
                                                              fluidRow(width = 12,
                                                                       br(),
                                                                       column(width = 6, align = "center",
                                                                              h4(strong("Visualization of the classification model's performance")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots1_1_1")
                                                                       ),
                                                                       column(width = 6, align = "center",
                                                                              h4(strong("Visualization of the classification model")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots1_1_2")
                                                                       )
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "input.medida == 'IC.Man'", align="center",
                                                              fluidRow(width = 12,
                                                                       br(),
                                                                       h3(strong("There is not a good model that considers only the variable Mandibular intercanine width")),
                                                                       br(),
                                                                       h3(strong("We can not do a sex estimation with your information")),
                                                                       br(),
                                                                       h3(strong("I am so sorry :("))
                                                              )
                                             )
                            ),
                            conditionalPanel(condition = "input.numMed == '2'",
                                             conditionalPanel(condition = "output.modelosVar2",
                                                              fluidRow(width = 12,
                                                                       br(),
                                                                       column(width = 6, align = "center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText2_1_1"))))
                                                                       ),
                                                                       column(width = 6, align = "center",
                                                                              h4(strong("Visualization of real sex")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_1_1")
                                                                       )
                                                              ),
                                                              br(),
                                                              fluidRow(width = 11,
                                                                       br(),
                                                                       column(width = 4, align = "center",
                                                                              h4(strong("Visualization of estimate sex")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_1_2")
                                                                       ),
                                                                       column(width = 3, align = "center",
                                                                              h4(strong("Classified as Male")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_1_3")
                                                                       ),
                                                                       column(width = 4, align = "center",
                                                                              h4(strong("Classified as Female")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_1_4")
                                                                       )
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosAgVar2",
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText2_2_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_2_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText2_2_2"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of the classification model")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_2_2")
                                                                       ),
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosSusVar2",
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("The model with the two variables you chose is not good enough but here is a model considering only one of those variables which can classify better:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText2_3_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of the classification model")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots2_3_1")
                                                                       ),
                                                              ),
                                                              br()
                                             )
                            ),
                            conditionalPanel(condition = "input.numMed == '3'",
                                             conditionalPanel(condition = "output.modelosVar3",
                                                              fluidRow(width = 12, align = "center",
                                                                       br(),
                                                                       wellPanel(h3(strong(htmlOutput("resultsText3_1_1"))))
                                                              ),
                                                              fluidRow(width = 12,
                                                                       br(),
                                                                       column(width = 6, align = "center",
                                                                              h4(strong("Visualization of predicted values")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_1_1")
                                                                       ),
                                                                       column(width = 6, align = "center",
                                                                              h4(strong("Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_1_2")
                                                                       )
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosAgVar3",
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText3_2_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (original model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_2_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText3_2_2"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (aditional model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_2_2")
                                                                       ),
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosSusVar3_1",
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("The model with the three variables you chose is not good enough but here is a model considering some of those variables which can classify better:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText3_3_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_3_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_3_2")
                                                                       ),
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosSusVar3_2",
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("The model with the three variables you chose is not good enough but here are two models considering some of those variables which can classify better:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText3_3_2"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("1st model's Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_3_3")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText3_3_3"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("2nd model's Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots3_3_4")
                                                                       ),
                                                              ),
                                                              br()
                                             )
                            ),
                            conditionalPanel(condition = "input.numMed == '4'",
                                             conditionalPanel(condition = "output.modelosAgVar4",
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText4_2_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (original model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_2_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("In adition, there are another two models, considering some of your input variables, with the with the same percentage of accuracy:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText4_2_2"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (1st aditional model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_2_2")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText4_2_3"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (2nd aditional model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_2_3")
                                                                       ),
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosSusVar4_1",
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("The model with the four variables you chose is not good enough but here is a model considering some of those variables which can classify better:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText4_3_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_3_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_3_2")
                                                                       ),
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosSusVar4_2",
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("The model with the four variables you chose is not good enough but here are two models considering some of those variables which can classify better:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText4_3_2"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("1st model's Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_3_3")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText4_3_3"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("2nd model's Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots4_3_4")
                                                                       ),
                                                              ),
                                                              br()
                                             )
                            ),
                            conditionalPanel(condition = "input.numMed == '5'",
                                             conditionalPanel(condition = "output.modelosAgVar5",
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText5_2_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (original model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots5_2_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("In adition, there are another two models, considering some of your input variables, with the with the same percentage of accuracy:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText5_2_2"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (1st aditional model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots5_2_2")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText5_2_3"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values (2nd aditional model)")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots5_2_3")
                                                                       ),
                                                              ),
                                                              br()
                                             ),
                                             conditionalPanel(condition = "output.modelosSusVar5",
                                                              fluidRow(width=12, align="center",
                                                                       br(),
                                                                       wellPanel(h3(strong("The model with the five variables you chose is not good enough but here is a model considering some of those variables which can classify better:")))
                                                              ),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6, align="center",
                                                                              wellPanel(h3(strong(htmlOutput("resultsText5_3_1"))))
                                                                       ),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Visualization of predicted values")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots5_3_1")
                                                                       ),
                                                              ),
                                                              br(),
                                                              fluidRow(width=12,
                                                                       br(),
                                                                       column(width=6),
                                                                       column(width=6, align="center",
                                                                              h4(strong("Precision-Recall curve")),
                                                                              br(),
                                                                              plotlyOutput("resultsPlots5_3_2")
                                                                       ),
                                                              ),
                                                              br()
                                             )
                            ),
                            conditionalPanel(condition = "input.numMed == '6'",
                                             fluidRow(width = 12, align = "center",
                                                      br(),
                                                      wellPanel(h3(strong(htmlOutput("resultsText6_1_1"))))
                                             ),
                                             fluidRow(width = 12,
                                                      br(),
                                                      column(width = 6, align = "center",
                                                             h4(strong("Visualization of predicted values")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_1")
                                                      ),
                                                      column(width = 6, align = "center",
                                                             h4(strong("Precision-Recall curve")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_2")
                                                      )
                                             ),
                                             br(),
                                             fluidRow(width = 12,
                                                      br(),
                                                      column(width = 4, align = "center",
                                                             wellPanel(h3(strong(htmlOutput("resultsText6_1_2"))))
                                                      ),
                                                      column(width = 4, align = "center",
                                                             wellPanel(h3(strong(htmlOutput("resultsText6_1_3"))))
                                                      ),
                                                      column(width = 4, align = "center",
                                                             wellPanel(h3(strong(htmlOutput("resultsText6_1_4"))))
                                                      )
                                             ),
                                             br(),
                                             fluidRow(width = 12,
                                                      br(),
                                                      column(width = 4, align = "center",
                                                             h4(strong("Visualization of predicted values")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_3")
                                                      ),
                                                      column(width = 4, align = "center",
                                                             h4(strong("Visualization of predicted values")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_4")
                                                      ),
                                                      column(width = 4, align = "center",
                                                             h4(strong("Visualization of predicted values")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_5")
                                                      )
                                             ),
                                             br(),
                                             fluidRow(width = 12,
                                                      br(),
                                                      column(width = 4, align = "center",
                                                             h4(strong("Precision-Recall curve")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_6")
                                                      ),
                                                      column(width = 4, align = "center",
                                                             h4(strong("Precision-Recall curve")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_7")
                                                      ),
                                                      column(width = 4, align = "center",
                                                             h4(strong("Precision-Recall curve")),
                                                             br(),
                                                             plotlyOutput("resultsPlots6_1_8")
                                                      )
                                             )
                            )
           )
  )
}
