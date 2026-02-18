
source("tabs/home.R")
source("tabs/reference_sample.R")
source("tabs/general_description.R")
source("tabs/description_of_distances.R")
source("tabs/sex_estimation.R")
source("tabs/results.R")



pageButtonUi <- function(id, label = "Resultado") {
  ns <- NS(id)
  actionButton(ns("page_change"), label = label)
}

pageButtonServer <- function(id, parentSession){
  moduleServer(id, function(input, output, session){
    observeEvent(input$page_change,{
      updateTabsetPanel(session=parentSession,
                        inputId="pages",
                        selected="Resultados")
    })
  })
}


ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  navbarPage(
    title = "Intercanine Distance",
    theme = shinytheme("flatly"),
    
    header = tags$div(
      style = "
      position: relative;
      width:100%;
      height:220px;
      background-image: url('images/Logo.png');
      background-size: cover;
      background-position: center;
      display: flex;
      align-items: center;
      justify-content: center;"
    ),
    
    tabPanel(
      "",
      icon = icon("chart-line"),
      mainPanel(
        width = "80%",
        tabsetPanel(
          id = "inTabset",
          home_ui("hm"),
          reference_sample_ui("rfrncsmpl"),
          general_description_ui("gnrldscrptn"),
          description_of_distances_ui("dscrptnfdstncs"),
          sex_estimation_ui("sxstmtn"),
          results_ui("rslts")
        )
      )
    )
  )
)
