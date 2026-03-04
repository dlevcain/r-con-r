# ============================================================
# main_ui.R
# Main UI layout and application composition
# ============================================================

# --- Module imports -----------------------------------------
source("tabs/reference_sample.R")
source("tabs/general_description.R")
source("tabs/description_of_distances.R")
source("tabs/sex_estimation.R")
source("tabs/results.R")


# --- UI helpers / components --------------------------------

pageButtonUi <- function(id, label = "Resultado") {
  ns <- NS(id)
  actionButton(ns("page_change"), label = label)
}

# --- Main UI -------------------------------------------------
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

  tags$head(
    tags$style(HTML("
      .logo-fixed-bar {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 440px;
        background-color: #FFFFFF;
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: 1040;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.15);
      }

      .logo-fixed-bar img {
        height: 100%;
        width: auto;
      }

      body {
        padding-top: 460px;
      }
    "))
  ),

  # Global JS infrastructure
  shinyjs::useShinyjs(),

  # Main navigation structure
  navbarPage(
    id = "main_nav",
    title = "Intercanine Distance",
    theme = shinytheme("flatly"),

    header = tags$div(
      class = "logo-fixed-bar",
      tags$img(src = "images/Logo.png", alt = "Intercanine Distance")
    ),

    # Paneles principales
    reference_sample_ui("rfrncsmpl"),
    general_description_ui("gnrldscrptn"),
    description_of_distances_ui("dscrptnfdstncs"),
    sex_estimation_ui("sxstmtn"),
    results_ui("rslts")
  )
)
