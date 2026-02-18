  
  
  
  # ============================================================================
  # Data loading
  # ============================================================================
  dir<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vQcDy5itj04-k_s_fxeS5XN_Y8b2w5TGqIJ3vU39vGGkNW1JF7BXFLDBE_GPGdDzw/pub?gid=152731684&single=true&output=csv"
  datag <- as.data.frame(read_csv(dir))
  datag[,4:15]<-datag[,4:15]/100
  
  
  # ============================================================
  # Global values / constants
  # ============================================================
  medidas <- list()
  valores <- list()
  
  
  # ============================================================
  # Base reactives (raw reactives)
  # ============================================================
  filtro <- reactive({
    req(input$zona)
    if (input$zona == "Maxilar") {
      datag[, c(1,2,3,4,5,6)]
    } else {
      datag[, c(1,2,3,7,8,9)]
    }
  })
  
  
  
  # ============================================================
  # Derived reactives (transformed data)
  # ============================================================
  
  
  # ============================================================
  # Filters / selectors logic
  # ============================================================
  
  
  # ============================================================
  # Outputs - Tables
  # ============================================================
  output$tabla <- renderTable({
    filtro()
  })
  
  
  
  # ============================================================
  # Outputs - Plots
  # ============================================================
  
  
  # ============================================================
  # Outputs - UI elements
  # ============================================================
  
  
  # ============================================================
  # Observers
  # ============================================================
  observeEvent(input$btn_img3, {
    showModal(
      modalDialog(
        tags$img(
          src = "images/Resumen.png",
          style = "width:100%; height:auto; display:block; margin:auto;"
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
    )
  })
  
  observeEvent(input$btn_img1, {
    showModal(
      modalDialog(
        tags$img(
          src = "images/Esq.png",
          style = "width:100%; height:auto; display:block; margin:auto;"
        ),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Cerrar")
      )
    )
  })
  
  
  
  # ============================================================
  # UI logic / navigation
  # ============================================================
  pageButtonServer("page", parentSession = session)
  
  # ============================================================
  # Module calls
  # ============================================================
  
  
  # ============================================================
  # Helpers / internal functions
  # ============================================================
  
  
  # ============================================================
  # Debug / logs
  # ============================================================
  

  

