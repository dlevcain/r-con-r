


server = function(input, output,session) {
  
  
  
  dir<-"https://docs.google.com/spreadsheets/d/e/2PACX-1vQcDy5itj04-k_s_fxeS5XN_Y8b2w5TGqIJ3vU39vGGkNW1JF7BXFLDBE_GPGdDzw/pub?gid=152731684&single=true&output=csv"
  datag <- as.data.frame(read_csv(dir))
  datag[,4:15]<-datag[,4:15]/100

  app_colors <- list(
    sex = c("#9AC0CD", "#FFAEB9"),
    classified = c("green3", "red3"),
    box_fill = c("#B2DFEE", "#FFB6C1"),
    box_border = c("#68838B", "#8B5F65")
  )

  
  filtro <- reactive({
    req(input$zona)
    if (input$zona == "Maxilar") {
      datag[, c(1,2,3,4,5,6)]
    } else {
      datag[, c(1,2,3,7,8,9)]
    }
  })
  
  
  
  
  
  
  
  output$tabla <- renderTable({
    filtro()
  })
  
  
  
  
  
  
  
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
  
  
  
  pageButtonServer("page", parentSession = session)
  
  
  
  
  
  # Debug / logs
  

  

  
  # R E F E R E N C E   S A M P L E ------------------------------------------------------------------------------------------------
  output$responses<- DT::renderDataTable({
    b<-filtro() 
    a_b_a<-data.frame(b)
    DT::datatable(a_b_a,options = list(searching = FALSE,info = FALSE,pageLength = 8,dom = 'ftp'),extensions = 'Responsive')
  })
  
  
  # G E N E R A L    D E S C R I P T I O N ------------------------------------------------------------------------------------------------  
  output$plots_sexo <- renderPlotly({
    dataS = datag
    data4 <- dataS[3]
    
    ch <- count(data4 %>% filter(Sexo == "Masculino"))
    cm <- count(data4 %>% filter(Sexo == "Femenino"))
    h <- as.integer(ch[2])
    m <- as.integer(cm[2])
    
    df <- data.frame(
      group = c("Masculino", "Femenino"),
      value = c(h,m)
    )
    
    colors <- c('#A3E4D7','#E8DAEF')
    fig <- plot_ly(df, labels = ~group, values = ~value, type = 'pie',
                   marker = list(colors = colors,
                                 line = list(color = '#FFFFFF', width = 1)
                   )
    )
    fig %>% layout(title = '',
                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
    )
  })
  
  output$plots_edad_sexo <- renderPlotly({
    dataS = datag
    
    p1 <- plot_ly(dataS, x = ~Sexo, y = ~Edad, color = ~Sexo, type = "box") 
    p1 %>% layout( xaxis = list(title = 'Sex'), yaxis = list(title = 'Age'))
  })
  
  output$plots_ori_sexo <- renderPlotly({
    dataS = datag
    
    fig <- plot_ly(data = dataS, x = dataS[,c(4)], y = dataS[,c(10)], color = dataS[,c(3)])
    fig %>% layout( xaxis = list(title = 'Distance - Without treatment (mm) '), yaxis = list(title = 'Distance - With treatment (mm)'))
  }) 
  
  output$plots_evaluacion <- renderPlotly({
    dataS = datag
    
    fig <- plot_ly(data = dataS, x = dataS[,c(7)], y = dataS[,c(13)], color = dataS[,c(3)])
    fig %>% layout( xaxis = list(title = 'Distance - Without treatment (mm)'), yaxis = list(title = 'Distance - With treatment (mm)'))
  })
  
  
  # D E S C R I P T I O N    O F    D I S T A C E S ------------------------------------------------------------------------------------------------
  output$plots_IC<- renderPlotly({
    dataS =filtro()
    
    p1 <- plot_ly(dataS, x=dataS[,3], y = dataS[,4], type = "box") 
    p1 %>% layout( xaxis = list(title =  paste(input$Zona,"-",input$Tratamiento)), yaxis = list(title = 'Distance (mm)'))
  })
  
  output$responses1 <-DT::renderDataTable(DT::datatable({{
    my.summary <- function(x, na.rm=TRUE){
      result <- c(N=length(x),
                  'Min'=min(x, na.rm=na.rm),
                  'Max'=max(x, na.rm=na.rm),
                  'Q1'=quantile(x, probs = 0.25, na.rm = TRUE,names=FALSE),
                  Median=median(x, na.rm=na.rm),
                  'Q3'=quantile(x, probs = 0.75, na.rm = TRUE,names=FALSE),
                  Mean=mean(x, na.rm=na.rm),
                  'SD'=sd(x, na.rm=na.rm),
                  Variance=var(x,na.rm=TRUE),
                  'VC'=sd(x, na.rm=na.rm)/mean(x, na.rm=na.rm)
      )
    }
    aa<-filtro()
    su_F<-subset(aa,aa[3]=="Femenino")
    su_M<-subset(aa,aa[3]=="Masculino")
    
    b_F<-round(as.data.frame(sapply(su_F[4], my.summary)),4)
    names(b_F)<-c("Female")
    
    b_M<-round(as.data.frame(sapply(su_M[4], my.summary)),4)
    names(b_M)<-c("Male")
    
    as<-cbind(b_F,b_M)
  }
  },
  options = list(searching = FALSE,info = FALSE,paging = FALSE,dom = 'ftp'),extensions = 'Responsive')
  )
  
  output$plots_MDD<- renderPlotly({
    dataS =filtro()
    
    p1 <- plot_ly(dataS, x=dataS[,3], y = dataS[,5],fillcolor = "#A3E4D7", line = list(color = "009999"), type = "box") 
    p1 %>% layout(xaxis = list(title =  paste(input$Zona,"-",input$Tratamiento)), yaxis = list(title = 'Mesiodistal Right (mm)'))
  })
  
  output$responses2 <-DT::renderDataTable(DT::datatable({{
    my.summary <- function(x, na.rm=TRUE){
      result <- c(N=length(x),
                  'Min'=min(x, na.rm=na.rm),
                  'Max'=max(x, na.rm=na.rm),
                  'Q1'=quantile(x, probs = 0.25, na.rm = TRUE,names=FALSE),
                  Median=median(x, na.rm=na.rm),
                  'Q3'=quantile(x, probs = 0.75, na.rm = TRUE,names=FALSE),
                  Mean=mean(x, na.rm=na.rm),
                  'SD'=sd(x, na.rm=na.rm),
                  Variance=var(x,na.rm=TRUE),
                  'VC'=sd(x, na.rm=na.rm)/mean(x, na.rm=na.rm))
    }
    aa<-filtro()
    su_F<-subset(aa,aa[3]=="Femenino")
    su_M<-subset(aa,aa[3]=="Masculino")
    
    b_F<-round(as.data.frame(sapply(su_F[5], my.summary)),4)
    names(b_F)<-c("Female")
    
    b_M<-round(as.data.frame(sapply(su_M[5], my.summary)),4)
    names(b_M)<-c("Male")
    
    as<-cbind(b_F,b_M)
  }
  },
  options = list(searching = FALSE,info = FALSE,paging = FALSE,dom = 'ftp'),extensions = 'Responsive')
  )
  
  output$plots_MDI<- renderPlotly({
    dataS = filtro()
    
    p1 <- plot_ly(dataS, x=dataS[,3], y = dataS[,6],fillcolor = "#E8DAEF", line = list(color = "#7D3C98"), type = "box") 
    p1 %>% layout(xaxis = list(title = paste(input$Zona,"-",input$Tratamiento)), yaxis = list(title = 'Mesiodistal Left (mm)'))
  })
  
  output$responses3 <-DT::renderDataTable(DT::datatable({{
    my.summary <- function(x, na.rm=TRUE){
      result <- c(N=length(x),
                  'Min'=min(x, na.rm=na.rm),
                  'Max'=max(x, na.rm=na.rm),
                  'Q1'=quantile(x, probs = 0.25, na.rm = TRUE,names=FALSE),
                  Median=median(x, na.rm=na.rm),
                  'Q3'=quantile(x, probs = 0.75, na.rm = TRUE,names=FALSE),
                  Mean=mean(x, na.rm=na.rm),
                  'SD'=sd(x, na.rm=na.rm),
                  Variance=var(x,na.rm=TRUE),
                  'VC'=sd(x, na.rm=na.rm)/mean(x, na.rm=na.rm))
    }
    aa<-filtro()
    su_F<-subset(aa,aa[3]=="Femenino")
    su_M<-subset(aa,aa[3]=="Masculino")
    
    b_F<-round(as.data.frame(sapply(su_F[6], my.summary)),4)
    names(b_F)<-c("Female")
    
    b_M<-round(as.data.frame(sapply(su_M[6], my.summary)),4)
    names(b_M)<-c("Male")
    
    as<-cbind(b_F,b_M)
  }
  },
  options = list(searching = FALSE,info = FALSE,paging = FALSE,dom = 'ftp'),extensions = 'Responsive')
  )
  
  
  # S E X   E S T I M A T I O N ------------------------------------------------------------------------------------------------  
  var2mod_1 <- c("MD.MaxR", "IC.Man")
  var2mod_2 <- c("MD.MaxR", "MD.ManR")
  var2mod_3 <- c("MD.MaxL", "MD.ManL")
  var2mod_4 <- c("IC.Man", "MD.ManR")
  var2mod_5 <- c("IC.Man", "MD.ManL")
  var2modAgrega_1 <- c("IC.Max","MD.MaxL")
  var2modAgrega_2 <- c("IC.Max","MD.ManR")
  var2modAgrega_3 <- c("IC.Max","MD.ManL")
  var2modSustituye_1 <- c("IC.Max","MD.MaxR")
  var2modSustituye_2 <- c("IC.Max","IC.Man")
  var2modSustituye_3 <- c("MD.MaxR","MD.MaxL")
  var2modSustituye_4 <- c("MD.MaxL","IC.Man")
  var2modSustituye_5 <- c("MD.MaxL","MD.ManR")
  var2modSustituye_6 <- c("MD.ManR","MD.ManL")
  var2modSustituye_7 <- c("MD.MaxR","MD.ManL")
  var3mod_1 <- c("MD.MaxR", "IC.Man", "MD.ManL")
  var3modAgrega_1 <- c("IC.Max", "IC.Man", "MD.ManR")
  var3modAgrega_2 <- c("IC.Max", "IC.Man", "MD.ManL")
  var3modAgrega_3 <- c("MD.MaxR", "MD.MaxL", "MD.ManL")
  var3modAgrega_4 <- c("MD.MaxR", "MD.ManR", "MD.ManL")
  var3modSustituye_1 <- c("IC.Max", "MD.MaxR", "IC.Man")
  var3modSustituye_2 <- c("MD.MaxR", "MD.MaxL", "IC.Man")
  var3modSustituye_3 <- c("MD.MaxR", "MD.MaxL", "MD.ManR")
  var3modSustituye_4 <- c("IC.Max", "MD.MaxR", "MD.ManR")
  var3modSustituye_5 <- c("IC.Max", "MD.MaxL", "MD.ManL")
  var3modSustituye_6 <- c("MD.MaxL", "IC.Man", "MD.ManL")
  var3modSustituye_7 <- c("MD.MaxL", "MD.ManR", "MD.ManL")
  var3modSustituye_8 <- c("MD.MaxL", "IC.Man", "MD.ManR")
  var3modSustituye_9 <- c("IC.Man", "MD.ManR", "MD.ManL")
  var3modSustituye_10 <- c("MD.MaxR", "IC.Man", "MD.ManR")
  var3modSustituye_11 <- c("IC.Max", "MD.MaxR", "MD.MaxL")
  var3modSustituye_12 <- c("IC.Max", "MD.MaxL", "IC.Man")
  var3modSustituye_13 <- c("IC.Max", "MD.MaxL", "MD.ManR")
  var3modSustituye_14 <- c("IC.Max", "MD.MaxR", "MD.ManL")
  var3modSustituye_15 <- c("IC.Max", "MD.ManR", "MD.ManL")
  var4modAgrega_1 <- c("MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManR")
  var4modAgrega_2 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "MD.ManL")
  var4modSustituye_1 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "IC.Man")
  var4modSustituye_2 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "MD.ManR")
  var4modSustituye_3 <- c("IC.Max", "MD.MaxL", "IC.Man", "MD.ManL")
  var4modSustituye_4 <- c("IC.Max", "MD.MaxL", "MD.ManR", "MD.ManL")
  var4modSustituye_5 <- c("MD.MaxL", "IC.Man", "MD.ManR", "MD.ManL")
  var4modSustituye_6 <- c("IC.Max", "MD.MaxR", "IC.Man", "MD.ManL")
  var4modSustituye_7 <- c("MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManL")
  var4modSustituye_8 <- c("MD.MaxR", "IC.Man", "MD.ManR", "MD.ManL")
  var4modSustituye_9 <- c("IC.Max", "MD.MaxR", "IC.Man", "MD.ManR")
  var4modSustituye_10 <- c("IC.Max", "MD.MaxR", "MD.ManR", "MD.ManL")
  var4modSustituye_11 <- c("MD.MaxR", "MD.MaxL", "MD.ManR", "MD.ManL")
  var4modSustituye_12 <- c("IC.Max", "IC.Man", "MD.ManR", "MD.ManL")
  var4modSustituye_13 <- c("IC.Max", "MD.MaxL", "IC.Man", "MD.ManR")
  var5modAgrega_1 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManR")
  var5modSustituye_1 <- c("IC.Max", "MD.MaxL", "IC.Man", "MD.ManR", "MD.ManL")
  var5modSustituye_2 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManL")
  var5modSustituye_3 <- c("IC.Max", "MD.MaxR", "IC.Man", "MD.ManR", "MD.ManL")
  var5modSustituye_4 <- c("MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManR", "MD.ManL")
  var5modSustituye_5 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "MD.ManL", "MD.ManR")
  
  
  output$modelosVar2 <- reactive({
    (input$medida2_1 %in% var2mod_1 && input$medida2_2 %in% var2mod_1) ||
      (input$medida2_1 %in% var2mod_2 && input$medida2_2 %in% var2mod_2) ||
      (input$medida2_1 %in% var2mod_3 && input$medida2_2 %in% var2mod_3) ||
      (input$medida2_1 %in% var2mod_4 && input$medida2_2 %in% var2mod_4) ||
      (input$medida2_1 %in% var2mod_5 && input$medida2_2 %in% var2mod_5)
  })
  outputOptions(output, "modelosVar2", suspendWhenHidden = FALSE)
  
  output$modelosAgVar2 <- reactive({
    (input$medida2_1 %in% var2modAgrega_1 && input$medida2_2 %in% var2modAgrega_1) ||
      (input$medida2_1 %in% var2modAgrega_2 && input$medida2_2 %in% var2modAgrega_2) ||
      (input$medida2_1 %in% var2modAgrega_3 && input$medida2_2 %in% var2modAgrega_3)
  })
  outputOptions(output, "modelosAgVar2", suspendWhenHidden = FALSE)
  
  output$modelosSusVar2 <- reactive({
    (input$medida2_1 %in% var2modSustituye_1 && input$medida2_2 %in% var2modSustituye_1) ||
      (input$medida2_1 %in% var2modSustituye_2 && input$medida2_2 %in% var2modSustituye_2) ||
      (input$medida2_1 %in% var2modSustituye_3 && input$medida2_2 %in% var2modSustituye_3) ||
      (input$medida2_1 %in% var2modSustituye_4 && input$medida2_2 %in% var2modSustituye_4) ||
      (input$medida2_1 %in% var2modSustituye_5 && input$medida2_2 %in% var2modSustituye_5) ||
      (input$medida2_1 %in% var2modSustituye_6 && input$medida2_2 %in% var2modSustituye_6) ||
      (input$medida2_1 %in% var2modSustituye_7 && input$medida2_2 %in% var2modSustituye_7)
  })
  outputOptions(output, "modelosSusVar2", suspendWhenHidden = FALSE)
  
  output$modelosVar3 <- reactive({
    input$medida3_1 %in% var3mod_1 && input$medida3_2 %in% var3mod_1 && input$medida3_3 %in% var3mod_1
  })
  outputOptions(output, "modelosVar3", suspendWhenHidden = FALSE)
  
  output$modelosAgVar3 <- reactive({
    (input$medida3_1 %in% var3modAgrega_1 && input$medida3_2 %in% var3modAgrega_1 && input$medida3_3 %in% var3modAgrega_1) ||
      (input$medida3_1 %in% var3modAgrega_2 && input$medida3_2 %in% var3modAgrega_2 && input$medida3_3 %in% var3modAgrega_2) ||
      (input$medida3_1 %in% var3modAgrega_3 && input$medida3_2 %in% var3modAgrega_3 && input$medida3_3 %in% var3modAgrega_3) ||
      (input$medida3_1 %in% var3modAgrega_4 && input$medida3_2 %in% var3modAgrega_4 && input$medida3_3 %in% var3modAgrega_4)
  })
  outputOptions(output, "modelosAgVar3", suspendWhenHidden = FALSE)
  
  output$modelosSusVar3_1 <- reactive({
    (input$medida3_1 %in% var3modSustituye_1 && input$medida3_2 %in% var3modSustituye_1 && input$medida3_3 %in% var3modSustituye_1) ||
      (input$medida3_1 %in% var3modSustituye_2 && input$medida3_2 %in% var3modSustituye_2 && input$medida3_3 %in% var3modSustituye_2) ||
      (input$medida3_1 %in% var3modSustituye_3 && input$medida3_2 %in% var3modSustituye_3 && input$medida3_3 %in% var3modSustituye_3) ||
      (input$medida3_1 %in% var3modSustituye_4 && input$medida3_2 %in% var3modSustituye_4 && input$medida3_3 %in% var3modSustituye_4) ||
      (input$medida3_1 %in% var3modSustituye_5 && input$medida3_2 %in% var3modSustituye_5 && input$medida3_3 %in% var3modSustituye_5) ||
      (input$medida3_1 %in% var3modSustituye_6 && input$medida3_2 %in% var3modSustituye_6 && input$medida3_3 %in% var3modSustituye_6) ||
      (input$medida3_1 %in% var3modSustituye_7 && input$medida3_2 %in% var3modSustituye_7 && input$medida3_3 %in% var3modSustituye_7) ||
      (input$medida3_1 %in% var3modSustituye_8 && input$medida3_2 %in% var3modSustituye_8 && input$medida3_3 %in% var3modSustituye_8) ||
      (input$medida3_1 %in% var3modSustituye_9 && input$medida3_2 %in% var3modSustituye_9 && input$medida3_3 %in% var3modSustituye_9)
  })
  outputOptions(output, "modelosSusVar3_1", suspendWhenHidden = FALSE)
  
  output$modelosSusVar3_2 <- reactive({
    (input$medida3_1 %in% var3modSustituye_10 && input$medida3_2 %in% var3modSustituye_10 && input$medida3_3 %in% var3modSustituye_10) ||
      (input$medida3_1 %in% var3modSustituye_11 && input$medida3_2 %in% var3modSustituye_11 && input$medida3_3 %in% var3modSustituye_11) ||
      (input$medida3_1 %in% var3modSustituye_12 && input$medida3_2 %in% var3modSustituye_12 && input$medida3_3 %in% var3modSustituye_12) ||
      (input$medida3_1 %in% var3modSustituye_13 && input$medida3_2 %in% var3modSustituye_13 && input$medida3_3 %in% var3modSustituye_13) ||
      (input$medida3_1 %in% var3modSustituye_14 && input$medida3_2 %in% var3modSustituye_14 && input$medida3_3 %in% var3modSustituye_14) ||
      (input$medida3_1 %in% var3modSustituye_15 && input$medida3_2 %in% var3modSustituye_15 && input$medida3_3 %in% var3modSustituye_15)
  })
  outputOptions(output, "modelosSusVar3_2", suspendWhenHidden = FALSE)
  
  output$modelosAgVar4 <- reactive({
    (input$medida4_1 %in% var4modAgrega_1 && input$medida4_2 %in% var4modAgrega_1 && input$medida4_3 %in% var4modAgrega_1 && input$medida4_4 %in% var4modAgrega_1) ||
      (input$medida4_1 %in% var4modAgrega_2 && input$medida4_2 %in% var4modAgrega_2 && input$medida4_3 %in% var4modAgrega_2 && input$medida4_4 %in% var4modAgrega_2)
  })
  outputOptions(output, "modelosAgVar4", suspendWhenHidden = FALSE)
  
  output$modelosSusVar4_1 <- reactive({
    (input$medida4_1 %in% var4modSustituye_1 && input$medida4_2 %in% var4modSustituye_1 && input$medida4_3 %in% var4modSustituye_1 && input$medida4_4 %in% var4modSustituye_1) ||
      (input$medida4_1 %in% var4modSustituye_2 && input$medida4_2 %in% var4modSustituye_2 && input$medida4_3 %in% var4modSustituye_2 && input$medida4_4 %in% var4modSustituye_2) ||
      (input$medida4_1 %in% var4modSustituye_3 && input$medida4_2 %in% var4modSustituye_3 && input$medida4_3 %in% var4modSustituye_3 && input$medida4_4 %in% var4modSustituye_3) ||
      (input$medida4_1 %in% var4modSustituye_4 && input$medida4_2 %in% var4modSustituye_4 && input$medida4_3 %in% var4modSustituye_4 && input$medida4_4 %in% var4modSustituye_4) ||
      (input$medida4_1 %in% var4modSustituye_5 && input$medida4_2 %in% var4modSustituye_5 && input$medida4_3 %in% var4modSustituye_5 && input$medida4_4 %in% var4modSustituye_5) ||
      (input$medida4_1 %in% var4modSustituye_6 && input$medida4_2 %in% var4modSustituye_6 && input$medida4_3 %in% var4modSustituye_6 && input$medida4_4 %in% var4modSustituye_6) ||
      (input$medida4_1 %in% var4modSustituye_7 && input$medida4_2 %in% var4modSustituye_7 && input$medida4_3 %in% var4modSustituye_7 && input$medida4_4 %in% var4modSustituye_7) ||
      (input$medida4_1 %in% var4modSustituye_8 && input$medida4_2 %in% var4modSustituye_8 && input$medida4_3 %in% var4modSustituye_8 && input$medida4_4 %in% var4modSustituye_8) 
  })
  outputOptions(output, "modelosSusVar4_1", suspendWhenHidden = FALSE)
  
  output$modelosSusVar4_2 <- reactive({
    (input$medida4_1 %in% var4modSustituye_9 && input$medida4_2 %in% var4modSustituye_9 && input$medida4_3 %in% var4modSustituye_9 && input$medida4_4 %in% var4modSustituye_9) ||
      (input$medida4_1 %in% var4modSustituye_10 && input$medida4_2 %in% var4modSustituye_10 && input$medida4_3 %in% var4modSustituye_10 && input$medida4_4 %in% var4modSustituye_10) ||
      (input$medida4_1 %in% var4modSustituye_11 && input$medida4_2 %in% var4modSustituye_11 && input$medida4_3 %in% var4modSustituye_11 && input$medida4_4 %in% var4modSustituye_11) ||
      (input$medida4_1 %in% var4modSustituye_12 && input$medida4_2 %in% var4modSustituye_12 && input$medida4_3 %in% var4modSustituye_12 && input$medida4_4 %in% var4modSustituye_12) ||
      (input$medida4_1 %in% var4modSustituye_13 && input$medida4_2 %in% var4modSustituye_13 && input$medida4_3 %in% var4modSustituye_13 && input$medida4_4 %in% var4modSustituye_13)
  })
  outputOptions(output, "modelosSusVar4_2", suspendWhenHidden = FALSE)
  
  output$modelosAgVar5 <- reactive({
    (input$medida5_1 %in% var5modAgrega_1 && input$medida5_2 %in% var5modAgrega_1 && input$medida5_3 %in% var5modAgrega_1 && input$medida5_4 %in% var5modAgrega_1 && input$medida5_5 %in% var5modAgrega_1)
  })
  outputOptions(output, "modelosAgVar5", suspendWhenHidden = FALSE)
  
  output$modelosSusVar5 <- reactive({
    (input$medida5_1 %in% var5modSustituye_1 && input$medida5_2 %in% var5modSustituye_1 && input$medida5_3 %in% var5modSustituye_1 && input$medida5_4 %in% var5modSustituye_1 && input$medida5_5 %in% var5modSustituye_1) ||
      (input$medida5_1 %in% var5modSustituye_2 && input$medida5_2 %in% var5modSustituye_2 && input$medida5_3 %in% var5modSustituye_2 && input$medida5_4 %in% var5modSustituye_2 && input$medida5_5 %in% var5modSustituye_2) ||
      (input$medida5_1 %in% var5modSustituye_3 && input$medida5_2 %in% var5modSustituye_3 && input$medida5_3 %in% var5modSustituye_3 && input$medida5_4 %in% var5modSustituye_3 && input$medida5_5 %in% var5modSustituye_3) ||
      (input$medida5_1 %in% var5modSustituye_4 && input$medida5_2 %in% var5modSustituye_4 && input$medida5_3 %in% var5modSustituye_4 && input$medida5_4 %in% var5modSustituye_4 && input$medida5_5 %in% var5modSustituye_4) ||
      (input$medida5_1 %in% var5modSustituye_5 && input$medida5_2 %in% var5modSustituye_5 && input$medida5_3 %in% var5modSustituye_5 && input$medida5_4 %in% var5modSustituye_5 && input$medida5_5 %in% var5modSustituye_5)
  })
  outputOptions(output, "modelosSusVar5", suspendWhenHidden = FALSE)
  
  Medidas <- reactive({
    if(input$numMed==2){
      medidas = list(med1=input$medida2_1, med2=input$medida2_2)
    }else if(input$numMed==3){
      medidas = list(med1=input$medida3_1, med2=input$medida3_2, med3=input$medida3_3)
    }else if(input$numMed==4){
      medidas = list(med1=input$medida4_1, med2=input$medida4_2, med3=input$medida4_3, med4=input$medida4_4)
    }else if(input$numMed==5){
      medidas = list(med1=input$medida5_1, med2=input$medida5_2, med3=input$medida5_3, med4=input$medida5_4, med5=input$medida5_5)
    }
  })
  
  Valores <- reactive({
    if(input$numMed==2){
      valores = list(val1=input$val2Med1, val2=input$val2Med2)
    }else if(input$numMed==3){
      valores = list(val1=input$val3Med1, val2=input$val3Med2, val3=input$val3Med3)
    }else if(input$numMed==4){
      valores = list(val1=input$val4Med1, val2=input$val4Med2, val3=input$val4Med3, val4=input$val4Med4)
    }else if(input$numMed==5){
      valores = list(val1=input$val5Med1, val2=input$val5Med2, val3=input$val5Med3, val4=input$val5Med4, val5=input$val5Med5)
    }
  })
  
  
  url <- "https://docs.google.com/spreadsheets/d/1vOGE2_5Br9qpQYt4sw8rkxHWuvq_Bb66u__UgQHwGrI/export?format=csv&gid=739303745"
  Datos <- read.csv(url)
  
  Datos$SexNum <- Datos$Sex
  Datos$Sex <- as.factor(Datos$Sex)
  
  ##############################################################################################
  ##############################################################################################
  #-------------------------------------------IC.MAX--------------------------------------------
  log_ICMax = glm(Sex ~ IC.Max, data = Datos, family = binomial)
  #-------------------------------------------MD.MAXR-------------------------------------------
  log_MDMaxR = glm(Sex ~ MD.MaxR, data = Datos, family = binomial)
  #-------------------------------------------MD.MAXL-------------------------------------------
  log_MDMaxL = glm(Sex ~ MD.MaxL, data = Datos, family = binomial)
  #-------------------------------------------MD.MANR-------------------------------------------
  log_MDManR = glm(Sex ~ MD.ManR, data = Datos, family = binomial)
  #-------------------------------------------MD.MANL-------------------------------------------
  log_MDManL = glm(Sex ~ MD.ManL, data = Datos, family = binomial)
  #-------------------------------------- MD.MaxR + IC.Man -------------------------------------
  log2var_1 = glm(Sex ~ MD.MaxR + IC.Man, data = Datos, family = binomial)
  #------------------------------------- MD.MaxR + MD.ManR -------------------------------------
  log2var_2 = glm(Sex ~ MD.MaxR + MD.ManR, data = Datos, family = binomial)
  #------------------------------------- MD.MaxL + MD.ManL -------------------------------------
  log2var_3 = glm(Sex ~ MD.MaxL + MD.ManL, data = Datos, family = binomial)
  #------------------------------------- IC.Man + MD.ManR --------------------------------------
  log2var_4 = glm(Sex ~ IC.Man + MD.ManR, data = Datos, family = binomial)
  #-------------------------------------- IC.Man + MD.ManL -------------------------------------
  log2var_5 = glm(Sex ~ IC.Man + MD.ManL, data = Datos, family = binomial)
  #-------------------------------------- IC.MAX + MD.MaxL -------------------------------------
  log2var_6 = glm(Sex ~ IC.Max + MD.MaxL, data = Datos, family = binomial)
  #-------------------------------------- IC.MAX + MD.ManR -------------------------------------
  log2var_7 = glm(Sex ~ IC.Max + MD.ManR, data = Datos, family = binomial)
  #-------------------------------------- IC.MAX + MD.ManL -------------------------------------
  log2var_8 = glm(Sex ~ IC.Max + MD.ManL, data = Datos, family = binomial)
  #--------------------------------- MD.MaxR + IC.Man + MD.ManL --------------------------------
  log3var_1 = glm(Sex ~ MD.MaxR + IC.Man + MD.ManL, data = Datos, family = binomial)
  #--------------------------------- IC.Max + IC.Man + MD.ManR ---------------------------------
  log3var_2 = glm(Sex ~ IC.Max + IC.Man + MD.ManR, data = Datos, family = binomial)
  #--------------------------------- IC.Max + IC.Man + MD.ManL ---------------------------------
  log3var_3 = glm(Sex ~ IC.Max + IC.Man + MD.ManL, data = Datos, family = binomial)
  #-------------------------------- MD.MaxR + MD.MaxL + MD.ManL --------------------------------
  log3var_4 = glm(Sex ~ MD.MaxR + MD.MaxL + MD.ManL, data = Datos, family = binomial)
  #-------------------------------- MD.MaxR + MD.ManR + MD.ManL --------------------------------
  log3var_5 = glm(Sex ~ MD.MaxR + MD.ManR + MD.ManL, data = Datos, family = binomial)
  #---------------------------- MD.MaxR + MD.MaxL + IC.Man + MD.ManR ---------------------------
  log4var_1 = glm(Sex ~ MD.MaxR + MD.MaxL + IC.Man + MD.ManR, data = Datos, family = binomial)
  #---------------------------- IC.Max + MD.MaxR + MD.MaxL + MD.ManL ---------------------------
  log4var_2 = glm(Sex ~ IC.Max + MD.MaxR + MD.MaxL + MD.ManL, data = Datos, family = binomial)
  #----------------------- IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR -----------------------
  log5var = glm(Sex ~ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR, data = Datos, family = binomial)
  #------------------ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR + MD.ManL ------------------
  log6var = glm(Sex ~ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR + MD.ManL, data = Datos, family = binomial)
  
  
  ##############################################################################################
  ################################# C L A S I F I C A C I O N ##################################
  ##############################################################################################
  #-------------------------------------------IC.MAX--------------------------------------------
  Datos$Sex_Estimado_ICMax <- as.numeric(log_ICMax$fitted.values>=0.5)
  Datos$Sex_Estimado_ICMax <-factor(Datos$Sex_Estimado_ICMax, labels=levels(Datos$Sex))
  Datos$Classified_ICMax <- as.numeric(Datos$Sex == Datos$Sex_Estimado_ICMax)
  Datos$Classified_ICMax[Datos$Classified_ICMax == 0] <- 'incorrectly'
  Datos$Classified_ICMax[Datos$Classified_ICMax == 1] <- 'correctly'
  levels(Datos$Sex_Estimado_ICMax) <- c("Male", "Female")
  #-------------------------------------------MD.MAXR-------------------------------------------
  Datos$Sex_Estimado_MDMaxR <- as.numeric(log_MDMaxR$fitted.values>=0.5)
  Datos$Sex_Estimado_MDMaxR <-factor(Datos$Sex_Estimado_MDMaxR, labels=levels(Datos$Sex))
  Datos$Classified_MDMaxR <- as.numeric(Datos$Sex == Datos$Sex_Estimado_MDMaxR)
  Datos$Classified_MDMaxR[Datos$Classified_MDMaxR == 0] <- 'incorrectly'
  Datos$Classified_MDMaxR[Datos$Classified_MDMaxR == 1] <- 'correctly'
  levels(Datos$Sex_Estimado_MDMaxR) <- c("Male", "Female")
  #-------------------------------------------MD.MAXL-------------------------------------------
  Datos$Sex_Estimado_MDMaxL <- as.numeric(log_MDMaxL$fitted.values>=0.5)
  Datos$Sex_Estimado_MDMaxL <-factor(Datos$Sex_Estimado_MDMaxL, labels=levels(Datos$Sex))
  Datos$Classified_MDMaxL <- as.numeric(Datos$Sex == Datos$Sex_Estimado_MDMaxL)
  Datos$Classified_MDMaxL[Datos$Classified_MDMaxL == 0] <- 'incorrectly'
  Datos$Classified_MDMaxL[Datos$Classified_MDMaxL == 1] <- 'correctly'
  Datos$Prediction_MDMaxL <- 1-log_MDMaxL$fitted.values
  levels(Datos$Sex_Estimado_MDMaxL) <- c("Male", "Female")
  #-------------------------------------------MD.MANR-------------------------------------------
  Datos$Sex_Estimado_MDManR <- as.numeric(log_MDManR$fitted.values>=0.5)
  Datos$Sex_Estimado_MDManR <-factor(Datos$Sex_Estimado_MDManR, labels=levels(Datos$Sex))
  Datos$Classified_MDManR <- as.numeric(Datos$Sex == Datos$Sex_Estimado_MDManR)
  Datos$Classified_MDManR[Datos$Classified_MDManR == 0] <- 'incorrectly'
  Datos$Classified_MDManR[Datos$Classified_MDManR == 1] <- 'correctly'
  levels(Datos$Sex_Estimado_MDManR) <- c("Male", "Female")
  #-------------------------------------------MD.MANL-------------------------------------------
  Datos$Sex_Estimado_MDManL <- as.numeric(log_MDManL$fitted.values>=0.5)
  Datos$Sex_Estimado_MDManL <-factor(Datos$Sex_Estimado_MDManL, labels=levels(Datos$Sex))
  Datos$Classified_MDManL <- as.numeric(Datos$Sex == Datos$Sex_Estimado_MDManL)
  Datos$Classified_MDManL[Datos$Classified_MDManL == 0] <- 'incorrectly'
  Datos$Classified_MDManL[Datos$Classified_MDManL == 1] <- 'correctly'
  Datos$Prediction_MDManL <- 1-log_MDManL$fitted.values
  levels(Datos$Sex_Estimado_MDManL) <- c("Male", "Female")
  #-------------------------------------- MD.MaxR + IC.Man -------------------------------------
  Datos$Sex_Estimado_2_1 <- as.numeric(log2var_1$fitted.values>=0.5)
  Datos$Sex_Estimado_2_1 <-factor(Datos$Sex_Estimado_2_1, labels=levels(Datos$Sex))
  Datos$Classified_2_1 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_1)
  Datos$Classified_2_1[Datos$Classified_2_1 == 0] <- 'incorrectly'
  Datos$Classified_2_1[Datos$Classified_2_1 == 1] <- 'correctly'
  Datos$Prediction_2_1 <- 1-log2var_1$fitted.values
  levels(Datos$Sex_Estimado_2_1) <- c("Male", "Female")
  #------------------------------------- MD.MaxR + MD.ManR -------------------------------------
  Datos$Sex_Estimado_2_2 <- as.numeric(log2var_2$fitted.values>=0.5)
  Datos$Sex_Estimado_2_2 <-factor(Datos$Sex_Estimado_2_2, labels=levels(Datos$Sex))
  Datos$Classified_2_2 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_2)
  Datos$Classified_2_2[Datos$Classified_2_2 == 0] <- 'incorrectly'
  Datos$Classified_2_2[Datos$Classified_2_2 == 1] <- 'correctly'
  Datos$Prediction_2_2 <- 1-log2var_2$fitted.values
  levels(Datos$Sex_Estimado_2_2) <- c("Male", "Female")
  #------------------------------------- MD.MaxL + MD.ManL -------------------------------------
  Datos$Sex_Estimado_2_3 <- as.numeric(log2var_3$fitted.values>=0.5)
  Datos$Sex_Estimado_2_3 <-factor(Datos$Sex_Estimado_2_3, labels=levels(Datos$Sex))
  Datos$Classified_2_3 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_3)
  Datos$Classified_2_3[Datos$Classified_2_3 == 0] <- 'incorrectly'
  Datos$Classified_2_3[Datos$Classified_2_3 == 1] <- 'correctly'
  Datos$Prediction_2_3 <- 1-log2var_3$fitted.values
  levels(Datos$Sex_Estimado_2_3) <- c("Male", "Female")
  #------------------------------------- IC.Man + MD.ManR --------------------------------------
  Datos$Sex_Estimado_2_4 <- as.numeric(log2var_4$fitted.values>=0.5)
  Datos$Sex_Estimado_2_4 <-factor(Datos$Sex_Estimado_2_4, labels=levels(Datos$Sex))
  Datos$Classified_2_4 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_4)
  Datos$Classified_2_4[Datos$Classified_2_4 == 0] <- 'incorrectly'
  Datos$Classified_2_4[Datos$Classified_2_4 == 1] <- 'correctly'
  Datos$Prediction_2_4 <- 1-log2var_4$fitted.values
  levels(Datos$Sex_Estimado_2_4) <- c("Male", "Female")
  #-------------------------------------- IC.Man + MD.ManL -------------------------------------
  Datos$Sex_Estimado_2_5 <- as.numeric(log2var_5$fitted.values>=0.5)
  Datos$Sex_Estimado_2_5 <-factor(Datos$Sex_Estimado_2_5, labels=levels(Datos$Sex))
  Datos$Classified_2_5 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_5)
  Datos$Classified_2_5[Datos$Classified_2_5 == 0] <- 'incorrectly'
  Datos$Classified_2_5[Datos$Classified_2_5 == 1] <- 'correctly'
  Datos$Prediction_2_5 <- 1-log2var_5$fitted.values
  levels(Datos$Sex_Estimado_2_5) <- c("Male", "Female")
  #-------------------------------------- IC.MAX + MD.MaxL -------------------------------------
  Datos$Sex_Estimado_2_6 <- as.numeric(log2var_6$fitted.values>=0.5)
  Datos$Sex_Estimado_2_6 <-factor(Datos$Sex_Estimado_2_6, labels=levels(Datos$Sex))
  Datos$Classified_2_6 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_6)
  Datos$Classified_2_6[Datos$Classified_2_6 == 0] <- 'incorrectly'
  Datos$Classified_2_6[Datos$Classified_2_6 == 1] <- 'correctly'
  Datos$Prediction_2_6 <- 1-log2var_6$fitted.values
  levels(Datos$Sex_Estimado_2_6) <- c("Male", "Female")
  #-------------------------------------- IC.MAX + MD.ManR -------------------------------------
  Datos$Sex_Estimado_2_7 <- as.numeric(log2var_7$fitted.values>=0.5)
  Datos$Sex_Estimado_2_7 <-factor(Datos$Sex_Estimado_2_7, labels=levels(Datos$Sex))
  Datos$Classified_2_7 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_7)
  Datos$Classified_2_7[Datos$Classified_2_7 == 0] <- 'incorrectly'
  Datos$Classified_2_7[Datos$Classified_2_7 == 1] <- 'correctly'
  levels(Datos$Sex_Estimado_2_7) <- c("Male", "Female")
  #-------------------------------------- IC.MAX + MD.ManL -------------------------------------
  Datos$Sex_Estimado_2_8 <- as.numeric(log2var_8$fitted.values>=0.5)
  Datos$Sex_Estimado_2_8 <-factor(Datos$Sex_Estimado_2_8, labels=levels(Datos$Sex))
  Datos$Classified_2_8 <- as.numeric(Datos$Sex == Datos$Sex_Estimado_2_8)
  Datos$Classified_2_8[Datos$Classified_2_8 == 0] <- 'incorrectly'
  Datos$Classified_2_8[Datos$Classified_2_8 == 1] <- 'correctly'
  Datos$Prediction_2_8 <- 1-log2var_8$fitted.values
  levels(Datos$Sex_Estimado_2_8) <- c("Male", "Female")
  #--------------------------------- MD.MaxR + IC.Man + MD.ManL --------------------------------
  Datos$Sex_Estimado_3_1 <- as.numeric(log3var_1$fitted.values>=0.5)
  Datos$Sex_Estimado_3_1 <-factor(Datos$Sex_Estimado_3_1, labels=levels(Datos$Sex))
  Datos$Prediction_3_1 <- 1-log3var_1$fitted.values
  levels(Datos$Sex_Estimado_3_1) <- c("Male", "Female")
  #--------------------------------- IC.Max + IC.Man + MD.ManR ---------------------------------
  Datos$Sex_Estimado_3_2 <- as.numeric(log3var_2$fitted.values>=0.5)
  Datos$Sex_Estimado_3_2 <-factor(Datos$Sex_Estimado_3_2, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_3_2) <- c("Male", "Female")
  #--------------------------------- IC.Max + IC.Man + MD.ManL ---------------------------------
  Datos$Sex_Estimado_3_3 <- as.numeric(log3var_3$fitted.values>=0.5)
  Datos$Sex_Estimado_3_3 <-factor(Datos$Sex_Estimado_3_3, labels=levels(Datos$Sex))
  Datos$Prediction_3_3 <- 1-log3var_3$fitted.values
  levels(Datos$Sex_Estimado_3_3) <- c("Male", "Female")
  #-------------------------------- MD.MaxR + MD.MaxL + MD.ManL --------------------------------
  Datos$Sex_Estimado_3_4 <- as.numeric(log3var_4$fitted.values>=0.5)
  Datos$Sex_Estimado_3_4 <-factor(Datos$Sex_Estimado_3_4, labels=levels(Datos$Sex))
  Datos$Prediction_3_4 <- 1-log3var_4$fitted.values
  levels(Datos$Sex_Estimado_3_4) <- c("Male", "Female")
  #-------------------------------- MD.MaxR + MD.ManR + MD.ManL --------------------------------
  Datos$Sex_Estimado_3_5 <- as.numeric(log3var_5$fitted.values>=0.5)
  Datos$Sex_Estimado_3_5 <-factor(Datos$Sex_Estimado_3_5, labels=levels(Datos$Sex))
  Datos$Prediction_3_5 <- 1-log3var_5$fitted.values
  levels(Datos$Sex_Estimado_3_5) <- c("Male", "Female")
  #---------------------------- MD.MaxR + MD.MaxL + IC.Man + MD.ManR ---------------------------
  Datos$Sex_Estimado_4_1 <- as.numeric(log4var_1$fitted.values>=0.5)
  Datos$Sex_Estimado_4_1 <-factor(Datos$Sex_Estimado_4_1, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_4_1) <- c("Male", "Female")
  #---------------------------- IC.Max + MD.MaxR + MD.MaxL + MD.ManL ---------------------------
  Datos$Sex_Estimado_4_2 <- as.numeric(log4var_2$fitted.values>=0.5)
  Datos$Sex_Estimado_4_2 <-factor(Datos$Sex_Estimado_4_2, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_4_2) <- c("Male", "Female")
  #----------------------- IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR -----------------------
  Datos$Sex_Estimado_5_1 <- as.numeric(log5var$fitted.values>=0.5)
  Datos$Sex_Estimado_5_1 <-factor(Datos$Sex_Estimado_5_1, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_5_1) <- c("Male", "Female")
  #------------------ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR + MD.ManL ------------------
  Datos$Sex_Estimado_6_1 <- as.numeric(log6var$fitted.values>=0.5)
  Datos$Sex_Estimado_6_1 <-factor(Datos$Sex_Estimado_6_1, labels=levels(Datos$Sex))
  Datos$Prediction_6_1 <- 1-log6var$fitted.values
  levels(Datos$Sex_Estimado_6_1) <- c("Male", "Female")
  
  levels(Datos$Sex) <- c("Male", "Female")
  
  
  observeEvent(input$resultado, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Resultados")
  })
  
  output$resultsText1_1_1 <- renderUI({
    text1 <- "With a logistic model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    switch(input$medida,
           "IC.Max" = HTML(paste(paste(text1,
                                       "Maxillary intercanine width",
                                       text2,
                                       if (predict(log_ICMax,data.frame(IC.Max=c(input$valMed)))>0) "Female" else "Male",
                                       text3,
                                       if (predict(log_ICMax,data.frame(IC.Max=c(input$valMed)),type="response")>0.5) round(predict(log_ICMax,data.frame(IC.Max=c(input$valMed)),type="response"),2) else round(1-predict(log_ICMax,data.frame(IC.Max=c(input$valMed)),type="response"),2)  
           ),
           paste(text4,
                 round(caret::confusionMatrix(Datos$Sex_Estimado_ICMax, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
                 text5),
           sep = '<br/>'
           )
           ),
           "MD.MaxR" = HTML(paste(paste(text1,
                                        "13's mesiodistal width",
                                        text2,
                                        if (predict(log_MDMaxR,data.frame(MD.MaxR=c(input$valMed)))>0) "Female" else "Male",
                                        text3,
                                        if (predict(log_MDMaxR,data.frame(MD.MaxR=c(input$valMed)),type="response")>0.5) round(predict(log_MDMaxR,data.frame(MD.MaxR=c(input$valMed)),type="response"),2) else round(1-predict(log_MDMaxR,data.frame(MD.MaxR=c(input$valMed)),type="response"),2)
           ),
           paste(text4,
                 round(caret::confusionMatrix(Datos$Sex_Estimado_MDMaxR, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
                 text5),
           sep = '<br/>'
           )
           ),
           "MD.MaxL" = HTML(paste(paste(text1,
                                        "23's mesiodistal width",
                                        text2,
                                        if (predict(log_MDMaxL,data.frame(MD.MaxL=c(input$valMed)))>0) "Female" else "Male",
                                        text3,
                                        if (predict(log_MDMaxL,data.frame(MD.MaxL=c(input$valMed)),type="response")>0.5) round(predict(log_MDMaxL,data.frame(MD.MaxL=c(input$valMed)),type="response"),2) else round(1-predict(log_MDMaxL,data.frame(MD.MaxL=c(input$valMed)),type="response"),2)
           ),
           paste(text4,
                 round(caret::confusionMatrix(Datos$Sex_Estimado_MDMaxL, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
                 text5),
           sep = '<br/>'
           )
           ),
           "MD.ManL" = HTML(paste(paste(text1,
                                        "33's mesiodistal width",
                                        text2,
                                        if (predict(log_MDManL,data.frame(MD.ManL=c(input$valMed)))>0) "Female" else "Male",
                                        text3,
                                        if (predict(log_MDManL,data.frame(MD.ManL=c(input$valMed)),type="response")>0.5) round(predict(log_MDManL,data.frame(MD.ManL=c(input$valMed)),type="response"),2) else round(1-predict(log_MDManL,data.frame(MD.ManL=c(input$valMed)),type="response"),2)
           ),
           paste(text4,
                 round(caret::confusionMatrix(Datos$Sex_Estimado_MDManL, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
                 text5),
           sep = '<br/>'
           )
           ),
           "MD.ManR" = HTML(paste(paste(text1,
                                        "43's mesiodistal width",
                                        text2,
                                        if (predict(log_MDManR,data.frame(MD.ManR=c(input$valMed)))>0) "Female" else "Male",
                                        text3,
                                        if (predict(log_MDManR,data.frame(MD.ManR=c(input$valMed)),type="response")>0.5) round(predict(log_MDManR,data.frame(MD.ManR=c(input$valMed)),type="response"),2) else round(1-predict(log_MDManR,data.frame(MD.ManR=c(input$valMed)),type="response"),2)
           ),
           paste(text4,
                 round(caret::confusionMatrix(Datos$Sex_Estimado_MDManR, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
                 text5),
           sep = '<br/>'
           )
           )
    )
  })
  
  output$resultsPlots1_1_1 <- renderPlotly({
    colorBxF <- app_colors$box_fill
    colorBxC <- app_colors$box_border
    colorManual <- app_colors$classified
    
    switch(input$medida,
           "IC.Max" = ggplot(Datos, aes(x=Sex, y=IC.Max, col=Classified_ICMax)) +
             geom_boxplot(aes(group=Sex), fill=colorBxF, color=colorBxC, alpha=0.5)+
             geom_jitter(position=position_jitter(0.2), alpha=0.5) + scale_colour_manual(name="Classified", values=colorManual),
           "MD.MaxR" = ggplot(Datos, aes(x=Sex, y=MD.MaxR, col=Classified_MDMaxR)) +
             geom_boxplot(aes(group=Sex), fill=colorBxF, color=colorBxC, alpha=0.5)+
             geom_jitter(position=position_jitter(0.2), alpha=0.5) + scale_colour_manual(name="Classified", values=colorManual),
           "MD.MaxL" = ggplot(Datos, aes(x=Sex, y=MD.MaxL, col=Classified_MDMaxL)) +
             geom_boxplot(aes(group=Sex), fill=colorBxF, color=colorBxC, alpha=0.5)+
             geom_jitter(position=position_jitter(0.2), alpha=0.5) + scale_colour_manual(name="Classified", values=colorManual),
           "MD.ManL" = ggplot(Datos, aes(x=Sex, y=MD.ManL, col=Classified_MDManL)) +
             geom_boxplot(aes(group=Sex), fill=colorBxF, color=colorBxC, alpha=0.5)+
             geom_jitter(position=position_jitter(0.2), alpha=0.5) + scale_colour_manual(name="Classified", values=colorManual),
           "MD.ManR" = ggplot(Datos, aes(x=Sex, y=MD.ManR, col=Classified_MDManR)) +
             geom_boxplot(aes(group=Sex), fill=colorBxF, color=colorBxC, alpha=0.5)+
             geom_jitter(position=position_jitter(0.2), alpha=0.5) + scale_colour_manual(name="Classified", values=colorManual)
    )
  })
  
  output$resultsPlots1_1_2 <- renderPlotly({
    colorSex <- app_colors$sex
    colorManual <- app_colors$classified
    
    switch(input$medida,
           "IC.Max" = ggplot()+
             geom_point(aes(x=Datos$IC.Max, y=Datos$SexNum, color=Datos$Sex))+
             geom_point(aes(x=Datos$IC.Max, y=log_ICMax$fitted.values, color=Datos$Classified_ICMax), alpha=0.5)+
             scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none"),
           "MD.MaxR" = ggplot()+
             geom_point(aes(x=Datos$MD.MaxR, y=Datos$SexNum, color=Datos$Sex))+
             geom_point(aes(x=Datos$MD.MaxR, y=log_MDMaxR$fitted.values, color=Datos$Classified_MDMaxR), alpha=0.5)+
             scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none"),
           "MD.MaxL" = ggplot()+
             geom_point(aes(x=Datos$MD.MaxL, y=Datos$SexNum, color=Datos$Sex))+
             geom_point(aes(x=Datos$MD.MaxL, y=log_MDMaxL$fitted.values, color=Datos$Classified_MDMaxL))+
             scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none"),
           "MD.ManL" = ggplot()+
             geom_point(aes(x=Datos$MD.ManL, y=Datos$SexNum, color=Datos$Sex))+
             geom_point(aes(x=Datos$MD.ManL, y=log_MDManL$fitted.values, color=Datos$Classified_MDManL), alpha=0.5)+
             scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none"),
           "MD.ManR" = ggplot()+
             geom_point(aes(x=Datos$MD.ManR, y=Datos$SexNum, color=Datos$Sex))+
             geom_point(aes(x=Datos$MD.ManR, y=log_MDManR$fitted.values, color=Datos$Classified_MDManR), alpha=0.5)+
             scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    )
  })
  
  
  output$resultsText2_1_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering the variables: "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_1))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_2))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and 43's mesiodistal width",
                       text2,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_2, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_3))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      ) 
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_4))){
      HTML(paste(paste(text1,
                       "43's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_4, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_5))){
      HTML(paste(paste(text1,
                       "33's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_5, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )  
    }
  })
  
  output$resultsPlots2_1_1 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    colorClass <- c("green3","red3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_1))){
      ggplot(Datos, aes(x=MD.MaxR, y=IC.Man, col=Sex))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Real Sex", values=colorSex)+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(22,33.2))      
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_2))){
      ggplot(Datos, aes(x=MD.MaxR, y=MD.ManR, col=Sex))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Real Sex", values=colorSex)+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(5.6,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_3))){
      ggplot(Datos, aes(x=MD.MaxL, y=MD.ManL, col=Sex))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Real Sex", values=colorSex)+
        scale_x_continuous(limits = c(6.8,9.2))+
        scale_y_continuous(limits = c(5.5,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_4))){
      ggplot(Datos, aes(x=MD.ManR, y=IC.Man, col=Sex))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Real Sex", values=colorSex)+
        scale_x_continuous(limits = c(5.6,8.2))+
        scale_y_continuous(limits = c(22,33.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_5))){
      ggplot(Datos, aes(x=MD.ManL, y=IC.Man, col=Sex))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Real Sex", values=colorSex)+
        scale_x_continuous(limits = c(5.5,8.2))+
        scale_y_continuous(limits = c(22,33.2))
    }
  })
  
  output$resultsPlots2_1_2 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    colorClass <- c("green3","red3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_1))){
      ggplot(Datos, aes(x=MD.MaxR, y=IC.Man, col=Sex_Estimado_2_1))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Estimate Sex", values=colorSex)+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(22,33.2))   
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_2))){
      ggplot(Datos, aes(x=MD.MaxR, y=MD.ManR, col=Sex_Estimado_2_2))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Estimate Sex", values=colorSex)+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(5.6,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_3))){
      ggplot(Datos, aes(x=MD.MaxL, y=MD.ManL, col=Sex_Estimado_2_3))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Estimate Sex", values=colorSex)+
        scale_x_continuous(limits = c(6.8,9.2))+
        scale_y_continuous(limits = c(5.5,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_4))){
      ggplot(Datos, aes(x=MD.ManR, y=IC.Man, col=Sex_Estimado_2_4))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Estimate Sex", values=colorSex)+
        scale_x_continuous(limits = c(5.6,8.2))+
        scale_y_continuous(limits = c(22,33.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_5))){
      ggplot(Datos, aes(x=MD.ManL, y=IC.Man, col=Sex_Estimado_2_5))+
        geom_jitter(position=position_jitter(0.2)) + 
        scale_colour_manual(name="Estimate Sex", values=colorSex)+
        scale_x_continuous(limits = c(5.5,8.2))+
        scale_y_continuous(limits = c(22,33.2))
    }
  })
  
  output$resultsPlots2_1_3 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    colorClass <- c("green3","red3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_1))){
      ggplot(Datos[Datos$Sex_Estimado_2_1 == 'Male',], aes(x=MD.MaxR, y=IC.Man, col=Classified_2_1))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(values=colorClass)+ guides(colour="none")+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(22,33.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_2))){
      ggplot(Datos[Datos$Sex_Estimado_2_2 == 'Male',], aes(x=MD.MaxR, y=MD.ManR, col=Classified_2_2))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(values=colorClass)+ guides(colour="none")+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(5.6,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_3))){
      ggplot(Datos[Datos$Sex_Estimado_2_3 == 'Male',], aes(x=MD.MaxL, y=MD.ManL, col=Classified_2_3))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(values=colorClass)+ guides(colour="none")+
        scale_x_continuous(limits = c(6.8,9.2))+
        scale_y_continuous(limits = c(5.5,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_4))){
      ggplot(Datos[Datos$Sex_Estimado_2_4 == 'Male',], aes(x=MD.ManR, y=IC.Man, col=Classified_2_4))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(values=colorClass)+ guides(colour="none")+
        scale_x_continuous(limits = c(5.6,8.2))+
        scale_y_continuous(limits = c(22,33.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_5))){
      ggplot(Datos[Datos$Sex_Estimado_2_5 == 'Male',], aes(x=MD.ManL, y=IC.Man, col=Classified_2_5))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(values=colorClass)+ guides(colour="none")+
        scale_x_continuous(limits = c(5.5,8.2))+
        scale_y_continuous(limits = c(22,33.2)) 
    }
  })
  
  output$resultsPlots2_1_4 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    colorClass <- c("green3","red3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_1))){
      ggplot(Datos[Datos$Sex_Estimado_2_1 == 'Female',], aes(x=MD.MaxR, y=IC.Man, col=Classified_2_1))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(name="Classified", values=colorClass)+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(22,33.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_2))){
      ggplot(Datos[Datos$Sex_Estimado_2_2 == 'Female',], aes(x=MD.MaxR, y=MD.ManR, col=Classified_2_2))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(name="Classified", values=colorClass)+
        scale_x_continuous(limits = c(6.2,9.5))+
        scale_y_continuous(limits = c(5.6,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_3))){
      ggplot(Datos[Datos$Sex_Estimado_2_3 == 'Female',], aes(x=MD.MaxL, y=MD.ManL, col=Classified_2_3))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(name="Classified", values=colorClass)+
        scale_x_continuous(limits = c(6.8,9.2))+
        scale_y_continuous(limits = c(5.5,8.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_4))){
      ggplot(Datos[Datos$Sex_Estimado_2_4 == 'Female',], aes(x=MD.ManR, y=IC.Man, col=Classified_2_4))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(name="Classified", values=colorClass)+
        scale_x_continuous(limits = c(5.6,8.2))+
        scale_y_continuous(limits = c(22,33.2))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2mod_5))){
      ggplot(Datos[Datos$Sex_Estimado_2_5 == 'Female',], aes(x=MD.ManL, y=IC.Man, col=Classified_2_5))+
        geom_jitter(position=position_jitter(0.2)) +
        scale_colour_manual(name="Classified", values=colorClass)+
        scale_x_continuous(limits = c(5.5,8.2))+
        scale_y_continuous(limits = c(22,33.2)) 
    }
  })
  
  
  output$resultsText2_2_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering the variables: "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is"
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2modAgrega_1))){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width and 23's mesiodistal width",
                       text2,
                       if (predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_6, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2modAgrega_3))){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_8, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2modAgrega_2))){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width and 43's mesiodistal width",
                       text2,
                       if (predict(log2var_7,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_7,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log2var_7,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log2var_7,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2)  
                       
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_7, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots2_2_1 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2modAgrega_1))){
      ggplot(Datos, aes(x=Sex, y=log2var_6$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2modAgrega_2))){
      ggplot(Datos, aes(x=Sex, y=log2var_7$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var2modAgrega_3))){
      ggplot(Datos, aes(x=Sex, y=log2var_8$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsText2_2_2 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "Also, there is a model considering the variable "
    text2 <- ", with the same percentage of accuracy. With this model, there is a "
    text3 <- " of probability that the estimate sex is "
    
    if("MD.MaxL" %in% medidas){
      HTML(paste(text1,
                 "23's mesiodistal width",
                 text2,
                 if (predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response")>0.5) round(predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2) else round(1-predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2),
                 text3,
                 if (predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]))>0) "Female" else "Male"
      )
      )
    }else if("MD.ManL" %in% medidas){
      HTML(paste(text1,
                 "33's mesiodistal width",
                 text2,
                 if (predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }else if("MD.ManR" %in% medidas){
      HTML(paste(text1,
                 "43's mesiodistal width",
                 text2,
                 if (predict(log_MDManR,list(MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log_MDManR,list(MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log_MDManR,list(MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2),
                 text3,
                 if (predict(log_MDManR,list(MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male"
      )
      )
    }
  })
  
  output$resultsPlots2_2_2 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    colorManual <- app_colors$classified
    
    if("MD.MaxL" %in% medidas){
      ggplot()+
        geom_point(aes(x=Datos$MD.MaxL, y=Datos$SexNum, color=Datos$Sex))+
        geom_point(aes(x=Datos$MD.MaxL, y=log_MDMaxL$fitted.values, color=Datos$Classified_MDMaxL))+
        scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    }else if("MD.ManR" %in% medidas){
      ggplot()+
        geom_point(aes(x=Datos$MD.ManR, y=Datos$SexNum, color=Datos$Sex))+
        geom_point(aes(x=Datos$MD.ManR, y=log_MDManR$fitted.values, color=Datos$Classified_MDManR), alpha=0.5)+
        scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    }else if("MD.ManL" %in% medidas){
      ggplot()+
        geom_point(aes(x=Datos$MD.ManL, y=Datos$SexNum, color=Datos$Sex))+
        geom_point(aes(x=Datos$MD.ManL, y=log_MDManL$fitted.values, color=Datos$Classified_MDManL), alpha=0.5)+
        scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    }
  })
  
  
  output$resultsText2_3_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if("IC.Max" %in% medidas){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width",
                       text2,
                       if (predict(log_ICMax,list(IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log_ICMax,list(IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log_ICMax,list(IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log_ICMax,list(IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_ICMax, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if("MD.MaxL" %in% medidas){
      HTML(paste(paste(text1,
                       "23's mesiodistal width",
                       text2,
                       if (predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response")>0.5) round(predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2) else round(1-predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_MDMaxL, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if("MD.ManL" %in% medidas){
      HTML(paste(paste(text1,
                       "33's mesiodistal width",
                       text2,
                       if (predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_MDManL, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots2_3_1 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    colorManual <- app_colors$classified
    
    if("IC.Max" %in% medidas){
      ggplot()+
        geom_point(aes(x=Datos$IC.Max, y=Datos$SexNum, color=Datos$Sex))+
        geom_point(aes(x=Datos$IC.Max, y=log_ICMax$fitted.values, color=Datos$Classified_ICMax), alpha=0.5)+
        scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    }else if("MD.MaxL" %in% medidas){
      ggplot()+
        geom_point(aes(x=Datos$MD.MaxL, y=Datos$SexNum, color=Datos$Sex))+
        geom_point(aes(x=Datos$MD.MaxL, y=log_MDMaxL$fitted.values, color=Datos$Classified_MDMaxL))+
        scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    }else if("MD.ManL" %in% medidas){
      ggplot()+
        geom_point(aes(x=Datos$MD.ManL, y=Datos$SexNum, color=Datos$Sex))+
        geom_point(aes(x=Datos$MD.ManL, y=log_MDManL$fitted.values, color=Datos$Classified_MDManL), alpha=0.5)+
        scale_colour_manual(values=c(colorSex,colorManual))+ guides(colour="none")
    }
  })
  
  
  
  output$resultsText3_1_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering the variables: "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    HTML(paste(paste(text1,
                     "MD13's mesiodistal width, Mandibular intercanine width and MD33's mesiodistal width",
                     text2,
                     if (predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                     text3,
                     if (predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)
    ),
    paste(text4,
          round(caret::confusionMatrix(Datos$Sex_Estimado_3_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
          text5),
    sep = '<br/>'
    )
    )
  })
  
  output$resultsPlots3_1_1 <- renderPlotly({
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log3var_1$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsPlots3_1_2 <- renderPlotly({
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    ggplot()+
      geom_path(data=pr_curve(Datos, Sex, Prediction_3_1), aes(x=recall, y=precision, color="Current model")) +
      geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
      geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
      scale_color_manual(name=NULL, values=colores) + 
      theme(legend.position = c(0.2,0.5))
  })
  
  output$resultsText3_2_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering the variables: "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is"
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_1))){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width, Mandibular intercanine width and 43's mesiodistal width",
                       text2,
                       if (predict(log3var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log3var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log3var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_2, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
      
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_2))){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width, Mandibular intercanine width and 33's mesiodistal width",
                       text2,
                       if (predict(log3var_3,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_3,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_3,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_3,list(IC.Max=valores[[which(medidas == "IC.Max")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_3))){
      HTML(paste(paste(text1,
                       "13's, 23's and 33's mesiodistal width",
                       text2,
                       if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_4, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_4))){
      HTML(paste(paste(text1,
                       "13's, 33's and 43's mesiodistal width",
                       text2,
                       if (predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_5, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots3_2_1 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_1))){
      ggplot(Datos, aes(x=Sex, y=log3var_2$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_2))){
      ggplot(Datos, aes(x=Sex, y=log3var_3$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_3))){
      ggplot(Datos, aes(x=Sex, y=log3var_4$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_4))){
      ggplot(Datos, aes(x=Sex, y=log3var_5$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsText3_2_2 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "Also, there is a model considering the variables "
    text2 <- ", with the same percentage of accuracy. With this model, there is a "
    text3 <- " of probability that the estimate sex is "
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_1))){
      HTML(paste(text1,
                 "Mandibular intercanine width and 43's mesiodistal width",
                 text2,
                 if (predict(log2var_4,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_4,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_4,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2),
                 text3,
                 if (predict(log2var_4,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_2))){
      HTML(paste(text1,
                 "Mandibular intercanine width and 33's mesiodistal width",
                 text2,
                 if (predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_3))){
      HTML(paste(text1,
                 "23's mesiodistal width and 33's mesiodistal width",
                 text2,
                 if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_4))){
      HTML(paste(text1,
                 "13's mesiodistal width and 43's mesiodistal width",
                 text2,
                 if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2),
                 text3,
                 if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male"
      )
      )
    }
  })
  
  output$resultsPlots3_2_2 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_1))){
      ggplot(Datos, aes(x=Sex, y=log2var_4$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_2))){
      ggplot(Datos, aes(x=Sex, y=log2var_5$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_3))){
      ggplot(Datos, aes(x=Sex, y=log2var_3$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modAgrega_4))){
      ggplot(Datos, aes(x=Sex, y=log2var_2$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsText3_3_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_1)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_2))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_4))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and 43's mesiodistal width",
                       text2,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_2, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_5)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_6)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_7))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_8))){
      HTML(paste(paste(text1,
                       "Mandibular intercanine width and 43's mesiodistal width",
                       text2,
                       if (predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_4, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_9))){
      HTML(paste(paste(text1,
                       "Mandibular intercanine width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_5,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_5, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots3_3_1 <- renderUI({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_1)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_2))){
      ggplot(Datos, aes(x=Sex, y=log2var_1$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_4))){
      ggplot(Datos, aes(x=Sex, y=log2var_2$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_5)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_6)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_7))){
      ggplot(Datos, aes(x=Sex, y=log2var_3$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_8))){
      ggplot(Datos, aes(x=Sex, y=log2var_4$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_9))){
      ggplot(Datos, aes(x=Sex, y=log2var_5$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsPlots3_3_2 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_1)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_2))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_1), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_4))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_2), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_5)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_6)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_7))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_3), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_8))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_4), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_9))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_5), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  output$resultsText3_3_2 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With this first model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_10))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_11)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_12)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_13))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width",
                       text2,
                       if (predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response")>0.5) round(predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2) else round(1-predict(log_MDMaxL,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_MDMaxL, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_14)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_15))){
      HTML(paste(paste(text1,
                       "33's mesiodistal width",
                       text2,
                       if (predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log_MDManL,list(MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_MDManL, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots3_3_3 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_10))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_1), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_11)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_12)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_13))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_MDMaxL), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_14)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_15))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_MDManL), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  output$resultsText3_3_3 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With the same accuracy and now considering the variables "
    text2 <-", with a probability of "
    text3 <- ", this second model classifies your input as "
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_10))){
      HTML(paste(text1,
                 "13's mesiodistal width and 43's mesiodistal width",
                 text2,
                 if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2),
                 text3,
                 if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_11)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_12)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_13))){
      HTML(paste(text1,
                 "23's mesiodistal width and Maxillary intercanine width",
                 text2,
                 if (predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2),
                 text3,
                 if (predict(log2var_6,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_14)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_15))){
      HTML(paste(text1,
                 "33's mesiodistal width and Maxillary intercanine width",
                 text2,
                 if (predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2),
                 text3,
                 if (predict(log2var_8,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male"
      )
      )
    }
  })
  
  output$resultsPlots3_3_4 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_10))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_2), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_11)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_12)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_13))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_6), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_14)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var3modSustituye_15))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_8), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  
  output$resultsText4_2_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering the variables: "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is"
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_1))){
      HTML(paste(paste(text1,
                       "Mandibular intercanine width, 13's, 23's and 43's mesiodistal width",
                       text2,
                       if (predict(log4var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log4var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log4var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log4var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_4_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_2))){
      HTML(paste(paste(text1,
                       "Maxillary intercanine width, 13's, 23's and 33's mesiodistal width",
                       text2,
                       if (predict(log4var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log4var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log4var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log4var_2,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_4_2, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots4_2_1 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_1))){
      ggplot(Datos, aes(x=Sex, y=log4var_1$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_2))){
      ggplot(Datos, aes(x=Sex, y=log4var_2$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsText4_2_2 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With the first model, considering the variables "
    text2 <- ", there is a "
    text3 <- " of probability that the estimate sex is "
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_1))){
      HTML(paste(text1,
                 "13's mesiodistal width and 43's mesiodistal width",
                 text2,
                 if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2),
                 text3,
                 if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_2))){
      HTML(paste(text1,
                 "23's mesiodistal width and 33's mesiodistal width",
                 text2,
                 if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }
  })
  
  output$resultsPlots4_2_2 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_1))){
      ggplot(Datos, aes(x=Sex, y=log2var_2$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_2))){
      ggplot(Datos, aes(x=Sex, y=log2var_3$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsText4_2_3 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "Now, the second model is considering the variables "
    text2 <- ", with this model there is a "
    text3 <- " of probability that the estimate sex is "
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_1))){
      HTML(paste(text1,
                 "13's mesiodistal width and Mandibular intercanine width",
                 text2,
                 if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2),
                 text3,
                 if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_2))){
      HTML(paste(text1,
                 "13's, 23's and 33's mesiodistal width",
                 text2,
                 if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }
  })
  
  output$resultsPlots4_2_3 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_1))){
      ggplot(Datos, aes(x=Sex, y=log2var_1$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modAgrega_2))){
      ggplot(Datos, aes(x=Sex, y=log3var_4$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  
  output$resultsText4_3_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_1))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_2))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and 43's mesiodistal width",
                       text2,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_2, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_4)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_5))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_6)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_7)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_8))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width, Mandibular intercanine width and 33's mesiodistal width",
                       text2,
                       if (predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots4_3_1 <- renderUI({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_1))){
      ggplot(Datos, aes(x=Sex, y=log2var_1$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_2))){
      ggplot(Datos, aes(x=Sex, y=log2var_2$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_4)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_5))){
      ggplot(Datos, aes(x=Sex, y=log2var_3$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_6)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_7)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_8))){
      ggplot(Datos, aes(x=Sex, y=log3var_1$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsPlots4_3_2 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_1))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_1), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_2))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_2), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_4)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_5))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_3), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_6)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_7)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_8))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_3_1), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  output$resultsText4_3_2 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With this first model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_9)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_10))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width and 43's mesiodistal width",
                       text2,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_2, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_11))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_12))){
      HTML(paste(paste(text1,
                       "33's mesiodistal width and Mandibular intercanine width",
                       text2,
                       if (predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_5,list(IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_5, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_13))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width and Maxillary intercanine width",
                       text2,
                       if (predict(log2var_6,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_6,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response")>0.5) round(predict(log2var_6,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2) else round(1-predict(log2var_6,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]]),type="response"),2)
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_6, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots4_3_3 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_9)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_10))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_2), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_11))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_3), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_12))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_5), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_13))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_6), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  output$resultsText4_3_3 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With the same accuracy and now considering the variables "
    text2 <-", with a probability of "
    text3 <- ", this second model classifies your input as "
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_9))){
      HTML(paste(text1,
                 "13's mesiodistal width and Mandibular intercanine width",
                 text2,
                 if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2),
                 text3,
                 if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_10))){
      HTML(paste(text1,
                 "13's, 33's and 43's mesiodistal width",
                 text2,
                 if (predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log3var_5,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_11))){
      HTML(paste(text1,
                 "13's, 23's and 33's mesiodistal width",
                 text2,
                 if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2),
                 text3,
                 if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_12))){
      HTML(paste(text1,
                 "Mandibular intercanine width, Maxillary intercanine width and 33's mesiodistal width",
                 text2,
                 if (predict(log3var_3,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response")>0.5) round(predict(log3var_3,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2) else round(1-predict(log3var_3,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]], IC.Max=valores[[which(medidas == "IC.Max")]]),type="response"),2),
                 text3,
                 if (predict(log3var_3,list(MD.ManL=valores[[which(medidas == "MD.ManL")]], IC.Man=valores[[which(medidas == "IC.Man")]], IC.Max=valores[[which(medidas == "IC.Max")]]))>0) "Female" else "Male"
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_13))){
      HTML(paste(text1,
                 "43's mesiodistal width and Maxillary intercanine width",
                 text2,
                 if (predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2),
                 text3,
                 if (predict(log2var_4,list(MD.ManR=valores[[which(medidas == "MD.ManR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male"
      )
      )
    }
  })
  
  output$resultsPlots4_3_3 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_9))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_1), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_10))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_3_5), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_11))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_3_4), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_12))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_3_3), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var4modSustituye_13))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_4), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  
  output$resultsText5_2_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering the variables: "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is"
    text5 <- "% accurate in making a correct prediction."
    
    HTML(paste(paste(text1,
                     "Mandibular and Maxillary intercanine width, 13's, 23's and 43's mesiodistal width",
                     text2,
                     if (predict(log5var,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male",
                     text3,
                     if (predict(log5var,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log5var,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log5var,list(IC.Max=valores[[which(medidas == "IC.Max")]], MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2)  
    ),
    paste(text4,
          round(caret::confusionMatrix(Datos$Sex_Estimado_5_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
          text5
    ),
    sep = '<br/>'
    )
    )
  })
  
  output$resultsPlots5_2_1 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log5var$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsText5_2_2 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With the first model, considering the variables "
    text2 <- ", there is a "
    text3 <- " of probability that the estimate sex is "
    
    HTML(paste(text1,
               "13's mesiodistal width and 43's mesiodistal width",
               text2,
               if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response")>0.5) round(predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2) else round(1-predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]),type="response"),2),
               text3,
               if (predict(log2var_2,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.ManR=valores[[which(medidas == "MD.ManR")]]))>0) "Female" else "Male"
    )
    )
  })
  
  output$resultsPlots5_2_2 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log2var_2$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsText5_2_3 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "Now, the second model is considering the variables "
    text2 <- ", with this model there is a "
    text3 <- " of probability that the estimate sex is "
    
    HTML(paste(text1,
               "13's mesiodistal width and Mandibular intercanine width",
               text2,
               if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]),type="response"),2),
               text3,
               if (predict(log2var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]]))>0) "Female" else "Male"
    )
    )
  })
  
  output$resultsPlots5_2_3 <- renderPlotly({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log2var_1$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  
  output$resultsText5_3_1 <- renderUI({
    medidas <- Medidas()
    valores <- Valores()
    
    text1 <- "With a logistic model, considering only the variable "
    text2 <- ", the estimate sex is "
    text3 <- " with a probability of "
    
    text4 <- "This model is "
    text5 <- "% accurate in making a correct prediction."
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_1))){
      HTML(paste(paste(text1,
                       "23's mesiodistal width and 33's mesiodistal width",
                       text2,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_2_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_2)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_4))){
      HTML(paste(paste(text1,
                       "13's mesiodistal width, Mandibular intercanine width and 33's mesiodistal width",
                       text2,
                       if (predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_1,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], IC.Man=valores[[which(medidas == "IC.Man")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_5))){
      HTML(paste(paste(text1,
                       "13's, 23's and 33's mesiodistal width",
                       text2,
                       if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]))>0) "Female" else "Male",
                       text3,
                       if (predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response")>0.5) round(predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2) else round(1-predict(log3var_4,list(MD.MaxR=valores[[which(medidas == "MD.MaxR")]], MD.MaxL=valores[[which(medidas == "MD.MaxL")]], MD.ManL=valores[[which(medidas == "MD.ManL")]]),type="response"),2)  
      ),
      paste(text4,
            round(caret::confusionMatrix(Datos$Sex_Estimado_3_4, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
            text5
      ),
      sep = '<br/>'
      )
      )
    }
  })
  
  output$resultsPlots5_3_1 <- renderUI({
    medidas <- Medidas()
    
    colorSex <- app_colors$sex
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_1))){
      ggplot(Datos, aes(x=Sex, y=log2var_3$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_2)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_4))){
      ggplot(Datos, aes(x=Sex, y=log3var_1$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_5))){
      ggplot(Datos, aes(x=Sex, y=log3var_4$fitted.values, fill=Sex))+
        geom_violin()+
        scale_fill_manual(values=colorSex)+
        theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
    }
  })
  
  output$resultsPlots5_3_2 <- renderUI({
    medidas <- Medidas()
    
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_1))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_2_3), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_2)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_3)) || all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_4))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_3_1), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }else if(all(medidas[order(unlist(medidas),decreasing=FALSE)] == sort(var5modSustituye_5))){
      ggplot()+
        geom_path(data=pr_curve(Datos, Sex, Prediction_3_4), aes(x=recall, y=precision, color="Current model")) +
        geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
        geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
        scale_color_manual(name=NULL, values=colores) + 
        theme(legend.position = c(0.2,0.5))
    }
  })
  
  output$resultsText6_1_1 <- renderUI({
    text1 <- "With a logistic model, considering your input variables, the estimate sex is "
    text2 <- " with a probability of "
    
    text3 <- "This model is"
    text4 <- "% accurate in making a correct prediction."
    
    HTML(paste(paste(text1,
                     if (predict(log6var,list(IC.Max=input$ICMax, MD.MaxR=input$MDMaxR, MD.MaxL=input$MDMaxL, IC.Man=input$ICMan, MD.ManR=input$MDManR, MD.ManL=input$MDManL))>0) "Female" else "Male",
                     text2,
                     if (predict(log6var,list(IC.Max=input$ICMax, MD.MaxR=input$MDMaxR, MD.MaxL=input$MDMaxL, IC.Man=input$ICMan, MD.ManR=input$MDManR, MD.ManL=input$MDManL),type="response")>0.5) round(predict(log6var,list(IC.Max=input$ICMax, MD.MaxR=input$MDMaxR, MD.MaxL=input$MDMaxL, IC.Man=input$ICMan, MD.ManR=input$MDManR, MD.ManL=input$MDManL),type="response"),2) else round(1-predict(log6var,list(IC.Max=input$ICMax, MD.MaxR=input$MDMaxR, MD.MaxL=input$MDMaxL, IC.Man=input$ICMan, MD.ManR=input$MDManR, MD.ManL=input$MDManL),type="response"),2)  
    ),
    paste(text3,
          round(caret::confusionMatrix(Datos$Sex_Estimado_6_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
          text4
    ),
    sep = '<br/>'
    )
    )
  })
  
  output$resultsPlots6_1_1 <- renderPlotly({
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log6var$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsPlots6_1_2 <- renderPlotly({
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    ggplot()+
      geom_path(data=pr_curve(Datos, Sex, Prediction_6_1), aes(x=recall, y=precision, color="Current model")) +
      geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
      geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
      scale_color_manual(name=NULL, values=colores) + 
      theme(legend.position = c(0.2,0.5))
  })
  
  
  
  output$resultsText6_1_2 <- renderUI({
    text1 <- "Considering the 23's and 33's mesiodistal width, with this model the estimate sex is "
    text2 <- " with a probability of "
    
    text3 <- "This model is"
    text4 <- "% accurate in making a correct prediction."
    
    HTML(paste(paste(text1,
                     if (predict(log2var_3,list(MD.MaxL=input$MDMaxL, MD.ManL=input$MDManL))>0) "Female" else "Male",
                     text2,
                     if (predict(log2var_3,list(MD.MaxL=input$MDMaxL, MD.ManL=input$MDManL),type="response")>0.5) round(predict(log2var_3,list(MD.MaxL=input$MDMaxL, MD.ManL=input$MDManL),type="response"),2) else round(1-predict(log2var_3,list(MD.MaxL=input$MDMaxL, MD.ManL=input$MDManL),type="response"),2)  
    ),
    paste(text3,
          round(caret::confusionMatrix(Datos$Sex_Estimado_2_3, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
          text4
    ),
    sep = '<br/>'
    )
    )
  })
  
  output$resultsPlots6_1_3 <- renderPlotly({
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log2var_3$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsPlots6_1_6 <- renderPlotly({
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    ggplot()+
      geom_path(data=pr_curve(Datos, Sex, Prediction_2_3), aes(x=recall, y=precision, color="Current model")) +
      geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
      geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
      scale_color_manual(name=NULL, values=colores) + 
      theme(legend.position = c(0.2,0.5))
  })
  
  
  output$resultsText6_1_3 <- renderUI({
    text1 <- "Considering the Mandibular intercanine width, 13's and 33's mesiodistal width, with this model the estimate sex is "
    text2 <- " with a probability of "
    
    text3 <- "This model is"
    text4 <- "% accurate in making a correct prediction."
    
    HTML(paste(paste(text1,
                     if (predict(log3var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan, MD.ManL=input$MDManL))>0) "Female" else "Male",
                     text2,
                     if (predict(log3var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan, MD.ManL=input$MDManL),type="response")>0.5) round(predict(log3var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan, MD.ManL=input$MDManL),type="response"),2) else round(1-predict(log3var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan, MD.ManL=input$MDManL),type="response"),2)  
    ),
    paste(text3,
          round(caret::confusionMatrix(Datos$Sex_Estimado_3_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
          text4
    ),
    sep = '<br/>'
    )
    )
  })
  
  output$resultsPlots6_1_4 <- renderPlotly({
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log3var_1$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsPlots6_1_7 <- renderPlotly({
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    ggplot()+
      geom_path(data=pr_curve(Datos, Sex, Prediction_3_1), aes(x=recall, y=precision, color="Current model")) +
      geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
      geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
      scale_color_manual(name=NULL, values=colores) + 
      theme(legend.position = c(0.2,0.5))
  })
  
  
  output$resultsText6_1_4 <- renderUI({
    text1 <- "Considering the Mandibular intercanine width and 13's mesiodistal width, with this model the estimate sex is "
    text2 <- " with a probability of "
    
    text3 <- "This model is"
    text4 <- "% accurate in making a correct prediction."
    
    HTML(paste(paste(text1,
                     if (predict(log2var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan))>0) "Female" else "Male",
                     text2,
                     if (predict(log2var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan),type="response")>0.5) round(predict(log2var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan),type="response"),2) else round(1-predict(log2var_1,list(MD.MaxR=input$MDMaxR, IC.Man=input$ICMan),type="response"),2)  
    ),
    paste(text3,
          round(caret::confusionMatrix(Datos$Sex_Estimado_2_1, Datos$Sex, positive="Female")$overall[1]*100, digits=2),
          text4
    ),
    sep = '<br/>'
    )
    )
  })
  
  output$resultsPlots6_1_5 <- renderPlotly({
    colorSex <- app_colors$sex
    
    ggplot(Datos, aes(x=Sex, y=log2var_1$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  output$resultsPlots6_1_8 <- renderPlotly({
    colores <- c("Current model"="gray19", "No skill"="blue", "Perfect model"="green3")
    
    ggplot()+
      geom_path(data=pr_curve(Datos, Sex, Prediction_2_1), aes(x=recall, y=precision, color="Current model")) +
      geom_line(aes(x=seq(0,1.02,by=0.01888889), y=rep(0,54), color="No skill"), linetype="dashed") + 
      geom_step(aes(x=seq(0,1.02,by=0.01888889), y=c(rep(1,53),0), color="Perfect model"), linetype="dashed") +
      scale_color_manual(name=NULL, values=colores) + 
      theme(legend.position = c(0.2,0.5))
  })
  
}
