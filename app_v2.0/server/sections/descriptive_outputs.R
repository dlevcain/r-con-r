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
      group = c("Male", "Female"),
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
    dataS <- dataS %>%
      mutate(Sex = recode(Sexo, "Masculino" = "Male", "Femenino" = "Female"))
    
    p1 <- plot_ly(dataS, x = ~Sex, y = ~Edad, color = ~Sex, type = "box") 
    p1 %>% layout( xaxis = list(title = 'Sex'), yaxis = list(title = 'Age'))
  })
  
  output$plots_ori_sexo <- renderPlotly({
    dataS = datag
    dataS <- dataS %>%
      mutate(Sex = recode(Sexo, "Masculino" = "Male", "Femenino" = "Female"))
    
    fig <- plot_ly(data = dataS, x = dataS[,c(4)], y = dataS[,c(10)], color = ~Sex)
    fig %>% layout( xaxis = list(title = 'Distance - Without treatment (mm) '), yaxis = list(title = 'Distance - With treatment (mm)'))
  }) 
  
  output$plots_evaluacion <- renderPlotly({
    dataS = datag
    dataS <- dataS %>%
      mutate(Sex = recode(Sexo, "Masculino" = "Male", "Femenino" = "Female"))
    
    fig <- plot_ly(data = dataS, x = dataS[,c(7)], y = dataS[,c(13)], color = ~Sex)
    fig %>% layout( xaxis = list(title = 'Distance - Without treatment (mm)'), yaxis = list(title = 'Distance - With treatment (mm)'))
  })
  
  
  # D E S C R I P T I O N    O F    D I S T A C E S ------------------------------------------------------------------------------------------------
  distance_x_axis_title <- reactive({
    zone_raw <- if (!is.null(input$zona)) input$zona else input$Zona
    treatment_raw <- if (!is.null(input$tratamiento)) input$tratamiento else input$Tratamiento

    zone_label <- dplyr::recode(
      zone_raw,
      "Maxilar" = "Maxillary",
      "Mandibular" = "Mandibular",
      .default = zone_raw
    )

    treatment_label <- dplyr::recode(
      treatment_raw,
      "Sin tratamiento" = "Without treatment",
      "Con tratamiento" = "With treatment",
      .default = treatment_raw
    )

    if (!is.null(treatment_label) && nzchar(treatment_label)) {
      paste(zone_label, "-", treatment_label)
    } else {
      zone_label
    }
  })

  output$plots_IC<- renderPlotly({
    dataS =filtro()
    
    p1 <- plot_ly(dataS, x=dataS[,3], y = dataS[,4], type = "box") 
    p1 %>% layout(xaxis = list(title = distance_x_axis_title()), yaxis = list(title = 'Distance (mm)'))
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
    p1 %>% layout(xaxis = list(title = distance_x_axis_title()), yaxis = list(title = 'Mesiodistal Right (mm)'))
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
    p1 %>% layout(xaxis = list(title = distance_x_axis_title()), yaxis = list(title = 'Mesiodistal Left (mm)'))
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
  
  
