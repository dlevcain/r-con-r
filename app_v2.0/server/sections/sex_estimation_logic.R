  # S E X   E S T I M A T I O N ------------------------------------------------------------------------------------------------  
  var2mod_1 <- list("MD.MaxR", "IC.Man")
  var2mod_2 <- list("MD.MaxR", "MD.ManR")
  var2mod_3 <- list("MD.MaxL", "MD.ManL")
  var2mod_4 <- list("IC.Man", "MD.ManR")
  var2mod_5 <- list("IC.Man", "MD.ManL")
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
  #-------------------------------------------------------------------------------
  var3mod_1 <- list("MD.MaxR", "IC.Man", "MD.ManL")
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
  #-------------------------------------------------------------------------------
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
  #-------------------------------------------------------------------------------
  var5modAgrega_1 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManR")
  var5modSustituye_1 <- c("IC.Max", "MD.MaxL", "IC.Man", "MD.ManR", "MD.ManL")
  var5modSustituye_2 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManL")
  var5modSustituye_3 <- c("IC.Max", "MD.MaxR", "IC.Man", "MD.ManR", "MD.ManL")
  var5modSustituye_4 <- c("MD.MaxR", "MD.MaxL", "IC.Man", "MD.ManR", "MD.ManL")
  var5modSustituye_5 <- c("IC.Max", "MD.MaxR", "MD.MaxL", "MD.ManL", "MD.ManR")
  
  
  # Utilidades para validación de combinaciones de medidas
  matches_model_group <- function(selected_measures, model_group) {
    model_values <- as.character(unlist(model_group, use.names = FALSE))
    all(selected_measures %in% model_values)
  }

  matches_any_model <- function(selected_measures, model_groups) {
    any(vapply(model_groups, function(group) {
      matches_model_group(selected_measures, group)
    }, logical(1)))
  }

  get_measure_inputs <- function(input, num_medidas) {
    vapply(seq_len(num_medidas), function(i) {
      input[[paste0("medida", num_medidas, "_", i)]]
    }, character(1), USE.NAMES = FALSE)
  }

  get_value_inputs <- function(input, num_medidas) {
    lapply(seq_len(num_medidas), function(i) {
      input[[paste0("val", num_medidas, "Med", i)]]
    })
  }

  var2mod <- list(var2mod_1, var2mod_2, var2mod_3, var2mod_4, var2mod_5)
  var2modAgrega <- list(var2modAgrega_1, var2modAgrega_2, var2modAgrega_3)
  var2modSustituye <- list(
    var2modSustituye_1, var2modSustituye_2, var2modSustituye_3, var2modSustituye_4,
    var2modSustituye_5, var2modSustituye_6, var2modSustituye_7
  )

  var3mod <- list(var3mod_1)
  var3modAgrega <- list(var3modAgrega_1, var3modAgrega_2, var3modAgrega_3, var3modAgrega_4)
  var3modSustituye <- list(
    var3modSustituye_1, var3modSustituye_2, var3modSustituye_3, var3modSustituye_4, var3modSustituye_5,
    var3modSustituye_6, var3modSustituye_7, var3modSustituye_8, var3modSustituye_9, var3modSustituye_10,
    var3modSustituye_11, var3modSustituye_12, var3modSustituye_13, var3modSustituye_14, var3modSustituye_15
  )

  var4modAgrega <- list(var4modAgrega_1, var4modAgrega_2)
  var4modSustituye <- list(
    var4modSustituye_1, var4modSustituye_2, var4modSustituye_3, var4modSustituye_4, var4modSustituye_5,
    var4modSustituye_6, var4modSustituye_7, var4modSustituye_8, var4modSustituye_9, var4modSustituye_10,
    var4modSustituye_11, var4modSustituye_12, var4modSustituye_13
  )

  var5modAgrega <- list(var5modAgrega_1)
  var5modSustituye <- list(var5modSustituye_1, var5modSustituye_2, var5modSustituye_3, var5modSustituye_4, var5modSustituye_5)

  #//////////////////////////////////////// variable: 2 //////////////////////////////////////// 
  output$modelosVar2 <- reactive({
    matches_any_model(get_measure_inputs(input, 2), var2mod)
  })
  outputOptions(output, "modelosVar2", suspendWhenHidden = FALSE)

  output$modelosAgVar2 <- reactive({
    matches_any_model(get_measure_inputs(input, 2), var2modAgrega)
  })
  outputOptions(output, "modelosAgVar2", suspendWhenHidden = FALSE)

  output$modelosSusVar2 <- reactive({
    matches_any_model(get_measure_inputs(input, 2), var2modSustituye)
  })
  outputOptions(output, "modelosSusVar2", suspendWhenHidden = FALSE)

  #//////////////////////////////////////// variable: 3 ////////////////////////////////////////
  output$modelosVar3 <- reactive({
    matches_any_model(get_measure_inputs(input, 3), var3mod)
  })
  outputOptions(output, "modelosVar3", suspendWhenHidden = FALSE)

  output$modelosAgVar3 <- reactive({
    matches_any_model(get_measure_inputs(input, 3), var3modAgrega)
  })
  outputOptions(output, "modelosAgVar3", suspendWhenHidden = FALSE)

  output$modelosSusVar3_1 <- reactive({
    matches_any_model(get_measure_inputs(input, 3), var3modSustituye[1:9])
  })
  outputOptions(output, "modelosSusVar3_1", suspendWhenHidden = FALSE)

  output$modelosSusVar3_2 <- reactive({
    matches_any_model(get_measure_inputs(input, 3), var3modSustituye[10:15])
  })
  outputOptions(output, "modelosSusVar3_2", suspendWhenHidden = FALSE)

  #//////////////////////////////////////// variable: 4 ////////////////////////////////////////
  output$modelosAgVar4 <- reactive({
    matches_any_model(get_measure_inputs(input, 4), var4modAgrega)
  })
  outputOptions(output, "modelosAgVar4", suspendWhenHidden = FALSE)

  output$modelosSusVar4_1 <- reactive({
    matches_any_model(get_measure_inputs(input, 4), var4modSustituye[1:8])
  })
  outputOptions(output, "modelosSusVar4_1", suspendWhenHidden = FALSE)

  output$modelosSusVar4_2 <- reactive({
    matches_any_model(get_measure_inputs(input, 4), var4modSustituye[9:13])
  })
  outputOptions(output, "modelosSusVar4_2", suspendWhenHidden = FALSE)

  #//////////////////////////////////////// variable: 5 ////////////////////////////////////////
  output$modelosAgVar5 <- reactive({
    matches_any_model(get_measure_inputs(input, 5), var5modAgrega)
  })
  outputOptions(output, "modelosAgVar5", suspendWhenHidden = FALSE)

  output$modelosSusVar5 <- reactive({
    matches_any_model(get_measure_inputs(input, 5), var5modSustituye)
  })
  outputOptions(output, "modelosSusVar5", suspendWhenHidden = FALSE)

  Medidas <- reactive({
    req(input$numMed)
    selected_measures <- get_measure_inputs(input, input$numMed)
    setNames(as.list(selected_measures), paste0("med", seq_along(selected_measures)))
  })

  Valores <- reactive({
    req(input$numMed)
    selected_values <- get_value_inputs(input, input$numMed)
    setNames(selected_values, paste0("val", seq_along(selected_values)))
  })

  # M O D E L O S ------------------------------------------------------------------------------------------------
  url <- "https://docs.google.com/spreadsheets/d/1vOGE2_5Br9qpQYt4sw8rkxHWuvq_Bb66u__UgQHwGrI/export?format=csv&gid=739303745"
  Datos <- read.csv(url)
  
  Datos$SexNum <- Datos$Sex
  Datos$Sex <- as.factor(Datos$Sex)
  
  ##############################################################################################
  ######################################## M O D E L O #########################################
  ##############################################################################################
  #//////////////////////////////////////// variable: 1 ////////////////////////////////////////
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
  #//////////////////////////////////////// variable: 2 ////////////////////////////////////////
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
  #//////////////////////////////////////// variable: 3 ////////////////////////////////////////
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
  #//////////////////////////////////////// variable: 4 ////////////////////////////////////////
  #---------------------------- MD.MaxR + MD.MaxL + IC.Man + MD.ManR ---------------------------
  log4var_1 = glm(Sex ~ MD.MaxR + MD.MaxL + IC.Man + MD.ManR, data = Datos, family = binomial)
  #---------------------------- IC.Max + MD.MaxR + MD.MaxL + MD.ManL ---------------------------
  log4var_2 = glm(Sex ~ IC.Max + MD.MaxR + MD.MaxL + MD.ManL, data = Datos, family = binomial)
  #//////////////////////////////////////// variable: 5 ////////////////////////////////////////
  #----------------------- IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR -----------------------
  log5var = glm(Sex ~ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR, data = Datos, family = binomial)
  #//////////////////////////////////////// variable: 6 ////////////////////////////////////////
  #------------------ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR + MD.ManL ------------------
  log6var = glm(Sex ~ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR + MD.ManL, data = Datos, family = binomial)
  
  
  ##############################################################################################
  ################################# C L A S I F I C A C I O N ##################################
  ##############################################################################################
  #//////////////////////////////////////// variable: 1 ////////////////////////////////////////
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
  #//////////////////////////////////////// variable: 2 ////////////////////////////////////////
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
  #//////////////////////////////////////// variable: 3 ////////////////////////////////////////
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
  #//////////////////////////////////////// variable: 4 ////////////////////////////////////////
  #---------------------------- MD.MaxR + MD.MaxL + IC.Man + MD.ManR ---------------------------
  Datos$Sex_Estimado_4_1 <- as.numeric(log4var_1$fitted.values>=0.5)
  Datos$Sex_Estimado_4_1 <-factor(Datos$Sex_Estimado_4_1, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_4_1) <- c("Male", "Female")
  #---------------------------- IC.Max + MD.MaxR + MD.MaxL + MD.ManL ---------------------------
  Datos$Sex_Estimado_4_2 <- as.numeric(log4var_2$fitted.values>=0.5)
  Datos$Sex_Estimado_4_2 <-factor(Datos$Sex_Estimado_4_2, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_4_2) <- c("Male", "Female")
  #//////////////////////////////////////// variable: 5 ////////////////////////////////////////
  #----------------------- IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR -----------------------
  Datos$Sex_Estimado_5_1 <- as.numeric(log5var$fitted.values>=0.5)
  Datos$Sex_Estimado_5_1 <-factor(Datos$Sex_Estimado_5_1, labels=levels(Datos$Sex))
  levels(Datos$Sex_Estimado_5_1) <- c("Male", "Female")
  #//////////////////////////////////////// variable: 6 ////////////////////////////////////////
  #------------------ IC.Max + MD.MaxR + MD.MaxL + IC.Man + MD.ManR + MD.ManL ------------------
  Datos$Sex_Estimado_6_1 <- as.numeric(log6var$fitted.values>=0.5)
  Datos$Sex_Estimado_6_1 <-factor(Datos$Sex_Estimado_6_1, labels=levels(Datos$Sex))
  Datos$Prediction_6_1 <- 1-log6var$fitted.values
  levels(Datos$Sex_Estimado_6_1) <- c("Male", "Female")
  
  levels(Datos$Sex) <- c("Male", "Female")
  
  
  # R E S U L T S ------------------------------------------------------------------------------------------------
  observeEvent(input$resultado, {
    updateTabsetPanel(session, "inTabset",
                      selected = "Resultados")
  })
  
  #//////////////////////////////////////// variable: 1 ////////////////////////////////////////
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
    colorBxF <- c("#B2DFEE","#FFB6C1")
    colorBxC <- c("#68838B", "#8B5F65")
    colorManual <- c("green3","red3")
    
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
    colorSex <- c("#9AC0CD","#FFAEB9")
    colorManual <- c("green3","red3")
    
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
  
  #//////////////////////////////////////// variable: 2 ////////////////////////////////////////
  
  #------------------------------ M O D E L O S      B U E N O S -------------------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
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
  
  
  #---------------------- M O D E L O S      C O N      A G R E G A D O S ----------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    colorManual <- c("green3","red3")
    
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
  
  
  #------------------------- M O D E L O S      S U S T I T U I D O S --------------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    colorManual <- c("green3","red3")
    
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
  
  
  #//////////////////////////////////////// variable: 3 ////////////////////////////////////////
  
  #-------------------------------- M O D E L O      B U E N O ---------------------------------
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
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  #---------------------- M O D E L O S      C O N      A G R E G A D O S ----------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  #------------------------- M O D E L O S      S U S T I T U I D O S --------------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  
  #//////////////////////////////////////// variable: 4 ////////////////////////////////////////
  #---------------------- M O D E L O S      C O N      A G R E G A D O S ----------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  
  #------------------------- M O D E L O S      S U S T I T U I D O S --------------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  
  #//////////////////////////////////////// variable: 5 ////////////////////////////////////////
  #----------------------- M O D E L O      C O N      A G R E G A D O S -----------------------
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
    ggplot(Datos, aes(x=Sex, y=log2var_1$fitted.values, fill=Sex))+
      geom_violin()+
      scale_fill_manual(values=colorSex)+
      theme(legend.position = "none")+ geom_jitter(height = 0, width = 0.1)
  })
  
  
  #------------------------- M O D E L O S      S U S T I T U I D O S --------------------------  
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
    
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  #//////////////////////////////////////// variable: 6 ////////////////////////////////////////
  #---------------------------------------- M O D E L O ----------------------------------------
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
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
  
  
  #---------------------------- M O D E L O S      A G R E G A D O S ---------------------------
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
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
    colorSex <- c("#9AC0CD","#FFAEB9")
    
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
  
