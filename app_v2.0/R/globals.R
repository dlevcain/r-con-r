

library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)

library(shinyjs)
library(shinycssloaders)
library(shinyBS)

library(plotly)
library(DT)
library(pheatmap)

library(tidyverse)   # includes ggplot2, dplyr, scales, etc.
library(cluster)
library(factoextra)
library(NbClust)
library(caret)
library(yardstick)

library(RColorBrewer)
library(wesanderson)

library(rsconnect)
library(Rmisc)





Choices <- list(
  " " = "0",
  "Maxillary intercanine width" = "IC.Max",
  "Mandibular intercanine width" = "IC.Man",
  "13's mesiodistal width" = "MD.MaxR",
  "23's mesiodistal width" = "MD.MaxL",
  "33's mesiodistal width" = "MD.ManL",
  "43's mesiodistal width" = "MD.ManR"
)
