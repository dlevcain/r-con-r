# ============================================================
# globals.R
# Global configuration, dependencies, and domain constants
# ============================================================

# --- Libraries ----------------------------------------------

# Core Shiny
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)

# UI / UX
library(shinyjs)
library(shinycssloaders)
library(shinyBS)

# Display / Visualization
library(plotly)
library(DT)
library(pheatmap)

# Data & ML
library(tidyverse)   # includes ggplot2, dplyr, scales, etc.
library(cluster)
library(factoextra)
library(NbClust)
library(caret)
library(yardstick)

# Colors / Palettes 
library(RColorBrewer)
library(wesanderson)

# Deploy
library(rsconnect)
library(Rmisc)




# --- Domain constants ---------------------------------------

Choices <- list(
  " " = "0",
  "Maxillary intercanine width" = "IC.Max",
  "Mandibular intercanine width" = "IC.Man",
  "13's mesiodistal width" = "MD.MaxR",
  "23's mesiodistal width" = "MD.MaxL",
  "33's mesiodistal width" = "MD.ManL",
  "43's mesiodistal width" = "MD.ManR"
)
