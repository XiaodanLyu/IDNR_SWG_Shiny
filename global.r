## ====================================================
## require package shiny, readxl, DT, ggplot2 and dplyr
## ====================================================
if (!require("shiny")) install.packages("shiny")
if (!require("readxl")) install.packages("readxl")
if (!require("devtools")) install.packages("devtools")
if (!require("DT")) devtools::install_github('rstudio/DT')
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
## ======================================================

# The index list for all species
index <<- read.csv("./data/Index.csv", header = T, as.is = T)

# Coordinates, Landform Regions, County, Wildlife Management Units and Predicted values 
est <<- read.csv("./data/Predicted_values.csv", header = T)
# Public Lands vertices
pp <<- read.csv("./data/Public_Lands.csv")

# Model Coefficient estimates
model <- readxl::read_excel("./data/Models.xlsx", sheet = "ModelEst")
# get model coefficients significance indicator
model <<- model %>% mutate(o.s = (o.lcl*o.ucl > 0), g.s = (g.lcl*g.ucl) > 0,
                           e.s = (e.lcl*e.ucl > 0), p.s = (p.lcl*p.ucl > 0))
# Model Real Parameter estimates
para <<- readxl::read_excel("./data/Models.xlsx", sheet = "Real_Parameter")

# Map height and width
height <<- 415
width <<- 600
