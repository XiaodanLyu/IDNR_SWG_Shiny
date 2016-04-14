# require package shiny, DT, readxl, reshape2 and ggplot2 
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

# The index list for all species
index <<- read.csv("./data/Index.csv", header = T, as.is = T)

# Model Coefficient estimates
model <- readxl::read_excel("./data/Models.xlsx", sheet = "ModelEst")
model <<- transform(model, o.s = (o.lcl*o.ucl > 0), g.s = (g.lcl*g.ucl) > 0,
                    e.s = (e.lcl*e.ucl > 0), p.s = (p.lcl*p.ucl > 0))
# Model Real Parameter estimates
para <<- readxl::read_excel("./data/Models.xlsx", sheet = "Real_Parameter")

# Map height and width
height <<- 415
width <<- 600

# Map components
est <<- read.csv("./data/Predicted_values.csv", header = T)
#Achoices <<- levels(est$LAND)
pp <<- read.csv("./data/Public_Lands.csv")