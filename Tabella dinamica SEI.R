# Caricamento librerie ----
library(shiny)
library(leaflet)
library(DT)
library(ggplot2)
library(dplyr)
library(readxl)
library(rpart)
library(rpart.plot)
library(ResourceSelection)
library(shinythemes)
library(sp)
library(terra)
library(sf)
library(plotly)
library(raster)
library(tidyverse)
library(stars)
library(ggspatial)

# Caricamento dati ----
dati_csv <- read.csv("Data/Dati.csv", sep = ";", header = TRUE, na.strings = "", dec = ",")

# Colori ----
cols <- c('#ff0000', '#ffa500', '#72d65d', '#00a2d2')


