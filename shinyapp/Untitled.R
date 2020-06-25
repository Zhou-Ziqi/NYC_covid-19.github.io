library(flexdashboard)
library(shiny)
library(plotly)
library(tidyverse)
library(readxl)
library(viridis)
library(leaflet)
library(maps)
library(sp)
library(maptools)
library(broom)
library(httr)
library(rgdal)

library(sf)
library(raster)

library(spData)
library(tmap)  


#get data
spdf = rgdal::readOGR("/Users/ziqizhou/Desktop/MSPH_1st_Year/P8105_Data_Science/Git/zipcode_covid-19/data/Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data








shinyApp(ui, server)