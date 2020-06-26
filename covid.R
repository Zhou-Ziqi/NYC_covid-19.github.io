library(sp)
library(tmap)  
library(tidyverse)
library(readxl)
library(viridis)
library(leaflet)
library(tigris)
library(plotly)

library(shiny)

#get data
spdf = rgdal::readOGR("/Users/ziqizhou/Desktop/MSPH_1st_Year/P8105_Data_Science/Git/zipcode_covid-19/data/Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data

data0613 = read.csv("./data/June/0613/data-by-modzcta0613.csv") %>% 
  drop_na()%>% janitor::clean_names() %>% 
  rename(positive = covid_case_count,
         MODZCTA = modified_zcta) %>% 
  mutate(day = "June13") %>% 
  dplyr::select(MODZCTA,positive,neighborhood_name, borough_group,day)

data0614 = read.csv("./data/June/0614/data-by-modzcta0614.csv") %>% 
  drop_na()%>% janitor::clean_names() %>% 
  rename(positive = covid_case_count,
         MODZCTA = modified_zcta) %>% 
  mutate(day = "June14") %>% 
  dplyr::select(MODZCTA,positive,neighborhood_name, borough_group,day)

jun13_jun14 = full_join(data0613,data0614)

#input the widges
date = jun13_jun14 %>% distinct(day) %>% pull()

posi_map = geo_join(spdf,jun13_jun14,"MODZCTA","MODZCTA")

# Setting up the pop up text
popup_sb <- paste0("Positive Cases: ", as.character(posi_map$positive))

pal <- colorNumeric("Greens", domain=posi_map$positive)

# selectInput widget
selectInput(
  "date_choice", 
  label = h3("Select date"),
  choices = date)



##########
ui =  fluidPage(
  titlePanel("Cumulative Positive Cases in NYC"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of Cumulative Positive cases in NYC."),
      
      
      selectInput(
        "date_choice", 
        label = h3("Select date"),
        choices = date),
      
      width = 3
      
    ),
    
    mainPanel(
      leafletOutput("map")
    )
  )
)




server = function(input, output) {
  
  output$map = renderLeaflet({ 
    
    jun13_jun14 %>%
      filter(day == input$date_choice) %>% 
      leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -73.99653, lat = 40.75074, zoom = 12) %>% 
      addPolygons(data = posi_map , 
                  fillColor = ~pal(posi_map$positive), 
                  fillOpacity = 0.7, 
                  weight = 0.2, 
                  smoothFactor = 0.2, 
                  popup = ~popup_sb) %>%
      addLegend(pal = pal, 
                values = posi_map$positive, 
                position = "bottomright", 
                title = "Cumulative cases")
      
    
    
    
  })
  
}


shinyApp(ui, server)
