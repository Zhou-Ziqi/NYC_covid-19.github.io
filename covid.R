##load packages
library(sp)
library(tmap)  
library(tidyverse)
library(readxl)
library(viridis)
library(leaflet)
library(tigris)
library(plotly)
library(shinyWidgets)
library(shiny)

##get data
spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data
#June data
Junedata = read.csv("./data/Junedata.csv")
#input the widges
plot_date = Junedata %>% distinct(day) %>% pull()
outcome_selection = c("positive","covid_death_count","covid_case_rate","covid_death_rate","newcases_june")




  Map_data = geo_join(spdf,plot_df,"MODZCTA","zipcode")
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(Map_data$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(Map_data$MODZCTA),
                     "<br>", 
                     "Total Number of ",outcome," Cases", as.character(Map_data$outcome)
  )
  
  # Setting up the pop up text
  pal <- colorNumeric("Reds", domain=Map_data$outcome)
  
  
  g1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data = Map_data , 
                fillColor = ~pal(Map_data$outcome), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = Map_data$outcome, 
              position = "bottomright", 
              title = "Number")
  g1



##########
ui =  fluidPage(
  titlePanel("COVID-19 in NYC"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of Cumulative Positive cases in NYC."),
      
      selectInput("date_choice", 
                  label = h3("Select date"),
                  choices = plot_date),
      
      pickerInput(inputId = "outcome",label =  "Outcome:",   
                  choices = c("Cumulative Cases Count", "Death Count", "Positive Cases Rate", "Death Rate","New cases"), 
                  selected = c("Cumulative Cases Count"),
                  multiple = FALSE)
    ),
    
    
    mainPanel(
      fluidRow(leafletOutput(outputId = "cumulative_plot")

      
    )
  )))




server = function(input, output,session) {
  
  
  output$map1 = renderLeaflet({
    
    
  })
}



shinyApp(ui, server)
