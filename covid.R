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
spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data

data0613 = read.csv("./data/June/0613/data-by-modzcta0613.csv") %>% 
  drop_na()%>% janitor::clean_names() %>% 
  rename(positive = covid_case_count,
         MODZCTA = modified_zcta) %>% 
  mutate(day = "June13") %>% 
  dplyr::select(MODZCTA,positive,neighborhood_name, borough_group,day,covid_death_count)

data0614 = read.csv("./data/June/0614/data-by-modzcta0614.csv") %>% 
  drop_na()%>% janitor::clean_names() %>% 
  rename(positive = covid_case_count,
         MODZCTA = modified_zcta) %>% 
  mutate(day = "June14") %>% 
  dplyr::select(MODZCTA,positive,neighborhood_name, borough_group,day,covid_death_count)

jun13_jun14 = full_join(data0613,data0614)

#input the widges
date = jun13_jun14 %>% distinct(day) %>% pull()

Map_data = geo_join(spdf,jun13_jun14,"MODZCTA","MODZCTA")

# Setting up the pop up text
popup_sb <- paste0("Positive Cases: ", as.character(Map_data$positive))

pal <- colorNumeric("Greens", domain=Map_data$positive)


popup_dea_sb <- paste0("Death Count: ", as.character(Map_data$covid_death_count),
                       "<br>", 
                       "Neighborhood Name", as.character(Map_data$neighborhood_name))

pal_dea <- colorNumeric("Reds", domain=Map_data$covid_death_count)


##########
ui =  fluidPage(
  titlePanel("COVID-19 in NYC"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of Cumulative Positive cases in NYC."),
      
      selectInput(
        "date_choice", 
        label = h3("Select date"),
        choices = date),
      
      
      
      checkboxGroupInput(inputId = "EventFinder",
                         label = "Select Event:",
                         choices = c("Death" = "D", "Positive Case" = "P"),
                         selected = "P"),
      
      width = 3
      ),
    
    
    
    mainPanel(
      fluidRow(
        
        splitLayout(cellWidths = c("50%","50%"),
                    leafletOutput(outputId = "map1"),
                    leafletOutput(outputId = "map2"))

      ),
      width = 10
    )
  )
)




server = function(input, output) {
  
    output$map1 = renderLeaflet({
      Map_data %>%
      filter(day == input$date_choice)%>% 
        select()
       leaflet() %>%
       addProviderTiles("CartoDB.Positron") %>%
       setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
       addPolygons(data = Map_data , 
                fillColor = ~pal(Map_data$positive), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
       addLegend(pal = pal, 
              values = Map_data$positive, 
              position = "bottomright", 
              title = "Number")
  
  
})

}



shinyApp(ui, server)
