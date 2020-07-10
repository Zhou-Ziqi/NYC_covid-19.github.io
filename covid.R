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

#get data
spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data

Junedata = read.csv("./data/Junedata.csv")
#input the widges
date = Junedata %>% distinct(day) %>% pull()


choices = c("Cumulative Cases Count", "Death Count", "Positive Cases Rate", "Death Rate","New cases")


### write the functions to draw the map
###positive
positive = function(date){
  data_to_plot = Junedata %>% filter(day == date)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
  
  pal <- colorNumeric("Greens", domain=Junedata$positive)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(Junedata$zipcode),
                     "<br>", 
                     "Total Number of Cumulative Positive Cases: ", as.character(Junedata$positive)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$positive), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$positive, 
              position = "bottomright", 
              title = "Number")
  p1
}
### case rate

case_rate = function(date){ 
  
  data_to_plot = Junedata %>% filter(day == date)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
  
  pal <- colorNumeric("Greens", domain=Junedata$covid_case_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(Junedata$zipcode),
                     "<br>", 
                     "Case Rate of COVID-19: ", as.character(Junedata$covid_case_rate)
  )
  
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_case_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$covid_case_rate, 
              position = "bottomright", 
              title = "Number")
  
}




##death count

death_count = function(date){
  
  data_to_plot = Junedata %>% filter(day == date)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
  
  pal <- colorNumeric("Reds", domain=Junedata$covid_death_count)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(Junedata$zipcode),
                     "<br>", 
                     "Total Number of Death: ", as.character(Junedata$covid_death_count)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_death_count), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = data_to_plot_geo$covid_death_count, 
              position = "bottomright", 
              title = "Number")
  p1
}

##death rate
death_rate = function(date){
  data_to_plot = Junedata %>% filter(day == date)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
  
  pal <- colorNumeric("Reds", domain=Junedata$covid_death_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(Junedata$zipcode),
                     "<br>", 
                     "Death Rate: ", as.character(Junedata$covid_death_rate)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_death_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$covid_death_rate, 
              position = "bottomright", 
              title = "Number")
  p1
}


###new cases
newcase = function(date){
  
  data_to_plot = Junedata %>% filter(day == date)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
  
  pal <- colorNumeric("Greens", domain=Junedata$newcases_june)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(Junedata$zipcode),
                     "<br>", 
                     "Total Number of New Cases: ", as.character(Junedata$newcases_june)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$newcases_june), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$newcases_june, 
              position = "bottomright", 
              title = "Number")
  p1
}


##########

### ui
ui =  fluidPage(
  titlePanel("COVID-19 in NYC"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of Cumulative Positive cases in NYC."),
      
      selectInput(
        "date_choice", 
        label = h3("Select date"),
        choices = date),
      
      radioButtons(inputId = "outcome_selection",
                   label =  "Outcome:",   
                   c("Cumulative Cases Count" = "positive", 
                     "Death Count" = "death_count", 
                     "Positive Cases Rate" = "case_count", 
                     "Death Rate" = "death_rate",
                     "New cases" = "newcase")
                   
      )),
    
    
    mainPanel(leafletOutput(outputId = "map",width="100%",height="600px"))
    
  ))


###### server

server = function(input, output) {
  
  
  
  
  output$map = renderLeaflet({
    
    plot = switch (input$outcome_selection,
                   positive = positive,
                   death_count = death_count,
                   case_count = case_count,
                   death_rate = death_rate,
                   newcase = newcase
    )
    
    plot(input$date_choice)
  })
  
}







shinyApp(ui, server)
