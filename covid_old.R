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

Junedata = read.csv("./data/Junedata.csv")
#input the widges
date = Junedata %>% distinct(day) %>% pull()





##########
ui =  fluidPage(
  titlePanel("COVID-19 in NYC"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of Cumulative Positive cases in NYC."),
      
      selectInput(
        "date_choice", 
        label = h3("Select date"),
        choices = date)),
      
    
    
    
    mainPanel(
      fluidRow(
        
        splitLayout(cellWidths = c("20%","20%","20%","20%","20%"),
                    leafletOutput(outputId = "map1"),
                    leafletOutput(outputId = "map2"),
                    leafletOutput(outputId = "map3"),
                    leafletOutput(outputId = "map4"),
                    leafletOutput(outputId = "map5"))

      ),
      width = 10
    )
  
))



data_to_plot = Junedata %>% filter(day == "June28")

server = function(input, output) {
  
    output$map1 = renderLeaflet({
      data_to_plot = Junedata %>% filter(day == input$date_choice)
      
      data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=Junedata$positive)
      
      popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                         "<br>", 
                         "MODZCTA: ", as.character(Junedata$zipcode),
                         "<br>", 
                         "Total Number of Cumulative Positive Cases: ", as.character(Junedata$positive)
      )
      
       leaflet() %>%
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
  
})
    
    output$map2 = renderLeaflet({
      data_to_plot = Junedata %>% filter(day == input$date_choice)
      
      data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      pal_dea <- colorNumeric("Reds", domain=Junedata$covid_death_count)
      
      popup_dea_sb = paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                            "<br>", 
                            "MODZCTA: ", as.character(Junedata$zipcode),
                            "<br>", 
                            "Total Number of Death Count Cases: ", as.character(Junedata$covid_death_count)
      )
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data = data_to_plot_geo , 
                    fillColor = ~pal_dea(data_to_plot_geo$covid_death_count), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_dea_sb) %>%
        addLegend(pal = pal_dea, 
                  values = data_to_plot_geo$covid_death_count, 
                  position = "bottomright", 
                  title = "Number")
      
    })
    
    output$map3 = renderLeaflet({
      data_to_plot = Junedata %>% filter(day == input$date_choice)
      
      data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      popup_dea_rate_sb =  paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                                  "<br>", 
                                  "MODZCTA: ", as.character(Junedata$zipcode),
                                  "<br>", 
                                  "Death Rate of COVID-19: ", as.character(Junedata$covid_death_rate)
      )
      
      pal_dea_rate <- colorNumeric("Reds", domain=Junedata$covid_death_rate)
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data = data_to_plot_geo , 
                    fillColor = ~pal_dea_rate(data_to_plot_geo$covid_death_rate), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_dea_rate_sb) %>%
        addLegend(pal = pal_dea_rate, 
                  values = data_to_plot_geo$covid_death_rate, 
                  position = "bottomright", 
                  title = "Number")
      
    })
    
    output$map4 = renderLeaflet({
      data_to_plot = Junedata %>% filter(day == input$date_choice)
      
      data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      popup_case_rate_sb =  paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                                  "<br>", 
                                  "MODZCTA: ", as.character(Junedata$zipcode),
                                  "<br>", 
                                  "Case Rate of COVID-19: ", as.character(Junedata$covid_case_rate)
      )
      pal_case_rate <- colorNumeric("Greens", domain=Junedata$covid_case_rate)
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data = data_to_plot_geo , 
                    fillColor = ~pal_case_rate(data_to_plot_geo$covid_case_rate), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_case_rate_sb) %>%
        addLegend(pal = pal_case_rate, 
                  values = data_to_plot_geo$covid_case_rate, 
                  position = "bottomright", 
                  title = "Number")
      
    })
    
    output$map5 = renderLeaflet({
      data_to_plot = Junedata %>% filter(day == input$date_choice)
      
      data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      popup_new_sb =  paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                                   "<br>", 
                                   "MODZCTA: ", as.character(Junedata$zipcode),
                                   "<br>", 
                                   "Case Rate of COVID-19: ", as.character(Junedata$newcases_june)
      )
      pal_new_rate <- colorNumeric("Greens", domain=Junedata$newcases_june)
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data = data_to_plot_geo , 
                    fillColor = ~pal_new_rate(data_to_plot_geo$newcases_june), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_new_sb) %>%
        addLegend(pal = pal_new_rate, 
                  values = data_to_plot_geo$newcases_june, 
                  position = "bottomright", 
                  title = "Number")
      
    })
}



shinyApp(ui, server)
