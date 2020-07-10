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


Junedata = read.csv("./data/Junedata.csv")
#input the widges
date = Junedata %>% distinct(day) %>% pull()





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
                  choices = c("Cumulative Cases Count", "Death Count", "Positive Cases Rate", "Death Rate","New cases"), 
                  selected = c("Cumulative Cases Count")
                 
    )),
    
    
    mainPanel(leafletOutput(outputId = "map",width="100%",height="600px"))
    
  ))


###### server

server = function(input, output) {


    
  
  output$map = renderLeaflet({
    data_to_plot = Junedata %>% filter(day == input$date_choice)
    data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
 
    if (input$outcome_selection == "New cases"){
    pal <- colorNumeric("Greens", domain=Junedata$newcases_june)
      
    popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                         "<br>", 
                         "MODZCTA: ", as.character(Junedata$zipcode),
                         "<br>", 
                         "Total Number of Cumulative Positive Cases: ", as.character(Junedata$newcases_june)
      )
      
      leaflet() %>%
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
      
  }
  
    
    if (input$outcome_selection=="Death Rate") { 
    
        data_to_plot = Junedata %>% filter(day == input$date_choice)
        data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=Junedata$covid_death_rate)
      
      popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                         "<br>", 
                         "MODZCTA: ", as.character(Junedata$zipcode),
                         "<br>", 
                         "Total Number of Cumulative Positive Cases: ", as.character(Junedata$covid_death_rate)
      )
      
      leaflet() %>%
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
      
      
      }
  
    
    if (input$outcome_selection=="Cumulative Cases Count") { 
     
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
      
      
      }
      
    if (input$outcome_selection=="Death Count") { 
   
        data_to_plot = Junedata %>% filter(day == input$date_choice)
        data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=Junedata$covid_death_count)
      
      popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                         "<br>", 
                         "MODZCTA: ", as.character(Junedata$zipcode),
                         "<br>", 
                         "Total Number of Cumulative Positive Cases: ", as.character(Junedata$covid_death_count)
      )
      
      leaflet() %>%
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
      
      
      }
      
    if (input$outcome_selection=="Positive Cases Rate") { 
      
        data_to_plot = Junedata %>% filter(day == input$date_choice)
        data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")

      pal <- colorNumeric("Greens", domain=Junedata$covid_case_rate)
      
      popup_sb <- paste0("Neighborhood Name: ", as.character(Junedata$neighborhood_name),
                         "<br>", 
                         "MODZCTA: ", as.character(Junedata$zipcode),
                         "<br>", 
                         "Total Number of Cumulative Positive Cases: ", as.character(Junedata$covid_case_rate)
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
    
    
    })
  
      }
  
    





shinyApp(ui, server)
