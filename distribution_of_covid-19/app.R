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
spdf = rgdal::readOGR("./data/Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data

Junedata = read.csv("./data/final_Junedata.csv") %>% 
    mutate(day = as.Date(day))
#input the widges
date = Junedata %>% distinct(day) %>% pull()


choices = c("Cumulative Cases Count", "Death Count", "Positive Cases Rate", "Death Rate","New cases")



### write the functions to draw the map
###positive
positive = function(date){
    data_to_plot = Junedata %>% filter(day == date)
    data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
    data_to_plot = subset(data_to_plot, !is.na(positive))
    pal <- colorNumeric("Greens", domain=data_to_plot$positive)
    
    popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                       "<br>", 
                       "MODZCTA: ", as.character(data_to_plot$zipcode),
                       "<br>", 
                       "Total Number of Cumulative Positive Cases: ", as.character(data_to_plot$positive)
    )
    
    p1 = leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data =  data_to_plot_geo , 
                    fillColor = ~pal(data_to_plot$positive), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values =  data_to_plot$positive, 
                  position = "bottomright", 
                  title = "Number")
    p1
}
### case rate
case_rate = function(date){ 
    
    data_to_plot = Junedata %>% filter(day == date)
    data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
    data_to_plot = subset(data_to_plot, !is.na(covid_case_rate))
    pal <- colorNumeric("Greens", domain=data_to_plot$covid_case_rate)
    
    popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                       "<br>", 
                       "MODZCTA: ", as.character(data_to_plot$zipcode),
                       "<br>", 
                       "Case Rate of COVID-19: ", as.character(data_to_plot$covid_case_rate)
    )
    
    leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data =  data_to_plot_geo , 
                    fillColor = ~pal(data_to_plot$covid_case_rate), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values =  data_to_plot$covid_case_rate, 
                  position = "bottomright", 
                  title = "Number")
    
}
##death count
death_count = function(date){
    
    data_to_plot = Junedata %>% filter(day == date)
    data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
    data_to_plot = subset(data_to_plot, !is.na(covid_death_count))
    pal <- colorNumeric("Reds", domain=data_to_plot$covid_death_count)
    
    popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                       "<br>", 
                       "MODZCTA: ", as.character(data_to_plot$zipcode),
                       "<br>", 
                       "Total Number of Death: ", as.character(data_to_plot$covid_death_count)
    )
    
    p1 = leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data =  data_to_plot_geo , 
                    fillColor = ~pal(data_to_plot$covid_death_count), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = data_to_plot$covid_death_count, 
                  position = "bottomright", 
                  title = "Number")
    p1
}
##death rate
death_rate = function(date){
    data_to_plot = Junedata %>% filter(day == date)
    data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
    data_to_plot = subset(data_to_plot, !is.na(covid_death_rate))
    pal <- colorNumeric("Reds", domain=data_to_plot$covid_death_rate)
    
    popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                       "<br>", 
                       "MODZCTA: ", as.character(data_to_plot$zipcode),
                       "<br>", 
                       "Death Rate: ", as.character(data_to_plot$covid_death_rate)
    )
    
    p1 = leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data =  data_to_plot_geo , 
                    fillColor = ~pal(data_to_plot$covid_death_rate), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values =  data_to_plot$covid_death_rate, 
                  position = "bottomright", 
                  title = "Number")
    p1
}
###new cases
newcase = function(date){
    
    data_to_plot = Junedata %>% filter(day == date)
    data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","zipcode")
    data_to_plot = subset(data_to_plot, !is.na(newcases_june))
    pal <- colorNumeric("Greens", domain=data_to_plot$newcases_june)
    
    popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                       "<br>", 
                       "MODZCTA: ", as.character(data_to_plot$zipcode),
                       "<br>", 
                       "Total Number of New Cases: ", as.character(data_to_plot$newcases_june)
    )
    
    p1 = leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
        addPolygons(data =  data_to_plot_geo , 
                    fillColor = ~pal(data_to_plot$newcases_june), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2, 
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values =  data_to_plot$newcases_june, 
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
            
            dateInput(
                inputId = "date_choice", 
                label = "Select Date",
                format = "yyyy-mm-dd",
                value = "2020-06-01",
                language = "en"
               ),
            
            radioButtons(inputId = "outcome_selection",
                         label =  "Outcome:",   
                         c("Cumulative Cases Count" = "positive",
                           "Positive Cases Rate (per 100,000 people)" = "case_count", 
                           "Death Count" = "death_count", 
                           "Death Rate (per 100,000 people)" = "death_rate",
                           "New cases" = "newcase")
                         ),
            
            helpText("Cumulative Case Count is the count of confirmed cases."),
            helpText("Positive Case Rate is the rate of confirmed cases per 100,000 people by ZCTA."),
            helpText("Population denominators for ZCTAs derived from intercensal estimates by the Bureau of Epidemiology Services"),
            helpText("Death Count is the count of confirmed deaths"),
            helpText("Death Rate is the rate of confirmed deaths per 100,000 people by ZCTA"),
            helpText("Rates per 100,000 people"),
            helpText("Rates for annual citywide-, borough-, ZCTA (ZIP Code Tabulation Area)-, and demographic-specific categories were calculated using interpolated intercensal population estimates updated in 2019. These rates differ from previously reported rates based on the 2000 Census or previous versions of population estimates. The Health Department produced these population estimates based on estimates from the U.S. Census Bureau and NYC Department of City Planning.
           ")
            
           
            
            
            
            
            ),
        
        
        mainPanel(leafletOutput(outputId = "map",width="100%",height="600px"))
        
    ))


###### server

server = function(input, output) {
    
    
    
    
    output$map = renderLeaflet({
        
        plot = switch (input$outcome_selection,
                       positive = positive,
                       death_count = death_count,
                       case_count = case_rate,
                       death_rate = death_rate,
                       newcase = newcase
        )
        
        plot(input$date_choice)
    })
    
    output$piechart = renderPlotly({
        
    })
    
}







shinyApp(ui, server)
