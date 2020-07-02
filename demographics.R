library(shiny)
library(flexdashboard)
library(tidyverse)
library(readxl)
library(viridis)
library(leaflet)
library(plotly)
library(shinyWidgets)

library(tigris)
library(sp)

library(tmap)    # for static and interactive maps

spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
spdf@data

data_by_modzcta = read_csv("data/data-by-modzcta.csv") %>% 
  select(1:3) %>% 
  janitor::clean_names() %>% 
  rename(zipcode = modified_zcta)


sex_age_raw = read_csv("data/predictor_data/sex_by_age_zipcode_nyc.csv") %>% select(-1)
sex_age = left_join(sex_age_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything())

income_raw = read_csv("data/predictor_data/median_income_nyc.csv") %>% select(-1)
income = left_join(income_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything())

household_raw = read_csv("data/predictor_data/household_zipcode_nyc.csv") %>% select(-1)
household = left_join(household_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything())

race_raw = read_csv("data/predictor_data/race_by_zipcode_nyc.csv") %>% select(-1)
race = left_join(race_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything())

# Define UI for application that draws a histogram
zipcode = sex_age %>% distinct(zipcode) %>% pull()
nbh_name = sex_age %>% distinct(neighborhood_name) %>% pull() %>% sort()

### Define App

shinyApp(
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      theme = "cosmo",
      "Demographics",
      tabPanel("Race",
               sidebarPanel(
                 # Picker for nhb name ##多选
                 selectInput("nbhid", 
                   label = "Choose a neighborhood", 
                   choices = nbh_name, 
                   selected = NULL)),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", leafletOutput("race_map", width="100%",height="600px")), ###
                   tabPanel("Borough Level", plotlyOutput("race_boro", width="100%",height="600px")),
                   tabPanel("Neighborhood Level", plotlyOutput("race_nbh", width="100%",height="600px"))))
      ),
      tabPanel("Income",
               sidebarPanel(
                 # Picker for nhb name
                 selectInput("nbhid", 
                             label = "Choose a neighborhood", 
                             choices = nbh_name, 
                             selected = NULL)),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Median Income Map", leafletOutput("income_map", width="100%",height="600px")),
                   tabPanel("Mean Income Map", leafletOutput("income_map1", width="100%",height="600px")),
                   tabPanel("Borough Level", plotlyOutput("income_boro", width="100%",height="600px"))
                 )
               )
      ),
      tabPanel("Household",
               sidebarPanel(
                 # Picker for nhb name
                 selectInput("nbhid1", 
                             label = "Choose a neighborhood", 
                             choices = nbh_name, 
                             selected = NULL)),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map", leafletOutput("household_map", width="100%",height="600px")),
                   tabPanel("Borough Level", plotlyOutput("household_boro", width="100%",height="600px")),
                   tabPanel("Neighborhood Level", plotlyOutput("household_nbh", width="100%",height="600px"))
                 )
               )
          )
    )
  ),
  
  server = function(input, output) {
    output$race_map <- renderLeaflet({
      
    mergedata <- merge(race_new, spdf@data, by.y = "MODZCTA", by.x = "zipcode", sort=FALSE)
    num.dots <- select(mergedata, white_alone:two_or_more_races) / 100
    
    sp.dfs <- lapply(names(num.dots), function(x) {
      dotsInPolys(spdf, as.integer(num.dots[,x]), f="random")
    })
    
    pal <- c("#8DD3C7", "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69")
    
    
    
    par(mar = c(0,0,0,0))
    plot(spdf, lwd = 0.01, border = "black")
    for (i in 1:length(sp.dfs)) {
      plot(sp.dfs[[i]], add = T, pch = 16, cex = 0.1, col = pal[i])
    }
    })
    
    output$race_boro <- renderPlotly({
      race_gp = race %>% 
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") 
      #%>% 
        #group_by(borough_group, race) %>% 
        #summarise(pop = sum(population))
      
        #plot_ly(x = forcats::fct_reorder(race_gp$borough_group, race_gp$pop),
        #        y = race_gp$pop,
        #        type = "bar",
        #        color = race_gp$race,
        #        colors = "Spectral",
        #        opacity=0.8) %>% 
        #layout(boxmode = "group",
        #        legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5)
        #)
        
       plot_ly(x = forcats::fct_reorder(race_gp$borough_group, race_gp$population),
               y = race_gp$population,
               type = "box",
               colors = "Spectral",
               color = race_gp$race) %>% 
         layout(boxmode = "group",
                legend=list(title=list(text='<b> Family Size </b>'),orientation = 'h', xanchor = "center", x = 0.5))
       
      
    })
    
    
    output$race_nbh <- renderPlotly({
      race_nbh = race %>% 
        filter(neighborhood_name == input$nbhid) %>%
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>%
        group_by(neighborhood_name, race) %>% 
        summarise(pop = sum(population)) %>% 
        drop_na()
      
        plot_ly(labels = race_nbh$race,
                values = race_nbh$pop,
                type = "pie",
                colors = "Spectral",
                opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5))
        
    })
    
    output$income_map <- renderLeaflet({
      posi_map_income = geo_join(spdf,income,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=posi_map_income$median)
      
      # Getting rid of rows with NA values
      posi_map_income <- subset(posi_map_income, !is.na(median))
      
      # Setting up the pop up text
      popup_sb <- paste0("ZCTA: ", posi_map_income$zipcode, 
                         "<br>", 
                         "NBH: ", posi_map_income$neighborhood_name,
                         "<br>", 
                         "Borough: ", posi_map_income$borough_group,
                         "<br>", 
                         "Median Income: $", as.character(round(posi_map_income$median),0))
      
      
      leaflet(height = "100%") %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.71181, zoom = 11) %>% 
        addPolygons(data = posi_map_income , 
                    fillColor = ~pal(posi_map_income$median), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = posi_map_income$median, 
                  position = "bottomright", 
                  title = "Median Income")
    })
    
    output$income_map1 <- renderLeaflet({
      posi_map_income = geo_join(spdf,income,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=posi_map_income$mean)
      
      # Getting rid of rows with NA values
      posi_map_income <- subset(posi_map_income, !is.na(mean))
      
      # Setting up the pop up text
      popup_sb <- paste0("ZCTA: ", posi_map_income$zipcode, 
                         "<br>", 
                         "NBH: ", posi_map_income$neighborhood_name,
                         "<br>", 
                         "Borough: ", posi_map_income$borough_group,
                         "<br>", 
                         "Median Income: $", as.character(round(posi_map_income$mean),0))
      
      
      leaflet(height = "100%") %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.71181, zoom = 11) %>% 
        addPolygons(data = posi_map_income , 
                    fillColor = ~pal(posi_map_income$mean), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = posi_map_income$mean, 
                  position = "bottomright", 
                  title = "mean Income")
    })
    
    
    
    output$income_boro <- renderPlotly ({
      income_gp = income %>% 
        pivot_longer(mean:median, names_to = "stat", values_to = "value")

      plot_ly(x = forcats::fct_reorder(income_gp$borough_group, income_gp$value),
              y = income_gp$value,
              type = "box",
              color = income_gp$stat,
              colors = "Set1") %>% 
        layout(boxmode = "group",
               legend=list(title=list(text='<b> Statistics </b>')))
    })
    
    output$household_boro <- renderPlotly({
      
      household_gp = household %>% 
        pivot_longer("1":"7_or_more", names_to = "size", values_to = "number")
      
      plot_ly(x = forcats::fct_reorder(household_gp$borough_group, household_gp$number),
              y = household_gp$number,
              type = "box",
              colors = "Spectral",
              color = household_gp$size) %>% 
        layout(boxmode = "group",
               legend=list(title=list(text='<b> Family Size </b>'),orientation = 'h', xanchor = "center", x = 0.5))
               
      
    })
    
    
    output$household_nbh <- renderPlotly({
      household_nbh = household %>% 
        filter(neighborhood_name == input$nbhid1) %>%
        pivot_longer("1":"7_or_more", names_to = "size", values_to = "number") %>%
        group_by(neighborhood_name, size) %>% 
        summarise(num = sum(number)) %>% 
        drop_na()
      
      plot_ly(labels = household_nbh$size,
              values = household_nbh$num,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Family Size </b>'), orientation = 'h', xanchor = "center", x = 0.5))
      
    })
    
    
  }
)
