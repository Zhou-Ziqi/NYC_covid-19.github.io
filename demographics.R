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


library(maptools)
library(rgeos)
library(rgdal)
library(ggthemes)


spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")

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

race_new = 
  rbind(race,c(99999, rep(0,10)))



#household_new = rbind(household,c(99999, rep(0,10)))

#mergedata_race <- merge(race_new, spdf@data, by.y = "MODZCTA", by.x = "zipcode", sort=FALSE)
#mergedata_household <- merge(household_new, spdf@data, by.y = "MODZCTA", by.x = "zipcode", sort=FALSE)


# Define UI for application that draws a histogram
zipcode = sex_age %>% distinct(zipcode) %>% pull()
nbh_name = sex_age %>% distinct(neighborhood_name) %>% pull() %>% sort()
boro_name = sex_age %>% distinct(borough_group) %>% pull() %>% sort()
race_name = sort(colnames(race)[5:11])
house_name  = colnames(household)[5:11]

### Define App

shinyApp(
  ui = fluidPage(
    h1("Demographics"),
    br(),
    br(),
    fluidRow(
      column(width = 4, offset = 1, selectInput("character",
                                                "Choose a characteristics",
                                                c("please select..." = "plz",
                                                  "Race" = "race",
                                                  "Income" = "income",
                                                  "Household Size" = "house"))),
      column(width = 5, "this part will have some instructions"),
    ),
    hr(),
    conditionalPanel(
      condition = "input.character == 'race'",
      h2("Comparison"),
      fluidRow(
        column(width = 3,
               sidebarPanel(width = 12,
                            selectInput("nbhid1", 
                                        label = "Choose a Neibourhood", 
                                        choices =nbh_name, 
                                        selected = NULL))),
        column(width = 9, h4("some words to describe the pie chart"))),
      br(),
      fluidRow(

        column(width = 4,align="center",
               textOutput("nbh1"),
               plotlyOutput("race_nbh", width="100%",height="500px")),
        column(width = 4,align="center",
               textOutput("boro1"),
               plotlyOutput("race_boro", width="100%",height="500px")),
        column(width = 4,align="center",
               textOutput("nyc1"),
               plotlyOutput("race_nyc", width="100%",height="500px"))),
      hr(),
      h1("Map"),
      fluidRow(
        column(width = 3,
               verticalLayout(
                 sidebarPanel(width = 12,
                              pickerInput(inputId = "raceid",
                                          label = "Choose a Race",
                                          choices = str_replace_all(race_name, "_", " "),
                                          multiple = TRUE,
                                          selected = str_replace_all(race_name, "_", " ")[1],
                                          options = list(`actions-box` = TRUE)
                              )),
                 hr(),
                 h4("some words to describe the map"))),
        column(width = 9,leafletOutput("race_map", width="100%",height="700px")))
    )
  ),
  
  server = function(input, output){
    
    output$nbh1 <- renderText({
      
      input$nbhid1
      
    })
    output$nyc1 <- renderText({"New York City"})
    output$boro1 <- renderText({
      
      which_boro = race %>% filter(neighborhood_name == input$nbhid1) %>% select(borough_group) %>% unique()
      which_boro$borough_group
      
    })
    
    output$race_nbh <- renderPlotly({
      race_nbh = race %>% 
        filter(neighborhood_name == input$nbhid1) %>%
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>%
        group_by(neighborhood_name, race) %>% 
        summarise(pop = sum(population)) %>% 
        drop_na()
      
      plot_ly(labels = str_replace_all(race_nbh$race,"_"," "),
              values = race_nbh$pop,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5))
      
    })
    output$race_boro <- renderPlotly({
      
      which_boro = race %>% filter(neighborhood_name == input$nbhid1) %>% select(borough_group) %>% unique()
      
      race_gp = race %>% 
        filter(borough_group == which_boro$borough_group) %>% 
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>% 
        group_by(race) %>% 
        summarise(pop = sum(population)) %>% 
        drop_na()
      
      plot_ly(labels = str_replace_all(race_gp$race,"_"," "),
              values = race_gp$pop,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'),orientation = 'h', xanchor = "center", x = 0.5))
      
      
    })
    output$race_nyc <- renderPlotly({
      race_nyc = race %>% 
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>%
        group_by(race) %>% 
        summarise(pop = sum(population)) %>% 
        drop_na()
      
      plot_ly(labels = str_replace_all(race_nyc$race,"_"," "),
              values = race_nyc$pop,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5))
      
    })
    output$race_map <- renderLeaflet({
      pt = race %>% 
        select(zipcode:total, str_replace_all(input$raceid," ","_"))
      
      pct = rowSums(pt[5:ncol(pt)])/pt$total
      
      race_pt = race %>% 
        mutate(percentage = pct)
      
      
      posi_map_race = geo_join(spdf,race_pt,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=posi_map_race$percentage)
      
      # Getting rid of rows with NA values
      posi_map_race <- subset(posi_map_race, !is.na(percentage))
      
      # Setting up the pop up text
      popup_sb <- paste0("<b> ZCTA: </b>", posi_map_race$zipcode, 
                         "<br>", 
                         "<b> NBH: </b>", posi_map_race$neighborhood_name,
                         "<br>", 
                         "<b> Borough: </b>", posi_map_race$borough_group,
                         "<br>", 
                         "<b> Total: </b>", posi_map_race$total,
                         "<br>", 
                         "<b> Percentage: </b>", round(posi_map_race$percentage*100,2), "%")
      
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.71181, zoom = 10) %>% 
        addPolygons(data = posi_map_race, 
                    fillColor = ~pal(posi_map_race$percentage), 
                    fillOpacity = 0.7, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    popup = ~popup_sb) %>%
        addLegend(pal = pal, 
                  values = posi_map_race$percentage, 
                  position = "bottomright", 
                  title = "Proportions on NTA Level")
      
    })
  }
)












