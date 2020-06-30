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
    shinythemes::themeSelector(),
    navbarPage(
      # = "cosmo",
      "Demographics",
      tabPanel("Race",
               sidebarPanel(
                 # Picker for nhb name
                 selectInput("nbhid", 
                   label = "Choose a neighborhood", 
                   choices = nbh_name, 
                   selected = NULL)
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("Map",
                            h4("Table"),
                            tableOutput("table"),
                            h4("Verbatim text output"),
                            verbatimTextOutput("txtout"),
                            h1("Header 1"),
                            h2("Header 2"),
                            h3("Header 3"),
                            h4("Header 4"),
                            h5("Header 5")
                   ),
                   tabPanel("Borough Level", plotlyOutput("race_boro")),
                   tabPanel("Neighborhood Level", plotlyOutput("race_nbh"))
                 )
               )
      ),
      tabPanel("Income", "This panel is intentionally left blank"),
      tabPanel("Household", "This panel is intentionally left blank")
    )
  ),
  
  server = function(input, output) {
    output$race_boro <- renderPlotly({
      race_gp = race %>% 
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>% 
        group_by(borough_group, race) %>% 
        summarise(pop = sum(population))
      
        plot_ly(x = forcats::fct_reorder(race_gp$borough_group, race_gp$pop),
                y = race_gp$pop,
                type = "bar",
                color = race_gp$race,
                colors = "Spectral",
                opacity=0.8) %>% 
        layout(boxmode = "group",
                legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5)
        )
      
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
                color = race,
                colors = "Spectral",
                opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5))
        
    })
  }
)
