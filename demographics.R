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
  ui = tagList(
    #shinythemes::themeSelector(),
    navbarPage(
      theme = "cosmo",
      "Demographics",
      tabPanel("Map",
               tabsetPanel(
                 tabPanel("Race",
                          sidebarPanel(width = 3,
                            selectInput("boroid1", 
                                          label = "Choose a Borough", 
                                          choices =boro_name, 
                                          selected = NULL),
                            selectInput("nbhid1", 
                                          label = "Choose a Neibourhood", 
                                          choices =nbh_name, 
                                          selected = NULL),
                            pickerInput(inputId = "raceid",
                                        label = "Choose a Race",
                                        choices = str_replace_all(race_name, "_", " "),
                                        multiple = TRUE,
                                        selected = str_replace_all(race_name, "_", " ")[1],
                                        options = list(`actions-box` = TRUE)
                            )),

                          mainPanel(width = 9,
                            leafletOutput("race_map", width="100%",height="600px")
                          ),
                          
                          hr(),

                          fluidRow(
                            column(width = 12,
                                   h3("Description")
                            )
                          )),
                            
                   tabPanel("Median Income Map", 
                            sidebarPanel(width = 3,
                              selectInput("boroid2", 
                                          label = "Choose a Borough", 
                                          choices =boro_name, 
                                          selected = NULL),
                              selectInput("nbhid2", 
                                          label = "Choose a Neibourhood", 
                                          choices =nbh_name, 
                                          selected = NULL)),
                            mainPanel(width = 9,
                              leafletOutput("income_map", width="100%",height="600px")
                            )),
                            
                   tabPanel("Household", 
                            sidebarPanel(width = 3,
                              selectInput("boroid3", 
                                          label = "Choose a Borough", 
                                          choices =boro_name, 
                                          selected = NULL),
                              selectInput("nbhid3", 
                                          label = "Choose a Neibourhood", 
                                          choices =nbh_name, 
                                          selected = NULL),
                              pickerInput(inputId = "houseid",
                                          label = "Choose a Household size",
                                          choices = str_replace_all(house_name, "_", " "),
                                          multiple = TRUE,
                                          selected = str_replace_all(house_name, "_", " ")[1],
                                          options = list(`actions-box` = TRUE))),
                            mainPanel(width = 9,
                              leafletOutput("household_map", width="100%",height="600px")))
                            )
      ),
      navbarMenu("Comparison",
                 tabPanel("Race",
                          sidebarPanel(width = 3,
                                                 selectInput("nbhid4", 
                                                             label = "Choose a neighborhood", 
                                                             choices = nbh_name, 
                                                             selected = NULL)),
                          mainPanel(width = 9,
                            fluidRow(
                              column(width = 4,align="center",
                                     textOutput("nbh1"),
                                     plotlyOutput("race_nbh", width="100%",height="600px")),
                              column(width = 4,align="center",
                                     textOutput("boro1"),
                                     plotlyOutput("race_boro", width="100%",height="600px")),
                              column(width = 4,align="center",
                                     textOutput("nyc1"),
                                     plotlyOutput("race_nyc", width="100%",height="600px")))
                          )
                          ),
                 tabPanel("Income", 
                          sidebarPanel(width = 3,
                                       selectInput("nbhid5", 
                                                   label = "Choose a neighborhood", 
                                                   choices = nbh_name, 
                                                   selected = NULL)),
                          mainPanel(width = 9, plotlyOutput("income_nbh", width="100%",height="600px"))
                          ),
                 tabPanel("Household", 
                          sidebarPanel(width = 3,
                                       selectInput("nbhid6", 
                                                   label = "Choose a neighborhood", 
                                                   choices = nbh_name, 
                                                   selected = NULL)),
                          mainPanel(width = 9,
                                    fluidRow(
                                      column(width = 4,align="center",
                                             textOutput("nbh2"),
                                             plotlyOutput("household_nbh", width="100%",height="600px")),
                                      column(width = 4,align="center",
                                             textOutput("boro2"),
                                             plotlyOutput("household_boro", width="100%",height="600px")),
                                      column(width = 4,align="center",
                                             textOutput("nyc2"),
                                             plotlyOutput("household_nyc", width="100%",height="600px"))
                                    )
                                    )))
                               
      
      )
  ),
  
  server = function(input, output) {
    
    output$nbh1 <- renderText({
      
      input$nbhid4
      
    })
    output$nbh2 <- renderText({
      
      input$nbhid6
      
    })
    
    output$nyc1 <- renderText({"New York City"})
    output$nyc2 <- renderText({"New York City"})
    
    output$boro1 <- renderText({
      
      which_boro = race %>% filter(neighborhood_name == input$nbhid4) %>% select(borough_group) %>% unique()
      which_boro$borough_group
      
    })
    output$boro2 <- renderText({
      
      which_boro = race %>% filter(neighborhood_name == input$nbhid6) %>% select(borough_group) %>% unique()
      which_boro$borough_group
      
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
                  title = "Percentage on NTA Level")
      
    })
    output$race_boro_tb <- renderTable({
      race %>% 
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>% 
        group_by(borough_group, race) %>% 
        summarise(pop = sum(population)) %>% 
        pivot_wider(
          names_from = race,
          values_from  = pop
        ) %>% 
        rename(borough = borough_group) %>% 
        rename_all(~ gsub("_", " ", .)) %>% 
        rename_all(~ gsub("alone", "", .))
    })
    output$race_nbh <- renderPlotly({
      race_nbh = race %>% 
        filter(neighborhood_name == input$nbhid4) %>%
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
      
      which_boro = race %>% filter(neighborhood_name == input$nbhid4) %>% select(borough_group) %>% unique()
      
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
    output$race_nbh_tb <- renderTable({
      race %>% 
        filter(neighborhood_name == input$nbhid4) %>%
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>% 
        group_by(neighborhood_name, race) %>% 
        summarise(pop = sum(population)) %>% 
        pivot_wider(
          names_from = race,
          values_from  = pop
        ) %>% 
        rename(neighborhood = neighborhood_name) %>% 
        rename_all(~ gsub("_", " ", .)) %>% 
        rename_all(~ gsub("alone", "", .))
    })
    
    output$income_map <- renderLeaflet({
      posi_map_income = geo_join(spdf,income,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=posi_map_income$median)
      
      # Getting rid of rows with NA values
      posi_map_income <- subset(posi_map_income, !is.na(median))
      
      # Setting up the pop up text
      popup_sb <- paste0("<b> ZCTA: </b>", posi_map_income$zipcode, 
                         "<br>", 
                         "<b> NBH: </b>", posi_map_income$neighborhood_name,
                         "<br>", 
                         "<b> Borough: </b>", posi_map_income$borough_group,
                         "<br>", 
                         "<b> Median Income: $ </b>", as.character(round(posi_map_income$median),0))
      
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.71181, zoom = 10) %>% 
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
    output$income_boro_tb <- renderTable({
      income %>% 
        pivot_longer(mean:median, names_to = "stat", values_to = "value") %>% 
        drop_na() %>% 
        group_by(borough_group, stat) %>% 
        summarise(val = ifelse(stat == "median", median(value), mean(value))) %>% 
        distinct() %>% 
        pivot_wider(
          names_from = stat,
          values_from  = val
        ) %>% 
        rename(borough = borough_group) %>% 
        rename_all(~ gsub("_", " ", .))
    })
    #output$income_nbh <- renderPlotly ({
    #  income_nbh = income %>% 
    #    filter(neighborhood_name == input$nbhid5) %>%
    #    pivot_longer(mean:median, names_to = "stat", values_to = "value")
    #  
    #  which_boro = income %>% filter(neighborhood_name == input$nbhid5) %>% select(borough_group) %>% unique()
    #  
    #  income_boro = income %>% 
    #    filter(borough_group == which_boro$borough_group) %>% 
    #    pivot_longer(mean:median, names_to = "stat", values_to = "value")
    #    
#
    #  
    #  p = plot_ly(x = forcats::fct_reorder(income_nbh$stat, income_nbh$value),
    #          y = income_nbh$value,
    #          type = "bar",
    #          color = income_nbh$stat,
    #          colors = "Set1",
    #          size = 3) %>% 
    #    layout(legend=list(title=list(text='<b> Statistics </b>')))
    #  
    #  add_bars(p, 
    #           x = forcats::fct_reorder(income_nbh$stat, income_nbh$value), 
    #           y = )
#
    #})
    
    output$household_map <- renderLeaflet({
      pt = household %>% 
        select(zipcode:total, str_replace_all(input$houseid," ","_"))

      pct = rowSums(pt[5:ncol(pt)])/pt$total
      
      house_pt = household %>% 
        mutate(percentage = pct)
        

      posi_map_house = geo_join(spdf,house_pt,"MODZCTA","zipcode")
      
      pal <- colorNumeric("Greens", domain=posi_map_house$percentage)
      
      # Getting rid of rows with NA values
      posi_map_house <- subset(posi_map_house, !is.na(percentage))
        
        # Setting up the pop up text
        popup_sb <- paste0("<b> ZCTA: </b>", posi_map_house$zipcode, 
                           "<br>", 
                           "<b> NBH: </b>", posi_map_house$neighborhood_name,
                           "<br>", 
                           "<b> Borough: </b>", posi_map_house$borough_group,
                           "<br>", 
                           "<b> Total: </b>", posi_map_house$total,
                           "<br>", 
                           "<b> Percentage: </b>", round(posi_map_house$percentage*100,2), "%")
        
        
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -73.99653, lat = 40.71181, zoom = 10) %>% 
          addPolygons(data = posi_map_house, 
                      fillColor = ~pal(posi_map_house$percentage), 
                      fillOpacity = 0.7, 
                      weight = 0.2, 
                      smoothFactor = 0.2,
                      popup = ~popup_sb) %>%
          
          addLegend(pal = pal, 
                    values = posi_map_house$percentage, 
                    position = "bottomright", 
                    title = "Percentage on NTA Level")
        
    })
    output$household_boro <- renderPlotly({
      
      which_boro = race %>% filter(neighborhood_name == input$nbhid6) %>% select(borough_group) %>% unique()
      
      house_gp = household %>% 
        filter(borough_group == which_boro$borough_group) %>% 
        pivot_longer(person_1:person_7_or_more, names_to = "size", values_to = "number") %>% 
        group_by(size) %>% 
        summarise(num = sum(number)) %>% 
        drop_na()
      
      plot_ly(labels = str_replace_all(house_gp$size,"_"," "),
              values = house_gp$num,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'),orientation = 'h', xanchor = "center", x = 0.5))
      
      
    })
    output$household_nbh <- renderPlotly({
      household_nbh = household %>% 
        filter(neighborhood_name == input$nbhid6) %>%
        pivot_longer(person_1:person_7_or_more, names_to = "size", values_to = "number") %>%
        group_by(neighborhood_name, size) %>% 
        summarise(num = sum(number)) %>% 
        drop_na()
      
      plot_ly(labels = factor(household_nbh$size, levels = c("person_1", "person_2", "person_3", "person_4", "person_5", "person_6", "person_7_or_more")),
              values = household_nbh$num,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Family Size </b>'), orientation = 'h', xanchor = "center", x = 0.5))
      
    })
    output$household_nyc <- renderPlotly({
      house_nyc = household %>% 
        pivot_longer(person_1:person_7_or_more, names_to = "size", values_to = "number") %>%
        group_by(size) %>% 
        summarise(num = sum(number)) %>% 
        drop_na()
      
      plot_ly(labels = str_replace_all(house_nyc$size,"_"," "),
              values = house_nyc$num,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5))
      
    })
    
    
  }
)
