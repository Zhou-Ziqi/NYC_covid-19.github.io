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

household_new = 
  rbind(household,c(99999, rep(0,10)))

mergedata_race <- merge(race_new, spdf@data, by.y = "MODZCTA", by.x = "zipcode", sort=FALSE)
mergedata_household <- merge(household_new, spdf@data, by.y = "MODZCTA", by.x = "zipcode", sort=FALSE)


# Define UI for application that draws a histogram
zipcode = sex_age %>% distinct(zipcode) %>% pull()
nbh_name = sex_age %>% distinct(neighborhood_name) %>% pull() %>% sort()
boro_name = sex_age %>% distinct(borough_group) %>% pull() %>% sort()
race_name = race_name = colnames(race)[5:11]
house_name = household = colnames(household)[5:11]

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
                          sidebarPanel(
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

                          mainPanel(
                            leafletOutput("race_map", width="100%",height="600px")
                          ),
                          
                          hr(),

                          fluidRow(
                            column(width = 12,
                                   h3("Description")
                            )
                          )),
                            
                   tabPanel("Median Income Map", 
                            sidebarPanel(
                              selectInput("boroid2", 
                                          label = "Choose a Borough", 
                                          choices =boro_name, 
                                          selected = NULL),
                              selectInput("nbhid2", 
                                          label = "Choose a Neibourhood", 
                                          choices =nbh_name, 
                                          selected = NULL)),
                            mainPanel(
                              leafletOutput("income_map", width="100%",height="600px")
                            )),
                            
                   tabPanel("Household", 
                            sidebarPanel(
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
                            mainPanel(
                              leafletOutput("household_map", width="100%",height="600px")))
                            )
      ),
      tabPanel("Comparison",
               tabsetPanel(
                 tabPanel("Race",
                          sidebarPanel(
                            selectInput("nbhid2", 
                                        label = "Choose a neighborhood", 
                                        choices = nbh_name, 
                                        selected = NULL)),
                            
                          mainPanel(plotlyOutput("race_nbh", width="100%",height="600px"))),
                          
                 tabPanel("Income", plotlyOutput("income_nbh", width="100%",height="600px")),
                 tabPanel("Household", plotlyOutput("household_nbh", width="100%",height="600px"))
               )         
      )
      )
  ),
  
  server = function(input, output) {
    
    output$race_map <- renderLeaflet({
      spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
      num.dots.data <- mergedata %>% 
        select(str_replace_all(input$raceid," ","_"))
      
      num.dots <- num.dots.data / 200 + 1
        
        

      
      sp.dfs <- lapply(names(num.dots), function(x) {
        dotsInPolys(spdf, as.integer(num.dots[,x]), f="random")
      })
      #####
      spdf@data$id <- row.names(spdf@data)
      spdf.points <- fortify(spdf, region = "id")
      spdf.df <- merge(spdf.points, spdf@data, by = "id") %>% 
        filter(MODZCTA != 99999)
      

      dfs <- lapply(sp.dfs, function(x) {
        data.frame(coordinates(x)[,1:2])
      })
      

      for (i in 1:length(input$raceid)) {
        dfs[[i]]$Race <- input$raceid[i]
      }
      
      dots.final <- bind_rows(dfs)
      dots.final$Race <- str_replace_all(factor(dots.final$Race, levels = input$raceid),"_"," ")
      ####
      posi_map_race = geo_join(spdf,race,"MODZCTA","zipcode")
      
      pal <- colorFactor("Spectral", unique(dots.final$Race))
      
      posi_map_race <- subset(posi_map_race, !is.na(total))
      
      
      # Setting up the pop up text
      popup_sb <- paste0("<b> ZCTA: </b>", posi_map_race$zipcode, 
                         "<br>", 
                         "<b> NBH: </b>", posi_map_race$neighborhood_name,
                         "<br>", 
                         "<b> Borough: </b>", posi_map_race$borough_group,
                         "<br>", 
                         "<b> Total: </b>", posi_map_race$total,
                         "<br>", 
                         "<b> White alone: </b>", posi_map_race$white_alone,
                         "<br>", 
                         "<b> Black or African American alone: </b>",posi_map_race$black_or_african_american_alone,
                         "<br>", 
                         "<b> American indian and Alaska native alone: </b>", posi_map_race$american_indian_and_alaska_native_alone,
                         "<br>", 
                         "<b> Asian alone: </b>", posi_map_race$asian_alone,
                         "<br>", 
                         "<b> Native Hawaiian and other Pacific Islander alone: </b>", posi_map_race$native_hawaiian_and_other_pacific_islander_alone,
                         "<br>", 
                         "<b> Some other race alone: </b>", posi_map_race$some_other_race_alone,
                         "<br>", 
                         "<b> Two or more races: </b>", posi_map_race$two_or_more_races)
      
      
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -73.99653, lat = 40.71181, zoom = 11) %>% 
        addCircleMarkers(data = dots.final, lng = dots.final$x, lat = dots.final$y,
                         color = ~pal(Race),
                         radius = 1,
                         stroke = FALSE, fillOpacity = 0.7) %>% 
        addPolygons(data = posi_map_race, 
                    fillColor = "white", 
                    fillOpacity = 0.00001, 
                    weight = 0.2, 
                    smoothFactor = 0.2,
                    popup = ~popup_sb) %>%
       
        addLegend(pal = pal, 
                  values = unique((str_replace_all(dots.final$Race, "_", " "))), 
                  position = "bottomright", 
                  title = "Race")
      
    })
    
    output$race_boro <- renderPlotly({
      race_gp = race %>% 
        pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") 
      
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
        filter(neighborhood_name == input$nbhid1) %>%
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
    
    output$race_nbh_tb <- renderTable({
      race %>% 
        filter(neighborhood_name == input$nbhid1) %>%
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
      spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
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
    
    output$income_nbh <- renderPlotly ({
      income_nbh = income %>% 
        filter(neighborhood_name == input$nbhid2) %>%
        pivot_longer(mean:median, names_to = "stat", values_to = "value")

      
      plot_ly(x = forcats::fct_reorder(income_nbh$stat, income_nbh$value),
              y = income_nbh$value,
              type = "scatter",
              color = income_nbh$stat,
              colors = "Set1",
              size = 3) %>% 
        layout(legend=list(title=list(text='<b> Statistics </b>')))

    })
    
    output$household_map <- renderLeaflet({
      spdf = rgdal::readOGR("Geography-resources/MODZCTA_2010_WGS1984.geo.json")
      
        num.dots.data <- mergedata_household %>% 
          select(str_replace_all(input$houseid," ","_"))
        
        num.dots <- num.dots.data / 200 + 1
        
        
        
        
        sp.dfs <- lapply(names(num.dots), function(x) {
          dotsInPolys(spdf, as.integer(num.dots[,x]), f="random")
        })
        #####
        spdf@data$id <- row.names(spdf@data)
        spdf.points <- fortify(spdf, region = "id")
        spdf.df <- merge(spdf.points, spdf@data, by = "id") %>% 
          filter(MODZCTA != 99999)
        
        
        dfs <- lapply(sp.dfs, function(x) {
          data.frame(coordinates(x)[,1:2])
        })
        
        
        for (i in 1:length(input$houseid)) {
          dfs[[i]]$House <- input$houseid[i]
        }
        
        dots.final <- bind_rows(dfs)
        dots.final$House <- str_replace_all(factor(dots.final$House, levels = input$houseid),"_"," ")
        ####
        posi_map_house = geo_join(spdf,race,"MODZCTA","zipcode")
        
        pal <- colorFactor("Spectral", unique(dots.final$House))
        
        posi_map_house <- subset(posi_map_house, !is.na(total))
        
        
        # Setting up the pop up text
        popup_sb <- paste0("<b> ZCTA: </b>", posi_map_house$zipcode, 
                           "<br>", 
                           "<b> NBH: </b>", posi_map_house$neighborhood_name,
                           "<br>", 
                           "<b> Borough: </b>", posi_map_house$borough_group,
                           "<br>", 
                           "<b> Total: </b>", posi_map_house$total)
        
        
        leaflet() %>%
          addProviderTiles("CartoDB.Positron") %>%
          setView(lng = -73.99653, lat = 40.71181, zoom = 11) %>% 
          addCircleMarkers(data = dots.final, lng = dots.final$x, lat = dots.final$y,
                           color = ~pal(House),
                           radius = 1,
                           stroke = FALSE, fillOpacity = 0.7) %>% 
          addPolygons(data = posi_map_house, 
                      fillColor = "white", 
                      fillOpacity = 0.00001, 
                      weight = 0.2, 
                      smoothFactor = 0.2,
                      popup = ~popup_sb) %>%
          
          addLegend(pal = pal, 
                    values = unique((str_replace_all(dots.final$House, "_", " "))), 
                    position = "bottomright", 
                    title = "Household Size")
        
    })
    
    output$household_boro <- renderPlotly({
      
      household_gp = household %>% 
        pivot_longer(`1`:`7_or_more`, names_to = "size", values_to = "number")
      
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
        filter(neighborhood_name == input$nbhid3) %>%
        pivot_longer(`1`:`7_or_more`, names_to = "size", values_to = "number") %>%
        group_by(neighborhood_name, size) %>% 
        summarise(num = sum(number)) %>% 
        drop_na()
      
      plot_ly(labels = factor(household_nbh$size, levels = c("1", "2", "3", "4", "5", "6", "7_or_more")),
              values = household_nbh$num,
              type = "pie",
              colors = "Spectral",
              opacity=0.8) %>% 
        layout(legend=list(title=list(text='<b> Family Size </b>'), orientation = 'h', xanchor = "center", x = 0.5))
      
    })
    
    
  }
)
