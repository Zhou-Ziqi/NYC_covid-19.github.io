# Recently COVID-19 in NYC tracker

library(tidyverse)
library(shiny)
library(DT)

library(readxl)
library(sp)
library(tmap)  
library(viridis)
library(leaflet)
library(tigris)
library(plotly)
library(shinyWidgets)
library(table1)
library(htmlTable)


##read data

data_yester = read_csv("./data/data-by-modzcta0722.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date("2020-07-22"))
data_today = read_csv("./data/data-by-modzcta0723.csv") %>% 
  janitor::clean_names() %>% 
  mutate(date = as.Date("2020-07-23"))

data = rbind(data_today,data_yester)
new_case = 0
new_death = 0

for (i in 1:177) {
  new_case[i] = data[i,4] - data[i+177,4]
  new_case
  
  new_death[i] = data[i,7] - data[i+177,7]
  new_death
}

for (i in 178:354) {
  
  new_case[i] = NA
  
  new_death[i] = NA
}

data_to_table = data %>% 
  mutate(new_case = new_case,
         new_death = new_death) %>% 
  filter(date == max(date)) %>% 
  mutate(new_case = as.numeric(new_case),
         new_death = as.numeric(new_death),
         incidence_rate = round(new_case*100000/pop_denominator, digits = 1) )%>% 
  dplyr::select(modified_zcta,neighborhood_name,borough_group,
                covid_case_count,new_case,incidence_rate,
                covid_death_count,new_death) %>% 
  rename("Incremental Positive Cases" = new_case,
         "Incremental Death Cases" = new_death,
         Zipcode = modified_zcta,
         "Neighborhood" = neighborhood_name,
         "Borough"= borough_group,
         "Cumulative Positive Case Count" = covid_case_count,
         "Cumulative Death Count" = covid_death_count,
         "Incidence Rate" = incidence_rate)

data_to_plot = data %>% 
  mutate(new_case = new_case,
         new_death = new_death) %>% 
  filter(date == max(date)) %>% 
  mutate(new_case = as.numeric(new_case),
         new_death = as.numeric(new_death),
         incidence_rate = new_case/pop_denominator)

###########
# The map

spdf = rgdal::readOGR("./Geography-resources/MODZCTA_2010_WGS1984.geo.json")

choices = c("Cumulative Cases Count", "Death Count", "Positive Cases Rate", "Death Rate","New cases")

### write the functions to draw the map
###positive
positive = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  #data_to_plot = subset(data_to_plot, !is.na(covid_case_count))
  pal <- colorNumeric("Greens", domain=data_to_plot$covid_case_count)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot$modified_zcta),
                     "<br>", 
                     "Total Number of Cumulative Positive Cases: ", as.character(data_to_plot$covid_case_count)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot$covid_case_count), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot$covid_case_count, 
              position = "bottomright", 
              title = "Number")
  p1
}
### case rate
case_rate = function(date){ 
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot = subset(data_to_plot, !is.na(covid_case_rate))
  pal <- colorNumeric("Greens", domain=data_to_plot$covid_case_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot$modified_zcta),
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
  
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot = subset(data_to_plot, !is.na(covid_death_count))
  pal <- colorNumeric("Reds", domain=data_to_plot$covid_death_count)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot$modified_zcta),
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
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot = subset(data_to_plot, !is.na(covid_death_rate))
  pal <- colorNumeric("Reds", domain=data_to_plot$covid_death_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot$modified_zcta),
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
  
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date)) %>% 
    mutate(new_case = as.numeric(new_case))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot = subset(data_to_plot, !is.na(new_case))
  pal <- colorNumeric("Greens", domain=data_to_plot$new_case)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot$modified_zcta),
                     "<br>", 
                     "Total Number of New Cases: ", as.character(data_to_plot$new_case)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot$new_case), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot$new_case, 
              position = "bottomright", 
              title = "Number")
  p1
}



## ui
ui <- tagList(
  #shinythemes::themeSelector(),
  navbarPage(
    theme = "cosmo",
    "COVID-19",
  
  tabPanel(
  # Application title
  title= "Tracker of COVID-19 in NYC",
  helpText("data update by 2020-07-23"),
  helpText("input the zipcode you interested in in the search box"),
  # Sidebar with a slider input for number of bins 
  fluidRow(
    h4("Some word to describe the table"),
    column(width = 10, align="center",DT::dataTableOutput("table"))
  )
  ),
  
  tabPanel(title = "Distribution of COVID-19 at zipcode level",
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of COVID-19 in NYC."),
      helpText("data update by 2020-07-23"),
      radioButtons(inputId = "outcome_selection",
                   label =  "Outcome:",   
                   c("Cumulative Cases Count" = "positive",
                     "Positive Cases Rate (per 100,000 people)" = "case_rate", 
                     "Death Count" = "death_count", 
                     "Death Rate (per 100,000 people)" = "death_rate",
                     "New cases" = "newcase")
      ),
      
      helpText("Cumulative Case Count is the count of confirmed cases since the first case occured in NYC."),
      helpText("Positive Case Rate is the rate of confirmed cases per 100,000 people by ZCTA."),
      helpText("Population denominators for ZCTAs derived from intercensal estimates by the Bureau of Epidemiology Services"),
      helpText("Death Count is the count of confirmed deaths"),
      helpText("Death Rate is the rate of confirmed deaths per 100,000 people by ZCTA")
      
      
      
      
      
      
      
      
    ),
    
    
    mainPanel(
      fluidRow(column(12,
                      leafletOutput(outputId = "map",width="100%",height="600px")),
               
               column(12,
                      tableOutput(outputId = "descriptive"))
      ))))
      
  
))


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data_to_table
  },rownames = FALSE))
  
  
  output$map = renderLeaflet({
    
    plot = switch (input$outcome_selection,
                   positive = positive,
                   death_count = death_count,
                   case_rate = case_rate,
                   death_rate = death_rate,
                   newcase = newcase
    )
    
    plot(input$date_choice)
  })
  
  
}

shinyApp(ui, server)




