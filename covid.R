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
#June data
Junedata = read.csv("./data/Junedata.csv")


#input the widges
plot_date = Junedata %>% distinct(day) %>% pull()


#join the data
Map_data = geo_join(spdf,Junedata,"MODZCTA","zipcode")

# Setting up the pop up text
popup_sb <- paste0("Positive Cases: ", as.character(Map_data$positive))

pal <- colorNumeric("Greens", domain=Map_data$positive)


popup_dea_sb <- paste0("Cumulative Cases Count", as.character(Map_data$positive),
                       "<br>",
                       "Death Count: ", as.character(Map_data$covid_death_count),
                       "<br>", 
                       "Neighborhood Name: ", as.character(Map_data$neighborhood_name),
                       "MODZCTA: ", as.character(Map_data$MODZCTA))

pal_dea <- colorNumeric("Reds", domain=Map_data$covid_death_count)


###Map functions###

# function to plot cumulative COVID cases by date
cumulative_plot = function(outcome, plot_date) {
  data_to_plot = Map_data %>%
    filter(day == input$date_choice) %>% 

  g1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data = data_to_plot , 
                fillColor = ~pal(Map_data$outcome), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = Map_data$outcome, 
              position = "bottomright", 
              title = "Number")

  g1
}




# function to plot new COVID cases by date
new_cases_plot = function(cv_aggregated, plot_date) {
  plot_df_new = subset(cv_aggregated, date<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = date, y = new, fill = region)) + 
    geom_bar(position="stack", stat="identity") + 
    ylab("new cases") + theme_bw() + 
    scale_fill_manual(values=c(covid_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 1000; paste0(trans, "K")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}


##########
ui =  fluidPage(
  titlePanel("COVID-19 in NYC"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Create maps for the distribution of Cumulative Positive cases in NYC."),
      
      selectInput("date_choice", 
                  label = h3("Select date"),
                  choices = plot_date),
      
      pickerInput("outcome_select", "Outcome:",   
                  choices = c("Cumulative Cases Count", "Death Count", "Positive Cases Rate", "Death Rate","New cases"), 
                  selected = c("Cumulative Cases Count"),
                  multiple = FALSE)
    ),
    
    
    mainPanel(
      fluidRow(leafletOutput(outputId = "cumulative_plot")

      
    )
  )))




server = function(input, output,session) {

    output$cumulative_plot <- renderPlot({
      cumulative_plot(input$outcome, input$plot_date)
    })
    
    
}



shinyApp(ui, server)
