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

library(flexdashboard)

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



######
#demographics
byage = read_csv("./distribution_of_covid-19/data/demoage_data.csv") 

byrace = read_csv("./distribution_of_covid-19/data/BYRACE_demoage_data.csv")

bysex = read_csv("./distribution_of_covid-19/data/demoage_data_sex.csv")


# Define UI for application that draws plot

outcome_age = byage %>% distinct(outcome) %>% pull()
outcome_race = byrace %>% distinct(outcome) %>% pull()
outcome_sex = bysex %>% distinct(outcome) %>% pull()


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
ui <- #shinythemes::themeSelector(),
  navbarPage(
    
    title = div(img(src='logo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 60)),
    windowTitle = "NYC covid-19 dashboard",
    id = 'menus',
    tabPanel('Home',
             shinyjs::useShinyjs()),

  
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
  
  tabPanel(
    title = "Distribution of COVID-19 at zipcode level",
    h2("If here needs a title"),
    fluidRow(column(12, "This part is for instructions")),
        
    sidebarLayout(
      sidebarPanel(
      helpText("Create maps for the distribution of COVID-19 in NYC."),
      radioButtons(inputId = "outcome_selection",
                                    label =  "Outcome:",   
                                    c("Cumulative Cases Count" = "positive",
                                      "Positive Cases Rate (per 100,000 people)" = "case_rate", 
                                      "Death Count" = "death_count", 
                                      "Death Rate (per 100,000 people)" = "death_rate",
                                      "New cases" = "newcase")),
      
      
      helpText("Create maps for the distribution of COVID-19 in NYC."),
      helpText("data update by 2020-07-23"),
      
      
      helpText("Cumulative Case Count is the count of confirmed cases since the first case occured in NYC."),
      helpText("Positive Case Rate is the rate of confirmed cases per 100,000 people by ZCTA."),
      helpText("Population denominators for ZCTAs derived from intercensal estimates by the Bureau of Epidemiology Services"),
      helpText("Death Count is the count of confirmed deaths"),
      helpText("Death Rate is the rate of confirmed deaths per 100,000 people by ZCTA"))
    
      ,
      
      mainPanel(fluidRow(column(12,leafletOutput(outputId = "map",width="95%",height="550px"))))
      
      ),
      
    h3(),
    fluidPage(column(12,
                      tableOutput(outputId = "descriptive")),
              column(4, 
                     selectInput("outcome_age", 
                                 label = "Choose an Outcome", 
                                 choices = outcome_age
                     )),
              column(12,
                      plotlyOutput(outputId = "piechart_age")),
              br(),
              column(12,
                      plotlyOutput(outputId = "barchart_age", width = "100%")),
              column(12, 
                     helpText(paste0("Age data updated by ",as.character(max(byage$day))))),
            
              column(4, 
                     selectInput("outcome_sex", 
                                 label = "Choose an Outcome", 
                                 choices = outcome_sex
                     )),
              column(12,plotlyOutput(outputId = "piechart_sex")),
              column(12,
                      plotlyOutput(outputId = "barchart_sex", width = "100%")),
              
              column(12,
                     helpText(paste0("Sex data updated by ",as.character(max(bysex$day))))),
              
              column(4, 
                     selectInput("outcome_race", 
                                 label = "Choose an Outcome", 
                                 choices = outcome_race
                     )),
              column(12,plotlyOutput(outputId = "piechart_race")),
              column(12,
                      plotlyOutput(outputId = "barchart_race", width = "100%")),
              column(12,
                     helpText(paste0("Race data updated by ",as.character(max(byrace$day)))))
      )
    ),
      
  tabPanel("Distribution"),
  tabPanel("Time Trend"),
  tabPanel("Demographics"),
  tabPanel("Comorbidity"),
  tabPanel("About")
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyjs::addClass(id = "menus", class = "navbar-right")
  
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
  
  output$barchart_age = renderPlotly({
    
    a =  byage %>% filter(day == max(byage$day) & group != "Boroughwide") %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      facet_grid(~outcome)
    
    ggplotly(a)
    
  })
  
  output$barchart_sex = renderPlotly({
    
    b =  bysex %>%filter(day == max(bysex$day) & group != "Boroughwide") %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      facet_grid(~outcome)
    
    ggplotly(b)
  })
  
  
  output$barchart_race = renderPlotly({
    
    c =  byrace %>%  filter(day == max(byrace$day) & group != "Boroughwide") %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      facet_grid(~outcome)
    
    ggplotly(c)
  })
  
  output$piechart_age = renderPlotly({
    
    pie1data = byage %>% 
      filter(boro == "BX"& day == max(byage$day) &group != "Boroughwide" & outcome ==input$outcome_age)
    pie2data = byage %>% 
      filter(boro == "MN"& day == max(byage$day) &group != "Boroughwide" & outcome ==input$outcome_age)
    pie3data = byage %>% 
      filter(boro == "BK"& day == max(byage$day) &group != "Boroughwide" & outcome ==input$outcome_age)
    pie4data = byage %>% 
      filter(boro == "QN"& day == max(byage$day) &group != "Boroughwide" & outcome ==input$outcome_age)
    pie5data = byage %>% 
      filter(boro == "SI"& day == max(byage$day) &group != "Boroughwide" & outcome ==input$outcome_age)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = pie1data %>% select(group,count), 
                           labels = ~paste0("Age Group: ", pie1data$group), values = ~count,
                           name =  pie1data$boro, domain = list(row = 0, column = 0))
    fig <- fig %>% add_pie(data = pie2data %>% select(group,count), 
                           labels = ~paste0("Age Group: ", pie2data$group), values = ~count,
                           name =  pie2data$boro, domain = list(row = 0, column = 1)) 
    fig <- fig %>% add_pie(data = pie3data %>% select(group,count), 
                           labels = ~paste0("Age Group: ", pie3data$group), values = ~count,
                           name = pie3data$boro, domain = list(row = 0, column = 2)) 
    fig <- fig %>% add_pie(data = pie4data %>% select(group,count), 
                           labels = ~paste0("Age Group: ", pie4data$group), 
                           values = ~count,
                           name = pie4data$boro, domain = list(row = 0, column = 3)) 
    fig <- fig %>% add_pie(data = pie5data %>% select(group,count), 
                           labels = ~paste0("Age Group: ", pie5data$group), 
                           values = ~count,
                           name = pie5data$boro, domain = list(row = 0, column = 4)) 
    fig <- fig %>% layout(title = "Age Group Pie Chart", showlegend = T,
                          grid=list(rows=1, columns=5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
    
    fig
  })
  
  output$piechart_sex = renderPlotly({
    
    pie1data = bysex %>% 
      filter(boro == "BX"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie2data = bysex %>% 
      filter(boro == "MN"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie3data = bysex %>% 
      filter(boro == "BK"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie4data = bysex %>% 
      filter(boro == "QN"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie5data = bysex %>% 
      filter(boro == "SI"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = pie1data %>% select(group,count), 
                           labels = ~paste0("Gender: ", pie1data$group), values = ~count,
                           name =  pie1data$boro, domain = list(row = 0, column = 0))
    fig <- fig %>% add_pie(data = pie2data %>% select(group,count), 
                           labels = ~paste0("Gender: ", pie2data$group), values = ~count,
                           name =  pie2data$boro, domain = list(row = 0, column = 1)) 
    fig <- fig %>% add_pie(data = pie3data %>% select(group,count), 
                           labels = ~paste0("Gender: ", pie3data$group), values = ~count,
                           name = pie3data$boro, domain = list(row = 0, column = 2)) 
    fig <- fig %>% add_pie(data = pie4data %>% select(group,count), 
                           labels = ~paste0("Gender: ", pie4data$group), 
                           values = ~count,
                           name = pie4data$boro, domain = list(row = 0, column = 3)) 
    fig <- fig %>% add_pie(data = pie5data %>% select(group,count), 
                           labels = ~paste0("Gender: ", pie5data$group), 
                           values = ~count,
                           name = pie5data$boro, domain = list(row = 0, column = 4)) 
    fig <- fig %>% layout(title = "Gender Group Pie Charts", showlegend = T,
                          grid=list(rows=1, columns=5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
    
    fig
  })
  
  
  output$piechart_race = renderPlotly({
    
    pie1data = byrace %>% 
      filter(boro == "BX"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)
    pie2data = byrace %>% 
      filter(boro == "MN"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)
    pie3data = byrace %>% 
      filter(boro == "BK"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)
    pie4data = byrace %>% 
      filter(boro == "QN"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)
    pie5data = byrace %>% 
      filter(boro == "SI"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)
    
    fig <- plot_ly()
    fig <- fig %>% add_pie(data = pie1data %>% select(group,count), 
                           labels = ~paste0("Race: ", pie1data$group), values = ~count,
                           name =  pie1data$boro, domain = list(row = 0, column = 0))
    fig <- fig %>% add_pie(data = pie2data %>% select(group,count), 
                           labels = ~paste0("Race: ", pie2data$group), values = ~count,
                           name =  pie2data$boro, domain = list(row = 0, column = 1)) 
    fig <- fig %>% add_pie(data = pie3data %>% select(group,count), 
                           labels = ~paste0("Race: ", pie3data$group), values = ~count,
                           name = pie3data$boro, domain = list(row = 0, column = 2)) 
    fig <- fig %>% add_pie(data = pie4data %>% select(group,count), 
                           labels = ~paste0("Race: ", pie4data$group), 
                           values = ~count,
                           name = pie4data$boro, domain = list(row = 0, column = 3)) 
    fig <- fig %>% add_pie(data = pie5data %>% select(group,count), 
                           labels = ~paste0("Race: ", pie5data$group), 
                           values = ~count,
                           name = pie5data$boro, domain = list(row = 0, column = 4)) 
    fig <- fig %>% layout(title = "Race Group Pie Charts", showlegend = T,
                          grid=list(rows=1, columns=5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F))
    
    fig
  })
  
}

shinyApp(ui, server)




