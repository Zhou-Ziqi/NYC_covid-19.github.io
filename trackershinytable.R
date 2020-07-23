library(tidyverse)
library(shiny)
library(DT)
library(readxl)

#read the data


data_yester = read_csv("./data/data_for_table/data-by-modzcta0710.csv") %>% 
  mutate(date = as.Date("2020-07-10"))
data_today = read_csv("./data/data_for_table/data-by-modzcta0711.csv") %>% 
  mutate(date = as.Date("2020-07-11"))


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

data = data %>% 
  mutate(new_case = new_case,
         new_death = new_death) %>% 
  filter(date == max(date)) %>% 
  dplyr::select(MODIFIED_ZCTA,NEIGHBORHOOD_NAME,BOROUGH_GROUP,
                COVID_CASE_COUNT,new_case,
                COVID_DEATH_COUNT,new_death) %>% 
  mutate(new_case = as.numeric(new_case),
         new_death = as.numeric(new_death)) %>% 
  rename("Incremental Positive Cases" = new_case,
         "Incremental Death Cases" = new_death,
         Zipcode = MODIFIED_ZCTA,
         "Neighborhood" = NEIGHBORHOOD_NAME,
         "Borough"= BOROUGH_GROUP,
         "Cumulative Positive Case Count" = COVID_CASE_COUNT,
         "Cumulative Death Count" = COVID_DEATH_COUNT)

ui <- fluidPage(
  
  # Application title
  titlePanel("Tracker of COVID-19 in NYC"),
  helpText("data update by 2020-07-11"),
  helpText("input the zipcode you interested in in the search box"),
  # Sidebar with a slider input for number of bins 
  fluidRow(
    h4("Some word to describe the table"),
   column(width = 10, align="center",DT::dataTableOutput("table"))
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$table <- DT::renderDataTable(DT::datatable({
    data 
  },rownames = FALSE))
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

