#For the whole website

# Recently COVID-19 in NYC tracker
library(tidyverse)
library(shiny)
library(DT)
library(patchwork)

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
library(scales)

library(flexdashboard)
library(RColorBrewer)
library(grid)


library(Dict)

##
url1 = "https://twitter.com/intent/tweet?text=Hello%20world&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url2 = "https://www.facebook.com/sharer.php?u=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url3 = "https://www.instagram.com/columbiapublichealth/"
url4 = "https://www.linkedin.com/shareArticle?mini=true&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title=&summary=&source="
url5 = "mailto:info@example.com?&subject=&body=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url6 = "whatsapp://send?text=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url7 = "https://service.weibo.com/share/share.php?url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title="
##read data
##Home page data

df = read_csv("data/data-yqfdF.csv") %>% 
  janitor::clean_names() %>% 
  rename(date = date_of_interest) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))
N = nrow(df)
cases = pull(df,cases)
deaths = pull(df,deaths)
ave.cases = rep(0, N-6)
ave.deaths = rep(0, N-6)

for (i in 7:N) {
  ave.cases[i] = mean(cases[(i-6):(i)])
  ave.deaths[i] = mean(deaths[(i-6):(i)])
}

df.ave = df %>% 
  mutate(ave_cases = round(ave.cases), ave_deaths = round(ave.deaths))

### trakcer data
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
                covid_death_count,new_death, total_covid_tests) %>% 
  rename("New Cases" = new_case,
         "New Deaths" = new_death,
         Zipcode = modified_zcta,
         "Neighborhood" = neighborhood_name,
         "Borough"= borough_group,
         "Case Count" = covid_case_count,
         "Death Count" = covid_death_count,
         "Incidence Rate (Per 100,000 people)" = incidence_rate,
         "Total COVID-19 Tests" = total_covid_tests )

data_to_plot = data %>% 
  mutate(new_case = new_case,
         new_death = new_death
  ) %>% 
  filter(date == max(date)) %>% 
  mutate(new_case = as.numeric(new_case),
         new_death = as.numeric(new_death),
         incidence_rate = round(new_case*100000/pop_denominator, digits = 1) )

###########
# The map

spdf = rgdal::readOGR("./Geography-resources/MODZCTA_2010_WGS1984.geo.json")

choices = c("Cases Count", "Death Count", "Cases Rate (per 100,000 people)", "Death Rate(per 100,000 people)","New cases")



######
#demographics
byage = read_csv("./distribution_of_covid-19/data/demoage_data.csv") %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Case Count"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Hospitalization Count"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Death Count"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count))


byrace = read_csv("./distribution_of_covid-19/data/BYRACE_demoage_data.csv") %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Case Count"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Hospitalization Count"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Death Count"),
         outcome = str_replace_all(outcome, "CASE_RATE_ADJ","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE_ADJ","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE_ADJ","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count)) %>% 
  mutate(group = factor(group, levels = c("White","Black/African-American","Asian/Pacific-Islander","Hispanic/Latino")))


bysex = read_csv("./distribution_of_covid-19/data/demoage_data_sex.csv") %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Case Count"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Hospitalization Count"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Death Count"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count))



# Define UI for application that draws plot

outcome_age = byage %>% distinct(outcome) %>% pull()
outcome_race = byrace %>% distinct(outcome) %>% pull()
outcome_sex = bysex %>% distinct(outcome) %>% pull()


data_by_modzcta = read_csv("./data/data-by-modzcta.csv") %>% 
  select(1:3) %>% 
  janitor::clean_names() %>% 
  rename(zipcode = modified_zcta)


sex_age_raw = read_csv("./data/predictor_data/sex_by_age_zipcode_nyc.csv") %>% select(-1)
sex_age = left_join(sex_age_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything())

income_raw = read_csv("./data/predictor_data/median_income_nyc.csv") %>% select(-1)
income = left_join(income_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything())

household_raw = read_csv("./data/predictor_data/household_zipcode_nyc.csv") %>% select(-1)
household = left_join(household_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything()) %>% 
  rename(size_2 = person_2,
         size_1 = person_1,
         size_3 = person_3,
         size_4 = person_4,
         size_5 = person_5,
         size_6 = person_6,
         size_7_or_more = person_7_or_more)

race_raw = read_csv("./data/predictor_data/race_by_zipcode_nyc.csv") %>% select(-1)
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

###Time Trend

finalMaydata <- read_csv("data/finalMaydata.csv") %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,newcases) %>% 
  mutate(day = as.Date(day,format = "%m/%d/%y"))

final_Junedata <- read_csv("data/final_Junedata.csv") %>% 
  dplyr::rename(newcases = newcases_june) %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,newcases)

Aprildata_with_nebhod <- read_csv("data/Aprildata_with_nebhod.csv")

data <- rbind(finalMaydata,final_Junedata)
Week <- unique(as.Date(cut(data$day, "week")) + 6)

weeklydf_ <- data %>% 
  mutate(zipcode = factor(zipcode)) %>% 
  filter(day %in% Week)


weeklydf_max <- weeklydf_ %>%
  filter(day == max(day)) %>% select(zipcode,neighborhood_name)

weeklydf_ <- weeklydf_ %>% rename(nbh = neighborhood_name) %>% select(-zipcode)

weeklydf <-cbind(weeklydf_max, weeklydf_) 

weeklydf$zipcode_new <- paste(weeklydf$zipcode, weeklydf$neighborhood_name)

zip_nbh <- weeklydf %>% pull(zipcode_new) %>% unique()

data$week <- as.Date(cut(data$day, "week")) + 6
weeklynew <- aggregate(data$newcases, by=list(week=data$week, zipcode = data$zipcode), FUN=sum)
weeklynew <- weeklynew %>% rename(new_cases = x) %>% mutate(zipcode = factor(zipcode))

weeklynew <- left_join(weeklynew, weeklydf_max)
weeklynew$zipcode_new <- paste(weeklynew$zipcode, weeklynew$neighborhood_name)



# boro time trend written on Aug 12
borocase_new <- read_csv("data/boro_newcase_trend.csv") %>% select(-1)
borocase_cum <- read_csv("data/boro_cumcase_trend.csv") %>% select(-1)
week <- as.Date(cut(borocase_cum$date_of_interest, "week")) + 6

weeklydf_new <- borocase_new %>% 
  mutate(week = week) %>% 
  group_by(boro,week) %>% 
  summarise(case_count = sum(case_count),
            hospitalization_count = sum(hospitalized_count),
            death_count = sum(death_count)) %>% 
  pivot_longer(case_count:death_count,
               names_to = "type",
               values_to = "count") %>% 
  mutate(type = str_to_title(str_replace_all(type, "_", " ")),
         newtype = str_replace_all(paste(type, "Rate"),"Count ",""),
         rate = round(count/8394355*1000000,1)) %>% 
  rename(Borough = boro,
         Date = week,
         Count = count,
         Rate = rate)




weeklydf_cum <- borocase_cum %>% 
  mutate(boro = factor(boro)) %>% 
  filter(date_of_interest %in% week) %>%
  rename(cum_hospitalization_count = cum_hospitalized_count) %>% 
  pivot_longer(cum_case_count:cum_death_count,
               names_to = "type",
               values_to = "count") %>% 
  mutate(type = str_replace_all(type, "cum_", ""),
         type = str_to_title(str_replace_all(type, "_", " ")),
         newtype = str_replace_all(paste(type, "Rate"),"Count ",""),
         rate = round(count/8394355*1000000,1)) %>% 
  rename(Borough = boro,
         Date = date_of_interest,
         Count = count,
         Rate = rate)



cum_case <- function(){
  temp1 <- weeklydf_cum %>% 
    ggplot(aes(x = Date, y = Count)) + 
    geom_line(aes(color = Borough)) +
    geom_point(aes(color = Borough)) +
    facet_wrap(.~type, scales = "free") +
    theme_minimal() +
    theme(legend.position = "none")+
    #theme(panel.spacing.y=unit(2, "lines")) + 
    xlab("") + 
    ylab("")
  
  temp3 <- weeklydf_cum %>% 
    ggplot(aes(x = Date, y = Rate)) + 
    geom_line(aes(color = Borough)) +
    geom_point(aes(color = Borough)) +
    facet_wrap(.~newtype, scales = "free", strip.position="bottom") +
    theme_minimal() +
    #theme(panel.spacing.y=unit(2, "lines")) + 
    xlab("") + 
    ylab("")
  
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 1, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  
  
  subplot(ggplotly(temp1, height = 800), ggplotly(temp3, height = 800), nrows = 2) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
           #shapes = list(vline(as.Date("2020-03-01")), vline(as.Date("2020-03-20")),vline(as.Date("2020-06-22")),vline(as.Date("2020-07-06")),vline(as.Date("2020-07-20"))),
           #xaxis = list(title = "",type = "date"),
           yaxis = list(title = ""),
           margin = list(b=100))
  
  # ggplotly(temp1) %>%
  #   layout(legend = list(orientation = "h", x = 0.4, y = -0.2)
  #          #hovermode = "x unified",
  #          #xaxis = list(spikemode = "across",
  #          #             spikedash = "dash"),
  #          #hoverlabel = list(font = list(size = 10))
  #          )
  # 
  # ggplotly(temp3) %>%
  #   layout(legend = list(orientation = "h", x = 0.4, y = -0.2)
  #          #hovermode = "x unified",
  #          #xaxis = list(spikemode = "across",
  #          #             spikedash = "dash"),
  #          #hoverlabel = list(font = list(size = 10))
  #          )
}

# weeklydf_new_positive <- weeklydf_new %>% filter(type == "Case Count")
# weeklydf_new_hos <- weeklydf_new %>% filter(type == "Hospitalization Count")
# weeklydf_new_death <- weeklydf_new %>% filter(type == "Death Count")

new_case <- function(){
  
  # fig <- plot_ly()
  # fig <- fig %>% add_trace(weeklydf_new_positive, 
  #                x = ~Date, y = ~Count, 
  #                linetype = ~Borough, 
  #                mode = 'lines', 
  #                name = weeklydf_new_positive$type, 
  #                domain = list(row = 0, column = 0))
  # 
  # fig <- fig %>% add_trace(weeklydf_new_hos, 
  #                          x = ~Date, y = ~Count, 
  #                          linetype = ~Borough, 
  #                          mode = 'lines', 
  #                          name = weeklydf_new_hos$type, 
  #                          domain = list(row = 0, column = 1))
  # fig <- fig %>% add_trace(weeklydf_new_death, 
  #                          x = ~Date, y = ~Count, 
  #                          linetype = ~Borough, 
  #                          mode = 'lines', 
  #                          name =  weeklydf_new_death$type, 
  #                          domain = list(row = 0, column = 2))
  
  temp2 <- weeklydf_new %>%
    ggplot(aes(x = Date, y = Count)) +
    geom_line(aes(color = Borough)) +
    geom_point(aes(color = Borough)) +
    facet_wrap(.~type, scales = "free") +
    theme(panel.spacing.y=unit(3, "lines")) + 
    theme_minimal() +
    xlab("") +
    ylab("")
  
  temp4 <- weeklydf_new %>%
    ggplot(aes(x = Date, y = Rate)) +
    geom_line(aes(color = Borough)) +
    geom_point(aes(color = Borough)) +
    facet_wrap(.~newtype, scales = "free") +
    theme(panel.spacing.y=unit(3, "lines")) + 
    theme_minimal() +
    xlab("") +
    ylab("")
  
  subplot(ggplotly(temp2, height = 800),ggplotly(temp4, height = 800), nrows = 2) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  
  
  # ggplotly(temp2, height = 800) %>% 
  #   layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
  #          grid=list(rows=1, columns=3),
  #          hovermode = "x unified",
  #          xaxis = list(spikemode = "across",
  #                       spikedash = "dash"),
  #          hoverlabel = list(font = list(size = 10)))
}




### write the functions to draw the map
###positive
positive = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(covid_case_count))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$covid_case_count)
  
  popup_sb <- paste0("<b> Neighborhood Name: </b>", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "<b> MODZCTA:</b> ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "<b>Total Number of Case Count: </b>", as.character(data_to_plot_geo$covid_case_count)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_case_count), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$covid_case_count, 
              position = "bottomright", 
              title = "Number") 
  
  p1
}
### case rate
case_rate = function(date){ 
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(covid_case_rate))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$covid_case_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Case Rate of COVID-19: ", as.character(data_to_plot_geo$covid_case_rate)
  )
  
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_case_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$covid_case_rate, 
              position = "bottomright", 
              title = "Number")
  
}
##death count
death_count = function(date){
  
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(covid_death_count))
  pal <- colorNumeric("Reds", domain=data_to_plot_geo$covid_death_count)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Total Number of Death: ", as.character(data_to_plot_geo$covid_death_count)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_death_count), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values = data_to_plot_geo$covid_death_count, 
              position = "bottomright", 
              title = "Number")
  p1
}
##death rate
death_rate = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(covid_death_rate))
  pal <- colorNumeric("Reds", domain=data_to_plot_geo$covid_death_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Death Rate: ", as.character(data_to_plot_geo$covid_death_rate)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$covid_death_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$covid_death_rate, 
              position = "bottomright", 
              title = "Number")
  p1
}
###new cases
newcase = function(date){
  
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date)) %>% 
    mutate(new_case = as.numeric(new_case))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(new_case))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$new_case)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Total Number of New Cases: ", as.character(data_to_plot_geo$new_case)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$new_case), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$new_case, 
              position = "bottomright", 
              title = "Number")
  p1
}

### incidence rate

incidencerate = function(date){
  
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date)) %>% 
    mutate(incidence_rate = as.numeric(incidence_rate))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(incidence_rate))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$incidence_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Total Number of New Cases: ", as.character(data_to_plot_geo$incidence_rate)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$incidence_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$incidence_rate, 
              position = "bottomright", 
              title = "Number")
  p1
}


## ui
ui <- navbarPage(
  theme = "shiny.css",
  title = div(img(src='whitelogo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 50)),
  windowTitle = "NYC covid-19 dashboard",
  id = 'menus',
  tabPanel('Home',
           shinyjs::useShinyjs(),
           fluidRow(
             column(width = 5, offset = 1, div(img(src = "HomePagepic 2020-08-12 .png", height = "100%",width = "100%"),
                                               style="text-align: center;")),
             
             column(width = 5,  div(img(src = "newlogo3.png", height = "100%",width = "90%"),
                                    style="text-align: center;"))),
           br(),
           fluidRow(column(width = 10, offset = 1, span(htmlOutput("Hometext"), style="font-size: 15px;line-height:150%"))),
           br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding:40px;"
                    
           )
           
  ),
  
  
  tabPanel(
    # Application title
    title= "COVID-19 Tracker",
    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(width = 10, offset = 1, h2("COVID-19 Tracking")),
      column(width = 10, offset = 1, span(htmlOutput("Trackertext"), style="font-size: 15px; line-height:150%")),
      column(width = 10, offset = 1, align="center",DT::dataTableOutput("table")),
      column(width = 10, offset = 1, helpText("Last updated : 2020-07-23")),
      column(width = 10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data"))
    ),
    br(),
    fluidRow(align="center",
             span(htmlOutput("bannertext1", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
             #span(htmlOutput("sharetext", style="color:white")),
             #br(),
             #img(src='bottomlogo.png', height="20%", width="20%"),
             h5("Share on", style="color:white;font-size:12px"),
             actionButton("twitter_index",
                          label = "",
                          icon = icon("twitter"),
                          onclick = sprintf("window.open('%s')", url1),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("fb_index",
                          label = "",
                          icon = icon("facebook"),
                          onclick = sprintf("window.open('%s')", url2),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             #actionButton("ins_index",
             #             label = "",
             #             icon = icon("instagram"),
             #             onclick = sprintf("window.open('%s')", url3),
             #             style = "border-color: #FFFFFF;"),
             actionButton("linkedin_index",
                          label = "",
                          icon = icon("linkedin"),
                          onclick = sprintf("window.open('%s')", url4),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("whats_index",
                          label = "",
                          icon = icon("whatsapp"),
                          onclick = sprintf("window.open('%s')", url6),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("email_index",
                          label = "",
                          icon = icon("envelope"),
                          onclick = sprintf("window.open('%s')", url5),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             style = "background-color:#225091;padding-top:40px;padding:40px;"
             
    )
  ),
  
  tabPanel(
    title = "COVID-19 Distribution",
    column(width = 10, offset = 1, h2("COVID-19 Data by Neighborhoods and Demographics")),
    column(width = 10, offset = 1, span(htmlOutput("Distributionmaptext"), 
                                        style="font-size: 15px;  line-height:150%")),
    column(width = 10,offset = 1,
           sidebarLayout(
             
             sidebarPanel(
               radioButtons(inputId = "outcome_selection",
                            label =  "Data Display:",   
                            c("Case Count" = "positive",
                              "Case Rate (per 100,000 people)" = "case_rate", 
                              "Death Count" = "death_count", 
                              "Death Rate (per 100,000 people)" = "death_rate",
                              "New Cases" = "newcase",
                              "Incidence Rate (per 100,000 people)" = "incidencerate")),
               
               helpText("data update by 2020-07-23"),
               span(htmlOutput("Distributionmap_help_text"), 
                    style="font-size: 14px;line-height:150% ; color:grey")
             )
             ,
             
             mainPanel(column(10,leafletOutput(outputId = "map",width="120%",height="465px"))),
             position = c("left","right")
           )),
    
    br(),
    fluidPage(
      column(10, offset = 1, span(htmlOutput("DistribAgetext"), style="font-size: 15px; line-height:150%")),
      
      column(10, offset = 1,
             plotlyOutput(outputId = "barchart_age", width = "100%",height = "100%")),
      br(),
      column(4, offset = 1,
             selectInput("outcome_age", 
                         label = "Data Display", 
                         choices = outcome_age
             )),
      column(10,offset = 1,
             plotlyOutput(outputId = "piechart_age")),
      column(10, offset = 1,
             helpText(paste0("Age data updated by ",as.character(max(byage$day))))),
      br(),
      column(10, offset = 1, span(htmlOutput("DistribSextext"), style="font-size: 15px; line-height:150%")),
      
      
      column(10,offset = 1,
             plotlyOutput(outputId = "barchart_sex", width = "100%",height = "100%")),
      br(),
      column(4, offset = 1,
             selectInput("outcome_sex", 
                         label = "Data Display", 
                         choices = outcome_sex
             )),
      column(10,offset = 1,
             plotlyOutput(outputId = "piechart_sex")),
      
      column(10,offset = 1,
             helpText(paste0("Sex data updated by ",as.character(max(bysex$day))))),
      br(),
      column(10, offset = 1, span(htmlOutput("DistribRacetext"), style="font-size: 15px; line-height:150%")),
      
      column(10,offset = 1,
             plotlyOutput(outputId = "barchart_race", width = "100%",height = "100%")),
      br(),
      column(4, offset = 1,
             selectInput("outcome_race", 
                         label = "Data Display", 
                         choices = outcome_race
             )),
      column(10,offset = 1,
             plotlyOutput(outputId = "piechart_race")),
      column(10,offset = 1,
             helpText(paste0("Race data updated by ",as.character(max(byrace$day))))),
      column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data"))
    ),
    br(),
    fluidRow(align="center",
             span(htmlOutput("bannertext2", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
             #span(htmlOutput("sharetext", style="color:white")),
             #br(),
             #img(src='bottomlogo.png', height="20%", width="20%"),
             h5("Share on", style="color:white;font-size:12px"),
             actionButton("twitter_index",
                          label = "",
                          icon = icon("twitter"),
                          onclick = sprintf("window.open('%s')", url1),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("fb_index",
                          label = "",
                          icon = icon("facebook"),
                          onclick = sprintf("window.open('%s')", url2),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             #actionButton("ins_index",
             #             label = "",
             #             icon = icon("instagram"),
             #             onclick = sprintf("window.open('%s')", url3),
             #             style = "border-color: #FFFFFF;"),
             actionButton("linkedin_index",
                          label = "",
                          icon = icon("linkedin"),
                          onclick = sprintf("window.open('%s')", url4),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("whats_index",
                          label = "",
                          icon = icon("whatsapp"),
                          onclick = sprintf("window.open('%s')", url6),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             actionButton("email_index",
                          label = "",
                          icon = icon("envelope"),
                          onclick = sprintf("window.open('%s')", url5),
                          style = "border-color: #225091;color: #fff; background-color: #225091;"),
             style = "background-color:#225091;padding-top:40px;padding:40px;"
             
    )
  ),
  tabPanel(title = "COVID-19 Trends",
           fluidRow(column(10, offset = 1, h2("COVID-19 Trends"))),
           br(),
           fluidRow(column(width = 4,offset = 1,
                           radioButtons(inputId = "selection",
                                        label =  "Data Display:",   
                                        c("Total Count" = "cum_case",
                                          "Incidence Count" = "new_case"))),
                    column(width = 6, span(htmlOutput("borotrendtext"), style="font-size: 15px; line-height:150%"))),
           fluidRow(column(width = 10, offset = 1, plotlyOutput(outputId = "boro_cases"), div(style = "height:400px;"))),
           fluidRow(column(width = 10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data"))),
           fluidRow(column(width = 10, offset = 1, helpText("Last updated : 2020-08-12"))),
           hr(),
           
           fluidRow(
             column(10, offset = 1, "A look at how COVID-19 case count, case rate, death count, death rate, new cases, and incidence rate change over time in each of the NYC ZIP Code Tabulation Areas (ZCTAs) and by demographics groups. Choose a ZCTA to display the data."),
             column(3, offset = 1, pickerInput("zip1", 
                                               label = "Choose a ZCTA", 
                                               choices =zip_nbh,
                                               selected = zip_nbh[1],
                                               options = list(`actions-box` = TRUE))),
             column(7, plotlyOutput("pocase", width="100%",height="500px"))),
           hr(),
           
           #####
           fluidRow(
             column(width = 4, offset = 1, selectInput("character_timetrend",
                                                       "Data Display",
                                                       c("Case Count" = "pocase", 
                                                         "Death Count" = "death", 
                                                         "Case Rate (per 100,000 people)" = "porate", 
                                                         "Death Rate (per 100,000 people)" = "derate",
                                                         "New cases" = "newcase"
                                                       ),
                                                       selected = NULL)),
             column(width = 5, "this part is for instructions")
           ),
           
           #### Cumulative Cases Count
           conditionalPanel(
             condition = "input.character_timetrend == 'pocase'",
             fluidRow(column(10, offset = 1,h4("Cases Count"))),
             
             fluidRow(
               column(10, offset = 1, plotlyOutput("tt_age_cac", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_sex_cac", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_race_cac", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
           #### Death Count
           conditionalPanel(
             condition = "input.character_timetrend == 'death'",
             fluidRow(column(10, offset = 1, h4("Death Count"))),
             fluidRow(
               column(width = 3, offset = 1, pickerInput("zip2", 
                                                         label = "Choose zipcodes", 
                                                         choices =zip_nbh, 
                                                         selected = zip_nbh[1],
                                                         options = list(`actions-box` = TRUE))),
               column(7, plotlyOutput("death", width="100%",height="500px"))),
             fluidRow(
               column(10, offset = 1, plotlyOutput("tt_age_dec", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_sex_dec", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_race_dec", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
           #### Positive Cases Rate
           conditionalPanel(
             condition = "input.character_timetrend == 'porate'",
             fluidRow(column(10, offset = 1, h4("Cases Rate (per 100,000 people)"))),
             fluidRow(
               column(width = 3, offset = 1,pickerInput("zip3", 
                                                        label = "Choose zipcodes", 
                                                        choices =zip_nbh, 
                                                        selected = zip_nbh[1],
                                                        options = list(`actions-box` = TRUE))),
               column(7, plotlyOutput("porate", width="100%",height="500px"))),
             fluidRow(
               column(10, offset = 1, plotlyOutput("tt_age_carate", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_sex_carate", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_race_carate", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
           #### Death Rate
           conditionalPanel(
             condition = "input.character_timetrend == 'derate'",
             fluidRow(column(10, offset = 1, h4("Death Rate (per 100,000 people)"))),
             fluidRow(
               column(width = 3, offset = 1, pickerInput("zip4", 
                                                         label = "Choose zipcodes", 
                                                         choices =zip_nbh,
                                                         selected = zip_nbh[1],
                                                         options = list(`actions-box` = TRUE))),
               column(7, plotlyOutput("derate", width="100%",height="500px"))),
             fluidRow(
               column(10, offset = 1, plotlyOutput("tt_age_derate", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_sex_derate", width="100%",height="80%")),
               column(10, offset = 1, plotlyOutput("tt_race_derate", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           conditionalPanel(
             condition = "input.character_timetrend == 'newcase'",
             fluidRow(column(10, offset = 1, h4("New cases"))),
             fluidRow(
               column(width = 3, offset = 1, pickerInput("zip5", 
                                                         label = "Choose zipcodes", 
                                                         choices =zip_nbh,
                                                         selected = zip_nbh[1],
                                                         options = list(`actions-box` = TRUE))),
               column(7, plotlyOutput("newcases", width="100%",height="500px")))),
           br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext3", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding:40px;"
                    
           )
  ),
  
  
  tabPanel(title = "Neighborhoods",
           fluidRow(column(10, offset = 1, h2("Neighborhoods Characteristics"))),
           hr(),
           fluidRow(
             column(width = 4, offset = 1, selectInput("character",
                                                       "Data Display (more coming soon)",
                                                       c("Race" = "race",
                                                         "Income" = "income",
                                                         "Household Size" = "house"))),
             column(width = 6, "Select available display options to see and compare neighborhood characteristics using NYC ZIP Code Tabulation Areas (ZCTAs)")
           ),
           hr(),
           
           #### Race
           conditionalPanel(
             condition = "input.character == 'race'",
             
             fluidRow(
               column(width = 4,offset = 1,selectInput("nbhid1", 
                                                       label = "Choose a Neighborhood", 
                                                       choices =nbh_name, 
                                                       selected = NULL)),
               column(width = 6, "Choose a NYC ZCTAs neighborhood. 
               See how the selected neighborhood differs from the entire NYC and the NYC borough it belongs to. 
               "),
               column(width = 10, offset = 1, plotlyOutput("race_nbh",width = "100%"))
             ),
             hr(),
             fluidRow(
               column(width = 4, offset = 1,
                      verticalLayout(
                        column(12, "Use this map to see how the selected neighborhood characteristics vary by NYC ZCTAs."),
                        br(),
                        column(12,"Select the subgroups to display.
                        Choose one or multiple subgroups. 
                        Click on a ZCTA neighborhood on the map to display the data."),
                        hr(),
                        column(12,
                               pickerInput(inputId = "raceid",
                                           label = "Choose a Race",
                                           choices = str_replace_all(race_name, "_", " "),
                                           multiple = TRUE,
                                           selected = str_replace_all(race_name, "_", " ")[1],
                                           options = list(`actions-box` = TRUE)
                               ))
                        
                        )),
               column(width = 6,leafletOutput("race_map", width="100%",height="700px"))),
             column(10, offset = 1, helpText("Data Sources: Census 2010"))
           ),
           
           
           #### Household
           conditionalPanel(
             condition = "input.character == 'house'",
             
             fluidRow(
               column(width = 4, offset = 1,selectInput("nbhid2", 
                                                        label = "Choose a Neighborhood", 
                                                        choices =nbh_name, 
                                                        selected = NULL)),
               column(width = 6, "Choose a NYC ZCTAs neighborhood. 
               See how the selected neighborhood differs from the entire NYC and the NYC borough it belongs to."),
               column(width = 10, offset = 1, plotlyOutput("household_nbh", width="100%"))),
             
             
             hr(),
    
             fluidRow(
               
               
               column(width = 4, offset = 1,
                      verticalLayout(
                        column(12, "Use this map to see how the selected neighborhood characteristics vary by NYC ZCTAs."),
                        br(),
                        column(12,"Select the subgroups to display.
                        Choose one or multiple subgroups. 
                        Click on a ZCTA neighborhood on the map to display the data."),
                        hr(),
                        column(width = 12,
                               pickerInput(inputId = "houseid",
                                           label = "Choose a Household size",
                                           choices = str_replace_all(house_name, "_", " "),
                                           multiple = TRUE,
                                           selected = str_replace_all(house_name, "_", " ")[1],
                                           options = list(`actions-box` = TRUE)))
                        
                        )),
               column(width = 6,leafletOutput("household_map", width="100%",height="700px"))),
             column(10, offset = 1, helpText("Data Sources: Census 2010"))
           ),
           
           #### income
           conditionalPanel(
             condition = "input.character == 'income'",
             
             fluidRow(column(width = 4,offset = 1,selectInput("nbhid3", 
                                                              label = "Choose a Neighbourhood", 
                                                              choices =nbh_name, 
                                                              selected = NULL)),
                      column(6, "Choose a NYC ZCTAs neighborhood. 
                             See how the selected neighborhood differs from the entire NYC and the NYC borough it belongs to."),
                      column(width = 10, offset = 2,
                             plotlyOutput("income_nbh", width="80%",height="600px"))),
             
             hr(),
            
             fluidRow(
               column(4,offset = 1, "Use this map to see how the selected neighborhood characteristics vary by NYC ZCTAs."),
               column(width = 6, leafletOutput("income_map", width="100%",height="700px")),
               
               column(10, offset = 1, helpText("Data Sources: Census 2010"))),
             br(),
             fluidRow(align="center",
                      span(htmlOutput("bannertext4", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                      #span(htmlOutput("sharetext", style="color:white")),
                      #br(),
                      #img(src='bottomlogo.png', height="20%", width="20%"),
                      h5("Share on", style="color:white;font-size:12px"),
                      actionButton("twitter_index",
                                   label = "",
                                   icon = icon("twitter"),
                                   onclick = sprintf("window.open('%s')", url1),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("fb_index",
                                   label = "",
                                   icon = icon("facebook"),
                                   onclick = sprintf("window.open('%s')", url2),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      #actionButton("ins_index",
                      #             label = "",
                      #             icon = icon("instagram"),
                      #             onclick = sprintf("window.open('%s')", url3),
                      #             style = "border-color: #FFFFFF;"),
                      actionButton("linkedin_index",
                                   label = "",
                                   icon = icon("linkedin"),
                                   onclick = sprintf("window.open('%s')", url4),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("whats_index",
                                   label = "",
                                   icon = icon("whatsapp"),
                                   onclick = sprintf("window.open('%s')", url6),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      actionButton("email_index",
                                   label = "",
                                   icon = icon("envelope"),
                                   onclick = sprintf("window.open('%s')", url5),
                                   style = "border-color: #225091;color: #fff; background-color: #225091;"),
                      style = "background-color:#225091;padding-top:40px;padding:40px;"
                      
             )
           )),
  
  tabPanel("About",
           fluidRow(column(10, offset = 1, h2("About Us")),
                    column(10, offset = 1,span(uiOutput("abouttext",style = "font-size: 15px; line-height:150%"))),
                    column(10, offset = 1,span(uiOutput("abouttext2",style = "font-size: 15px; line-height:150%")))),
           br(),
           fluidRow(align="center",
                    span(htmlOutput("bannertext5", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
  letter-spacing: 0.3px;font-size:18px")),
                    #span(htmlOutput("sharetext", style="color:white")),
                    #br(),
                    #img(src='bottomlogo.png', height="20%", width="20%"),
                    h5("Share on", style="color:white;font-size:12px"),
                    actionButton("twitter_index",
                                 label = "",
                                 icon = icon("twitter"),
                                 onclick = sprintf("window.open('%s')", url1),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("fb_index",
                                 label = "",
                                 icon = icon("facebook"),
                                 onclick = sprintf("window.open('%s')", url2),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    #actionButton("ins_index",
                    #             label = "",
                    #             icon = icon("instagram"),
                    #             onclick = sprintf("window.open('%s')", url3),
                    #             style = "border-color: #FFFFFF;"),
                    actionButton("linkedin_index",
                                 label = "",
                                 icon = icon("linkedin"),
                                 onclick = sprintf("window.open('%s')", url4),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("whats_index",
                                 label = "",
                                 icon = icon("whatsapp"),
                                 onclick = sprintf("window.open('%s')", url6),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    actionButton("email_index",
                                 label = "",
                                 icon = icon("envelope"),
                                 onclick = sprintf("window.open('%s')", url5),
                                 style = "border-color: #225091;color: #fff; background-color: #225091;"),
                    style = "background-color:#225091;padding-top:40px;padding:40px;"
                    
           ))
  
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  shinyjs::addClass(id = "menus", class = "navbar-right")
  
  output$bannertext = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$bannertext1 = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$bannertext2 = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$bannertext3 = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$bannertext4 = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$bannertext5 = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$bannertext6 = renderText({
    return(
      "<b> NYC </b> Neighborhoods <b> COVID-19 </b> Dashboard"
    )
  })
  
  output$sharetext = renderText({
    return(
      "<b> Share on </b> "
    )
  })
  
  output$Hometext = renderText({
    return(
      "The NYC Neighborhoods COVID-19 Dashboard is a tracker and data visualization tool to provide continuously updated sources of COVID-19 data for lay public, policymakers and researchers. 
      <b> The COVID-19 Tracker </b> provides daily tracking of the local development for COVID-19 cases, deaths and tests in 177 NYC ZIP Code Tabulation Areas (ZCTAs).
      <b> The COVID-19 Distribution </b> provides a data visualization of COVID-19 case count, case rate, death count, death rate and new cases in NYC ZCTAs and by age, sex and race/ethnicity.
      <b> The COVID-19 Trends </b> shows the time trends for COVID-19 data by neighborhoods and demographics.
      <b> The Neighborhoods </b> shows and compares the neighborhood characteristics of NYC ZCTAs."
      
    )
  })
  
  
  output$Trackertext = renderText({
    return(
      "Tracking daily COVID-19 cases and deaths by <b> NYC ZIP Code Tabulation Areas (ZCTAs) </b>. 
      Data are presented as case count, new cases, incidence rate, death count, and new deaths. 
      <b> Case count and death count </b> are <b> total cumulative numbers </b> of COVID-19 cases and deaths by the updated date. 
      <b> New cases and new deaths </b> are incremental numbers of COVID-19 positive cases and deaths on the updated date. 
      <b> Incidence rate </b> is calculated as new cases divided by ZCTA population size and multiplied by 100,000 and are interpreted as daily new cases per 100,000 people in the ZCTA. 
      The population size is based on the intercensal population estimates from the U.S. Census Bureau and NYC Department of City Planning updated in 2019. 
      
      <br> <br>
      Choose number of records to show in each page or search neighborhoods using the search box. 
      The data can be sorted by case count, new cases, incidence rate, death count, and new deaths.
      <br> <br>"
    )
  })
  output$Distributionmap_help_text = renderText({
    return(
      "<span>&#8226;</span> Case count and death count are total cumulative numbers of COVID-19 cases and deaths by the updated date. 
     <br>
     <span>&#8226;</span>  New cases are incremental number of COVID-19 cases on the updated date. 
     <br>
     <span>&#8226;</span>  Case rate, death rate and incidence rate are calculated using case count, death count and new cases divided by ZCTA population size and multiplied by 100,000 and are interpreted as number of COVID-19 cases and deaths per 100,000 people in the ZCTA. 
    
    "
    )
  })
  
  output$borotrendtext = renderText({
    return(
      "A look at how COVID-19 case count, case rate, death count, death rate, new cases, and incidence rate change over time in each of the NYC ZIP Code Tabulation Areas (ZCTAs) and by demographics groups. Choose a ZCTA to display the data.
 Change “Choose zipcodes” to “Choose a ZCTA”"     
      )
  })
  
  output$Distributionmaptext = renderText({
    return(
      "Use this map to see how COVID-19 cases and deaths vary by NYC ZIP Code Tabulation Areas (ZCTAs). 
      Select available display options to visualize the data. Click a ZCTA on the map to see the data.
      <br> <br>"
    )
  })
  
  
  output$DistribAgetext = renderText({
    return("<br><br>See how COVID-19 cases, hospitalization and deaths differ by age groups and NYC boroughs. 
           The bar charts present counts and rates per 100,000 people. 
           The pie charts show percentage of age groups in each NYC borough.<br>")
    
  })
  
  output$DistribRacetext = renderText({
    return("<br><br>See how COVID-19 cases, hospitalization and deaths differ by race/ethnicity and NYC boroughs. 
           The bar charts present counts and rates per 100,000 people. 
           The pie charts show percentage of race and ethnicity groups in each NYC borough.<br>")
  })
  
  output$DistribSextext = renderText({
    return("<br><br>See how COVID-19 cases, hospitalization and deaths differ by Sex and NYC boroughs. 
    The bar charts present counts and rates per 100,000 people.
    The pie charts show percentage of sex groups in each NYC borough. <br>
")
  })
  
  output$NeighborhoodsText = renderText({
    return("Choose a NYC ZCTAs neighborhood. See how the selected neighborhood differs from the entire NYC and the NYC borough it belongs to. 
Keep one decimal for all numbers.")
  })
  
  
  output$abouttext = renderUI({
    urlzzq = a("Ziqi Zhou",href = "https://www.linkedin.com/in/ziqi-zhou-1b448a145/")
    urlzmy = a("Mengyu Zhang",href = "https://www.linkedin.com/in/mengyu-zhang-553421197")
    urlyyz = a("Yuanzhi Yu", href = "https://www.linkedin.com/in/yuanzhi（fisher）-yu-a1529918a/")
    urlqyc = a("Yuchen Qi",href = "https://www.linkedin.com/in/yuchen-qi/")
    urlcqx = a("Qixuan Chen",href = "https://www.publichealth.columbia.edu/people/our-faculty/qc2138")
    
    tagList("The NYC Neighborhood COVID-19 Dashboard is developed by Chen’s lab at Columbia University Biostatistics Department: 
    ",urlzzq,",",urlzmy,",",urlyyz,",",urlqyc,",",urlcqx,"."
            )
    
  })  
  
  output$abouttext2 = renderText({
    return("
    <br>
    We are thankful to Cindy Liu who designed the dashboard logo and our colleagues in the Mailman School of Public Health for comments and suggestions. We hope that you find the dashboard useful.
      <br> <br>     
    Disclaimer: We assume no responsibility or liability for any errors or omissions in the content of this site. If you believe there is an error in our data, please feel free to contact us. 
")
  })
  
  ###########
  ##Home plot
  
  output$HomePlot= renderPlotly({
    fig1 = ggplot(df.ave, aes(x = date, y = cases)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + geom_line(aes(x = date, y = ave_cases), color = "red", size = 1) + ggtitle("New reported cases by day in New York City") + theme_minimal() + labs(caption = "Note: the seven day average is the average of a day and the past 6 days") + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
      plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
    )
    fig2 = ggplot(df.ave, aes(x = date, y = deaths)) + geom_col(color = "#D5D2D2", fill = "#D5D2D2", alpha = 0.8) + geom_line(aes(x = date, y = ave_deaths), color = "black", size = 1) + ggtitle("New reported deaths by day in New York City") + theme_minimal() + labs(caption = "Note: the seven day average is the average of a day and the past 6 days") + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14),    # Center title position and size
      plot.caption = element_text(hjust = 0, face = "italic")# move caption to the left
    )
    fig1/fig2
  })
  
  
  
  
  output$table <- DT::renderDataTable(DT::datatable({
    data_to_table
  },rownames = FALSE))
  
  
  output$map = renderLeaflet({
    
    plot = switch (input$outcome_selection,
                   positive = positive,
                   death_count = death_count,
                   case_rate = case_rate,
                   death_rate = death_rate,
                   newcase = newcase,
                   incidencerate = incidencerate
    )
    
    plot(input$date_choice)
  })
  
  output$barchart_age = renderPlotly({
    
    a =  byage %>% filter(day == max(byage$day) & group != "Boroughwide") %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      xlab("") +
      ylab("") + 
      facet_wrap(outcome ~ ., scales = "free")
    
    ggplotly(a) %>% layout(legend = list(title=list(text='Age'),orientation = "h", x = 0.4, y = 1.2))
    
  })
  
  output$barchart_sex = renderPlotly({
    
    b =  bysex %>%filter(day == max(bysex$day) & group != "Boroughwide") %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      xlab("") +
      ylab("") + 
      facet_wrap(outcome ~ ., scales = "free")
    
    ggplotly(b) %>% layout(legend = list(orientation = "h", x = 0.4, y = 1.2))
    
    
  })
  
  
  output$barchart_race = renderPlotly({
    
    c =  byrace %>%  filter(day == max(byrace$day) & group != "Boroughwide") %>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group) %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      theme_bw() + 
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      xlab("") +
      ylab("") + 
      facet_wrap(outcome ~ ., scales = "free")
    
    ggplotly(c) %>% layout(legend = list(orientation = "h", x = 0.4, y = 1.2))
    
    
  })
  
  output$piechart_age = renderPlotly({
    pie1data = byage %>% 
      filter(boro == "Bronx"& day == max(byage$day) &group != "Boroughwide" & outcome == input$outcome_age) %>% 
      mutate(group = factor(group,levels = c("0-17","18-44","45-64","65-74","75+"))) %>% 
      arrange(group)
    
    pie2data = byage %>% 
      filter(boro == "Brooklyn"& day == max(byage$day) &group != "Boroughwide" & outcome == input$outcome_age) %>% 
      mutate(group = factor(group,levels = c("0-17","18-44","45-64","65-74","75+"))) %>% 
      arrange(group)
    
    pie3data = byage %>% 
      filter(boro == "Manhattan"& day == max(byage$day) &group != "Boroughwide" & outcome == input$outcome_age) %>% 
      mutate(group = factor(group,levels = c("0-17","18-44","45-64","65-74","75+"))) %>% 
      arrange(group)
    
    pie5data = byage %>% 
      filter(boro == "Staten Island"& day == max(byage$day) &group != "Boroughwide" & outcome == input$outcome_age) %>% 
      mutate(group = factor(group,levels = c("0-17","18-44","45-64","65-74","75+"))) %>% 
      arrange(group)
    
    pie4data = byage %>% 
      filter(boro == "Queens"& day == max(byage$day) &group != "Boroughwide" & outcome == input$outcome_age) %>% 
      mutate(group = factor(group,levels = c("0-17","18-44","45-64","65-74","75+"))) %>% 
      arrange(group)
    
    
    fig <- plot_ly(sort = FALSE)
    
    fig <- fig %>% add_trace(data = pie1data %>% select(group,count), 
                             labels = ~pie1data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name =  ~pie1data$boro, domain = list(row = 0, column = 0)
                             
    )
    fig <- fig %>% add_trace(data = pie2data %>% select(group,count), 
                             labels = ~pie2data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name =  pie2data$boro, domain = list(row = 0, column = 1)
    ) 
    fig <- fig %>% add_trace(data = pie3data %>% select(group,count), 
                             labels = ~pie3data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie3data$boro, domain = list(row = 0, column = 2)) 
    fig <- fig %>% add_trace(data = pie4data %>% select(group,count), 
                             labels = ~pie4data$group, 
                             values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie4data$boro, domain = list(row = 0, column = 3)) 
    fig <- fig %>% add_trace(data = pie5data %>% select(group,count), 
                             labels = ~pie5data$group, 
                             values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie5data$boro, domain = list(row = 0, column = 4)) 
    fig <- fig %>% layout(title = "", showlegend = T,
                          grid=list(rows=1, columns=5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          legend = list(title=list(text='Age'),orientation = "h", x = 0.4, y = 1.2)) %>% 
      add_annotations(x=seq(0.1,0.1+4*0.2,0.2),
                      y=0.05,
                      text = c("Bronx", "Brooklyn", "Manhattan","Queens","Staten Island"),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      showarrow = FALSE
      )
    
    fig
    
  })
  
  output$piechart_sex = renderPlotly({
    
    pie1data = bysex %>% 
      filter(boro == "Bronx"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie3data = bysex %>% 
      filter(boro == "Manhattan"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie2data = bysex %>% 
      filter(boro == "Brooklyn"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie4data = bysex %>% 
      filter(boro == "Queens"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie5data = bysex %>% 
      filter(boro == "Staten Island"& day == max(bysex$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    
    fig <- plot_ly(sort = FALSE)
    fig <- fig %>% add_trace(data = pie1data %>% select(group,count), 
                             labels = ~pie1data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name =  pie1data$boro, domain = list(row = 0, column = 0))
    fig <- fig %>% add_trace(data = pie2data %>% select(group,count), 
                             labels = ~pie2data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name =  pie2data$boro, domain = list(row = 0, column = 1)) 
    fig <- fig %>% add_trace(data = pie3data %>% select(group,count), 
                             labels = ~pie3data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie3data$boro, domain = list(row = 0, column = 2)) 
    fig <- fig %>% add_trace(data = pie4data %>% select(group,count), 
                             labels = ~pie4data$group, 
                             values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie4data$boro, domain = list(row = 0, column = 3)) 
    fig <- fig %>% add_trace(data = pie5data %>% select(group,count), 
                             labels = ~pie5data$group, 
                             values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = "pie",
                             name = pie5data$boro, domain = list(row = 0, column = 4)) 
    fig <- fig %>% layout(title = "", showlegend = T,
                          grid=list(rows=1, columns=5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          legend = list(orientation = "h", x = 0.4, y = 1.2)) %>% 
      add_annotations(x=seq(0.1,0.1+4*0.2,0.2),
                      y=0.05,
                      text = c("Bronx", "Brooklyn", "Manhattan","Queens","Staten Island"),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      showarrow = FALSE
      )
    
    fig
  })
  
  
  output$piechart_race = renderPlotly({
    
    pie1data = byrace %>% 
      filter(boro == "Bronx"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race) %>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie3data = byrace %>% 
      filter(boro == "Manhattan"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie2data = byrace %>% 
      filter(boro == "Brooklyn"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie4data = byrace %>% 
      filter(boro == "Queens"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie5data = byrace %>% 
      filter(boro == "Staten Island"& day == max(byrace$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    
    fig <- plot_ly(sort = FALSE)
    fig <- fig %>% add_trace(data = pie1data %>% select(group,count), 
                             labels = ~pie1data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name =  pie1data$boro, domain = list(row = 0, column = 0)
    )
    fig <- fig %>% add_trace(data = pie2data %>% select(group,count), 
                             labels = ~pie2data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name =  pie2data$boro, domain = list(row = 0, column = 1)
    )
    fig <- fig %>% add_trace(data = pie3data %>% select(group,count), 
                             labels = ~pie3data$group, values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie3data$boro, domain = list(row = 0, column = 2)
    ) 
    fig <- fig %>% add_trace(data = pie4data %>% select(group,count), 
                             labels = ~pie4data$group, 
                             values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie4data$boro, domain = list(row = 0, column = 3)
    ) 
    fig <- fig %>% add_trace(data = pie5data %>% select(group,count), 
                             labels = ~pie5data$group, 
                             values = ~count,
                             text = ~paste(round((count/sum(count))*100, digits = 1),"%"),
                             textinfo='text',
                             textposition="auto",
                             type = 'pie',
                             name = pie5data$boro, domain = list(row = 0, column = 4)
    )
    fig <- fig %>% layout(title = "", 
                          showlegend = T,
                          grid=list(rows=1, columns=5),
                          xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
                          legend = list(orientation = "h", x = 0.4, y = 1.2)) %>% 
      add_annotations(x=seq(0.1,0.1+4*0.2,0.2),
                      y=0.05,
                      text = c("Bronx", "Brooklyn", "Manhattan","Queens","Staten Island"),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      showarrow = FALSE
      )
    
    fig
  })
  
  
  
  ### Demographics
  
  # Race
  output$race_nbh <- renderPlotly({
    
    which_boro = race %>% filter(neighborhood_name == input$nbhid1) %>% select(borough_group) %>% unique()
    
    
    race_nbh = race %>% 
      filter(neighborhood_name == input$nbhid1) %>%
      pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>%
      group_by(neighborhood_name, race) %>% 
      summarise(pop = sum(population)) %>% 
      mutate(race = factor(race)) %>% 
      drop_na()
    
    race_nbh = race_nbh %>% 
      mutate(race = str_replace_all(race, "american_indian_and_alaska_native_alone","American Indian and Alaska Native Alone"),
             race = str_replace_all(race, "asian_alone","Asian Alone"),
             race = str_replace_all(race, "black_or_african_american_alone","Black or African American Alone"),
             race = str_replace_all(race, "native_hawaiian_and_other_pacific_islander_alone", "Native Hawaiian and Other Pacific Islander Alone"),
             race = str_replace_all(race, "some_other_race_alone","Some Other Race Alone"),
             race = str_replace_all(race, "two_or_more_races","Two or More Races"),
             race = str_replace_all(race, "white_alone", "White Alone")) %>% 
      mutate(race = factor(race, levels = c("White Alone","Black or African American Alone",
                                            "Asian Alone","American Indian and Alaska Native Alone",
                                            "Native Hawaiian and Other Pacific Islander Alone",
                                            "Some Other Race Alone","Two or More Races"))) %>% 
      arrange(race)
    
    race_gp = race %>% 
      filter(borough_group == which_boro$borough_group) %>% 
      pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>% 
      group_by(race) %>% 
      summarise(pop = sum(population)) %>% 
      mutate(race = factor(race)) %>% 
      drop_na()
    
    race_gp = race_gp %>% 
      mutate(race = str_replace_all(race, "american_indian_and_alaska_native_alone","American Indian and Alaska Native Alone"),
             race = str_replace_all(race, "asian_alone","Asian Alone"),
             race = str_replace_all(race, "black_or_african_american_alone","Black or African American Alone"),
             race = str_replace_all(race, "native_hawaiian_and_other_pacific_islander_alone", "Native Hawaiian and Other Pacific Islander Alone"),
             race = str_replace_all(race, "some_other_race_alone","Some Other Race Alone"),
             race = str_replace_all(race, "two_or_more_races","Two or More Races"),
             race = str_replace_all(race, "white_alone", "White Alone")) %>% 
      mutate(race = factor(race, levels = c("White Alone","Black or African American Alone",
                                            "Asian Alone","American Indian and Alaska Native Alone",
                                            "Native Hawaiian and Other Pacific Islander Alone",
                                            "Some Other Race Alone","Two or More Races"))) %>% 
      arrange(race)
    
    
    race_nyc = race %>% 
      pivot_longer(white_alone:two_or_more_races, names_to = "race", values_to = "population") %>%
      group_by(race) %>% 
      summarise(pop = sum(population)) %>% 
      mutate(race = factor(race)) %>% 
      drop_na() 
    
    race_nyc = race_nyc %>% 
      mutate(race = str_replace_all(race, "american_indian_and_alaska_native_alone","American Indian and Alaska Native Alone"),
             race = str_replace_all(race, "asian_alone","Asian Alone"),
             race = str_replace_all(race, "black_or_african_american_alone","Black or African American Alone"),
             race = str_replace_all(race, "native_hawaiian_and_other_pacific_islander_alone", "Native Hawaiian and Other Pacific Islander Alone"),
             race = str_replace_all(race, "some_other_race_alone","Some Other Race Alone"),
             race = str_replace_all(race, "two_or_more_races","Two or More Races"),
             race = str_replace_all(race, "white_alone", "White Alone")) %>% 
      mutate(race = factor(race, levels = c("White Alone","Black or African American Alone",
                                            "Asian Alone","American Indian and Alaska Native Alone",
                                            "Native Hawaiian and Other Pacific Islander Alone",
                                            "Some Other Race Alone","Two or More Races"))) %>% 
      arrange(race)
    
    plot = plot_ly(sort = FALSE)
    
    plot = plot %>% 
      add_trace(data = race_nbh,
                labels = str_replace_all(race_nbh$race,"_"," "),
                values = race_nbh$pop,
                text = ~paste(round((pop/sum(pop))*100, digits = 1),"%"),
                textinfo='text',
                textposition="auto",
                type = 'pie',
                opacity=0.8,
                domain = list(row = 0, column = 0),
                marker = list(colors = brewer.pal(7,"Blues")))
    
    
    plot = plot %>% 
      add_trace(data = race_gp,
                labels = str_replace_all(race_gp$race,"_"," "),
                values = race_gp$pop,
                text = ~paste(round((pop/sum(pop))*100, digits = 1),"%"),
                textinfo='text',
                textposition="auto",
                type = 'pie',
                opacity=0.8,
                domain = list(row = 0, column = 1),
                marker = list(colors = brewer.pal(7,"Blues")))
    
    
    plot = plot %>% 
      add_trace(data = race_nyc,
                labels = str_replace_all(race_nbh$race,"_"," "),
                values = race_nbh$pop,
                text = ~paste(round((pop/sum(pop))*100, digits = 1),"%"),
                textinfo='text',
                textposition="auto",
                type = 'pie',
                opacity=0.8,
                domain = list(row = 0, column = 2),
                marker = list(colors = brewer.pal(7,"Blues")))
    plot = plot %>%
      layout(title = "", showlegend = T,
             grid=list(rows=1, columns=3),
             xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
             legend=list(title=list(text='<b> Race </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5)) %>% 
      add_annotations(x=seq(0.15,0.15+2*0.35,0.35),
                      y=-0.3,
                      text = c(paste(input$nbhid1), paste(which_boro), "New York City"),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      showarrow = FALSE
      )
    
    
    plot
    
  })
  
  
  output$race_map <- renderLeaflet({
    pt = race %>% 
      select(zipcode:total, str_replace_all(input$raceid," ","_"))
    
    pct = rowSums(pt[5:ncol(pt)])/pt$total
    
    race_pt = race %>% 
      mutate(percentage = pct)
    
    
    posi_map_race = geo_join(spdf,race_pt,"MODZCTA","zipcode")
    
    pal <- colorNumeric("Blues", domain=posi_map_race$percentage)
    
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
  
  #income
  output$nbh3 <- renderText({
    
    input$nbhid3
    
  })
  output$nyc3 <- renderText({"New York City"})
  output$boro3 <- renderText({
    
    which_boro = race %>% filter(neighborhood_name == input$nbhid3) %>% select(borough_group) %>% unique()
    which_boro$borough_group
    
  })
  
  output$income_map <- renderLeaflet({
    posi_map_income = geo_join(spdf,income,"MODZCTA","zipcode")
    
    pal <- colorNumeric("Blues", domain=posi_map_income$median)
    
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
  output$income_nbh <- renderPlotly ({
    income_nbh = income %>% 
      drop_na() %>% 
      filter(neighborhood_name == input$nbhid3) %>%
      select(mean, median)
    
    which_boro = income %>% filter(neighborhood_name == input$nbhid3) %>% select(borough_group) %>% unique()
    
    incomeboro = income %>% 
      drop_na() %>% 
      filter(borough_group == which_boro$borough_group)
    
    mean = mean(incomeboro$mean)
    median = median(incomeboro$median)
    
    income_boro = c(mean, median)
    
    mean = mean(na.omit(income$mean))
    median = median(na.omit(income$median))
    
    income_nyc = c(mean, median)
    
    income_final = rbind(income_nbh, income_boro, income_nyc) %>% 
      mutate(level = c("NTA", "Borough", "NYC")) %>% 
      pivot_longer(mean:median, names_to = "stat", values_to = "value")
    
    
    
    plot_ly(x = forcats::fct_reorder(income_final$stat, income_final$value),
            y = income_final$value,
            type = "bar",
            color = income_final$level,
            colors = "Blues",
            size = 3,
            opacity = 0.8) %>% 
      layout(legend=list(title=list(text='<b> Statistics </b>')))
    
    
  })
  
  #Household
  
  output$household_map <- renderLeaflet({
    pt = household %>% 
      select(zipcode:total, str_replace_all(input$houseid," ","_"))
    
    pct = rowSums(pt[5:ncol(pt)])/pt$total
    
    house_pt = household %>% 
      mutate(percentage = pct)
    
    
    posi_map_house = geo_join(spdf,house_pt,"MODZCTA","zipcode")
    
    pal <- colorNumeric("Blues", domain=posi_map_house$percentage)
    
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
                title = "Proportions on NTA Level")
    
  })
  
  
  output$household_nbh <- renderPlotly({
    
    house_nbh = household %>% 
      filter(neighborhood_name == input$nbhid2) %>%
      pivot_longer(size_1:size_7_or_more, names_to = "size", values_to = "number") %>%
      group_by(neighborhood_name, size) %>% 
      summarise(num = sum(number)) %>% 
      mutate(size = str_replace_all(size,"size_1","Size 1"),
             size = str_replace_all(size,"size_2","Size 2"),
             size = str_replace_all(size,"size_3","Size 3"),
             size = str_replace_all(size,"size_4","Size 4"),
             size = str_replace_all(size,"size_5","Size 5"),
             size = str_replace_all(size,"size_6","Size 6"),
             size = str_replace_all(size,"size_7_or_more","Size 7 or more")) %>% 
      mutate(size = factor(size)) %>% 
      drop_na()
    
    which_boro = race %>% filter(neighborhood_name == input$nbhid2) %>% select(borough_group) %>% unique()
    
    house_gp = household %>% 
      filter(borough_group == which_boro$borough_group) %>% 
      pivot_longer(size_1:size_7_or_more, names_to = "size", values_to = "number") %>% 
      group_by(size) %>% 
      summarise(num = sum(number)) %>% 
      mutate(size = str_replace_all(size,"size_1","Size 1"),
             size = str_replace_all(size,"size_2","Size 2"),
             size = str_replace_all(size,"size_3","Size 3"),
             size = str_replace_all(size,"size_4","Size 4"),
             size = str_replace_all(size,"size_5","Size 5"),
             size = str_replace_all(size,"size_6","Size 6"),
             size = str_replace_all(size,"size_7_or_more","Size 7 or more")) %>% 
      mutate(size = factor(size)) %>% 
      drop_na() 
    
    
    house_nyc = household %>% 
      pivot_longer(size_1:size_7_or_more, names_to = "size", values_to = "number") %>%
      group_by(size) %>% 
      summarise(num = sum(number)) %>% 
      mutate(size = str_replace_all(size,"size_1","Size 1"),
             size = str_replace_all(size,"size_2","Size 2"),
             size = str_replace_all(size,"size_3","Size 3"),
             size = str_replace_all(size,"size_4","Size 4"),
             size = str_replace_all(size,"size_5","Size 5"),
             size = str_replace_all(size,"size_6","Size 6"),
             size = str_replace_all(size,"size_7_or_more","Size 7 or more")) %>% 
      mutate(size = factor(size)) %>% 
      drop_na()
    
    plot = plot_ly(sort = FALSE)
    
    plot = plot %>% 
      add_trace(data = house_nbh,
                labels = ~house_nbh$size,
                values = ~house_nbh$num,
                text = ~paste(round((num/sum(num))*100, digits = 1),"%"),
                textinfo='text',
                textposition="auto",
                type = 'pie',
                name = ~house_nbh$neighborhood_name,
                domain = list(row = 0, column = 0),
                marker = list(colors = brewer.pal(7,"Blues")))
    
    
    plot = plot %>% 
      add_trace(data = house_gp,
                labels = ~house_gp$size,
                values = ~house_gp$num,
                text = ~paste(round((num/sum(num))*100, digits = 1),"%"),
                textinfo='text',
                textposition="auto",
                type = 'pie',
                name = ~which_boro,
                domain = list(row = 0, column = 1),
                marker = list(colors = brewer.pal(7,"Blues")))
    
    
    plot = plot %>% 
      add_trace(data = house_nyc,
                labels = ~house_nyc$size,
                values = ~house_nyc$num,
                text = ~paste(round((num/sum(num))*100, digits = 1),"%"),
                textinfo='text',
                textposition="auto",
                type = 'pie',
                name = ~paste("New York City"),
                domain = list(row = 0, column = 2),
                marker = list(colors = brewer.pal(7,"Blues")))
    
    plot = plot %>%
      layout(title = "", showlegend = T,
             grid=list(rows=1, columns=3),
             xaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
             yaxis = list(showgrid = F, zeroline = FALSE, showticklabels = F),
             legend=list(title=list(text='<b> Family Size </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5)) %>% 
      add_annotations(x=seq(0.15,0.15+2*0.35,0.35),
                      y=-0.3,
                      text = c(paste(input$nbhid2), paste(which_boro), "New York City"),
                      xref = "paper",
                      yref = "paper",
                      xanchor = "center",
                      showarrow = FALSE
      )
    
    plot
    
    
    
  })
  
  
  #### Time Trend
  ### Case Count
  output$tt_age_cac = renderPlotly({
    Week <- unique(as.Date(cut(byage$day, "week")) + 6)
    weeklyage <- byage %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyage$day)
    x_max_us = max(weeklyage$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyage %>% 
      filter(group != "Boroughwide" & outcome == "Case Count") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Age  "),orientation = "h", x = 0.4, y = -0.2))
    
    
    
  })
  output$tt_sex_cac = renderPlotly({
    
    Week <- unique(as.Date(cut(bysex$day, "week")) + 6)
    weeklysex <- bysex %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklysex$day)
    x_max_us = max(weeklysex$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklysex %>% 
      filter(group != "Boroughwide" & outcome == "Case Count") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45)) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Gender  "),orientation = "h", x = 0.4, y = -0.2))
    
  })
  output$tt_race_cac = renderPlotly({
    Week <- unique(as.Date(cut(byrace$day, "week")) + 6)
    weeklyrace <- byrace %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyrace$day)
    x_max_us = max(weeklyrace$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyrace %>% 
      filter(group != "Boroughwide" & outcome == "Case Count") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Race  "),orientation = "h", x = 0.4, y = -0.2))
    
    
  })
  
  
  ####
  ## Case Rate
  output$tt_age_carate = renderPlotly({
    Week <- unique(as.Date(cut(byage$day, "week")) + 6)
    weeklyage <- byage %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyage$day)
    x_max_us = max(weeklyage$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyage %>% 
      filter(group != "Boroughwide" & outcome == "Case Rate (per 100,000 people)") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Age  "),orientation = "h", x = 0.4, y = -0.2))
    
    
    
  })
  output$tt_sex_carate = renderPlotly({
    
    Week <- unique(as.Date(cut(bysex$day, "week")) + 6)
    weeklysex <- bysex %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklysex$day)
    x_max_us = max(weeklysex$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklysex %>% 
      filter(group != "Boroughwide" & outcome == "Case Rate (per 100,000 people)") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45)) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Gender  "),orientation = "h", x = 0.4, y = -0.2))
    
  })
  output$tt_race_carate = renderPlotly({
    Week <- unique(as.Date(cut(byrace$day, "week")) + 6)
    weeklyrace <- byrace %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyrace$day)
    x_max_us = max(weeklyrace$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyrace %>% 
      filter(group != "Boroughwide" & outcome == "Case Rate (per 100,000 people)") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Race  "),orientation = "h", x = 0.4, y = -0.2))
    
    
  })
  
  ### Death Count
  
  output$tt_age_dec = renderPlotly({
    Week <- unique(as.Date(cut(byage$day, "week")) + 6)
    weeklyage <- byage %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyage$day)
    x_max_us = max(weeklyage$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyage %>% 
      filter(group != "Boroughwide" & outcome == "Death Count") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Age  "),orientation = "h", x = 0.4, y = -0.2))
    
    
    
  })
  output$tt_sex_dec = renderPlotly({
    
    Week <- unique(as.Date(cut(bysex$day, "week")) + 6)
    weeklysex <- bysex %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklysex$day)
    x_max_us = max(weeklysex$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklysex %>% 
      filter(group != "Boroughwide" & outcome == "Death Count") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Gender  "),orientation = "h", x = 0.4, y = -0.2))
    
  })
  output$tt_race_dec = renderPlotly({
    Week <- unique(as.Date(cut(byrace$day, "week")) + 6)
    weeklyrace <- byrace %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyrace$day)
    x_max_us = max(weeklyrace$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyrace %>% 
      filter(group != "Boroughwide" & outcome == "Death Count") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Race  "),orientation = "h", x = 0.4, y = -0.2))
    
    
  })
  
  ###Death Rate
  
  
  output$tt_age_derate = renderPlotly({
    Week <- unique(as.Date(cut(byage$day, "week")) + 6)
    weeklyage <- byage %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyage$day)
    x_max_us = max(weeklyage$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyage %>% 
      filter(group != "Boroughwide" & outcome == "Death Rate (per 100,000 people)") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Age  "),orientation = "h", x = 0.4, y = -0.2))
    
    
    
  })
  output$tt_sex_derate = renderPlotly({
    
    Week <- unique(as.Date(cut(bysex$day, "week")) + 6)
    weeklysex <- bysex %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklysex$day)
    x_max_us = max(weeklysex$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklysex %>% 
      filter(group != "Boroughwide" & outcome == "Death Rate (per 100,000 people)") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Gender  "),orientation = "h", x = 0.4, y = -0.2))
    
  })
  output$tt_race_derate = renderPlotly({
    Week <- unique(as.Date(cut(byrace$day, "week")) + 6)
    weeklyrace <- byrace %>% 
      filter(day %in% Week)
    
    x_min_us = min(weeklyrace$day)
    x_max_us = max(weeklyrace$day)
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    a = weeklyrace %>% 
      filter(group != "Boroughwide" & outcome == "Death Rate (per 100,000 people)") %>% 
      ggplot(aes(x = day, y = count ,color = group, group = group)) + 
      geom_line(size = 0.3) + geom_point(size = 0.8) + facet_grid(~boro) + 
      theme_minimal() +  
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      theme(legend.title = element_blank()) + 
      theme(legend.position="bottom") + 
      xlab("") + 
      ylab("")
    
    ggplotly(a) %>% layout(legend = list(title = list(text = "Race  "),orientation = "h", x = 0.4, y = -0.2))
    
    
  })
  
  ####### boro cases
  
  output$boro_cases = renderLeaflet({
    
    plot = switch (input$selection,
                   cum_case = cum_case,
                   new_case = new_case
    )
    
    plot()
  })
  
  
  #######
  
  output$pocase <- renderPlotly({
    
    weeklydf %>% 
      filter(zipcode_new %in% input$zip1) %>%
      plot_ly(x = ~day,
              y = ~positive,
              type="scatter",
              mode = 'lines+markers',
              colors= "Blues") %>% 
      layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
             xaxis = list(title = "",type = "date"),
             yaxis = list(title = ""))
    
    
    
  })
  
  output$death <- renderPlotly({
    weeklydf %>% 
      filter(zipcode_new %in% input$zip2) %>%
      plot_ly(x = ~day,
              y = ~covid_death_count,
              type="scatter",
              mode = 'lines+markers',
              colors= "Blues") %>% 
      layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
             xaxis = list(title = "",type = "date"),
             yaxis = list(title = ""))
    
  }) 
  
  output$porate <- renderPlotly({
    weeklydf %>% 
      filter(zipcode_new %in% input$zip3) %>%
      plot_ly(x = ~day,
              y = ~covid_case_rate,
              type="scatter",
              mode = 'lines+markers',
              colors= "Blues") %>% 
      layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
             xaxis = list(title = "",type = "date"),
             yaxis = list(title = ""))
  }) 
  
  output$derate <- renderPlotly({
    weeklydf %>% 
      filter(zipcode_new %in% input$zip4) %>%
      plot_ly(x = ~day,
              y = ~covid_death_rate,
              type="scatter",
              mode = 'lines+markers',
              colors= "Blues") %>% 
      layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
             xaxis = list(title = "",type = "date"),
             yaxis = list(title = ""))
  })
  
  output$newcases <- renderPlotly({
    weeklynew %>% 
      mutate(zipcode_new = factor(zipcode_new)) %>% 
      filter(zipcode_new %in% input$zip5) %>%
      plot_ly(x = ~week,
              y = ~new_cases,
              type="scatter",
              mode = 'lines+markers',
              colors= "Blues") %>% 
      layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
             xaxis = list(title = "",type = "date"),
             yaxis = list(title = ""))
  })
  
  
  
  
}

shinyApp(ui, server)




