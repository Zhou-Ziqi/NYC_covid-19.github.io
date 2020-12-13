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
library(lubridate)

###set the date

today = as.Date("2020-12-12")
yesterday = today - 1

##
url1 = "https://twitter.com/intent/tweet?text=Hello%20world&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url2 = "https://www.facebook.com/sharer.php?u=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url3 = "https://www.instagram.com/columbiapublichealth/"
url4 = "https://www.linkedin.com/shareArticle?mini=true&url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title=&summary=&source="
url5 = "mailto:NYCN-COVID@cumc.columbia.edu?&subject=&body=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url6 = "whatsapp://send?text=https://msph.shinyapps.io/nyc-neighborhoods-covid/"
url7 = "https://service.weibo.com/share/share.php?url=https://msph.shinyapps.io/nyc-neighborhoods-covid/&title="
##read data
##Home page data

#df = read_csv("data/data-yqfdF.csv") %>% 
#  janitor::clean_names() %>% 
#  rename(date = date_of_interest) %>% 
#  mutate(date = as.Date(date, format = "%m/%d/%Y"))
#N = nrow(df)
#cases = pull(df,cases)
#deaths = pull(df,deaths)
#ave.cases = rep(0, N-6)
#ave.deaths = rep(0, N-6)

#for (i in 7:N) {
#  ave.cases[i] = mean(cases[(i-6):(i)])
#  ave.deaths[i] = mean(deaths[(i-6):(i)])
#}

#df.ave = df %>% 
#  mutate(ave_cases = round(ave.cases), ave_deaths = round(ave.deaths))

### summary data

summary_table1 = read_csv(paste0("./data/summarytable/summary",month(yesterday),day(yesterday),".csv"))

summary_table2 = read_csv(paste0("./data/summarytable/summary",month(today),day(today),".csv"))





### trakcer data
data_yester = read_csv(paste0("./data/data_for_table/data-by-modzcta",month(yesterday),day(yesterday),".csv")) %>% 
  janitor::clean_names() %>% 
  mutate(date = yesterday) %>% drop_na()
data_today = read_csv(paste0("./data/data_for_table/data-by-modzcta",month(today),day(today),".csv"))  %>% 
  janitor::clean_names() %>% 
  mutate(date = today) %>% drop_na()



data_tracker = rbind(data_today,data_yester)
new_case = 0
new_death = 0
new_test = 0

for (i in 1:177) {
  new_case[i] = data_tracker[i,4] - data_tracker[i+177,4]
  new_case
  
  new_death[i] = data_tracker[i,7] - data_tracker[i+177,7]
  new_death
  
  new_test[i] = data_tracker[i,10] - data_tracker[i+177,10]
  new_test
}

for (i in 178:354) {
  
  new_case[i] = NA
  
  new_death[i] = NA
  
  new_test[i] = NA
}

data_to_table = data_tracker %>% 
  mutate(new_case = new_case,
         new_death = new_death,
         new_test = new_test) %>% 
  filter(date == max(date)) %>% 
  mutate(new_case = as.numeric(new_case),
         new_death = as.numeric(new_death),
         new_test = as.numeric(new_test),
         incidence_rate = round(new_case*100000/pop_denominator, digits = 1),
         new_test_rate = round(new_test*100000/pop_denominator, digits = 1),
         total_test_rate = round(total_covid_tests*100000/pop_denominator, digits = 1)
  )%>% 
  dplyr::select(modified_zcta,neighborhood_name,borough_group,
                covid_case_count,new_case,incidence_rate,
                covid_death_count,new_death, total_covid_tests,new_test) %>% 
  rename("New Cases" = new_case,
         "New Deaths" = new_death,
         "Zip Code" = modified_zcta,
         "Neighborhoods" = neighborhood_name,
         "Borough"= borough_group,
         "Total Cases" = covid_case_count,
         "Total Deaths" = covid_death_count,
         "Incidence Rate*" = incidence_rate,
         "Total Tests" = total_covid_tests,
         "New Tests" = new_test )

data_to_plot = data_tracker %>% 
  mutate(new_case = new_case,
         new_death = new_death,
         new_test = new_test
  ) %>% 
  filter(date == max(date)) %>% 
  mutate(new_case = as.numeric(new_case),
         new_death = as.numeric(new_death),
         new_test = as.numeric(new_test),
         incidence_rate = round(new_case*100000/pop_denominator, digits = 1),
         new_test_rate = round(new_test*100000/pop_denominator, digits = 1),
         total_test_rate = round(total_covid_tests*100000/pop_denominator, digits = 1))

###########
# The map

spdf = rgdal::readOGR("./Geography-resources/MODZCTA_2010_WGS1984.geo.json")

choices = c("Total Cases", "Total Deaths", "Case Rate (per 100,000 people)", "Death Rate(per 100,000 people)","New cases")



######
#demographics
#by race today for 2020-11-18

byracetoday = read_csv(paste0("./distribution_of_covid-19/data/demorace_today",month(today),day(today),".csv")) %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Total Cases"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Total Hospitalizations"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Total Deaths"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count)) %>% 
  mutate(outcome = factor(outcome, levels = c("Total Cases","Total Hospitalizations","Total Deaths",
                                              "Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(outcome) %>% 
  select(-group) %>% 
  rename(group = subgroup)

bysextoday = read_csv(paste0("./distribution_of_covid-19/data/demosex_today",month(today),day(today),".csv")) %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Total Cases"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Total Hospitalizations"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Total Deaths"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count)) %>% 
  mutate(outcome = factor(outcome, levels = c("Total Cases","Total Hospitalizations","Total Deaths",
                                              "Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(outcome) %>% 
  select(-group) %>% 
  rename(group = subgroup)

#change!but need to deal with it later
byagetoday = read_csv(paste0("./distribution_of_covid-19/data/demoage_today",month(today),day(today),".csv")) %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Total Cases"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Total Hospitalizations"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Total Deaths"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count)) %>% 
  mutate(outcome = factor(outcome, levels = c("Total Cases","Total Hospitalizations","Total Deaths",
                                              "Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(outcome) %>% 
  select(-group) %>% 
  rename(group = subgroup)
##

byage = read_csv(paste0("./distribution_of_covid-19/data/demoage_until118.csv")) %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Total Cases"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Total Hospitalizations"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Total Deaths"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count)) %>% 
  mutate(outcome = factor(outcome, levels = c("Total Cases","Total Hospitalizations","Total Deaths",
                                              "Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(outcome)


byrace = read_csv(paste0("./distribution_of_covid-19/data/demorace_until118.csv")) %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Total Cases"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Total Hospitalizations"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Total Deaths"),
         outcome = str_replace_all(outcome, "CASE_RATE_ADJ","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE_ADJ","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE_ADJ","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count)) %>% 
  mutate(group = factor(group, levels = c("White","Black/African-American","Asian/Pacific-Islander","Hispanic/Latino")))%>% 
  mutate(outcome = factor(outcome, levels = c("Total Cases","Total Hospitalizations","Total Deaths",
                                              "Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(outcome)



bysex = read_csv(paste0("./distribution_of_covid-19/data/demosex_until118.csv")) %>% 
  mutate(outcome = str_replace_all(outcome, "CASE_COUNT","Total Cases"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_COUNT","Total Hospitalizations"),
         outcome = str_replace_all(outcome, "DEATH_COUNT","Total Deaths"),
         outcome = str_replace_all(outcome, "CASE_RATE","Case Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "HOSPITALIZED_RATE","Hospitalization Rate (per 100,000 people)"),
         outcome = str_replace_all(outcome, "DEATH_RATE","Death Rate (per 100,000 people)"),
         boro = str_replace_all(boro, "BK","Brooklyn"),
         boro = str_replace_all(boro, "MN","Manhattan"),
         boro = str_replace_all(boro, "QN","Queens"),
         boro = str_replace_all(boro, "BX","Bronx"),
         boro = str_replace_all(boro, "SI","Staten Island"),
         count = round(count))%>% 
  mutate(outcome = factor(outcome, levels = c("Total Cases","Total Hospitalizations","Total Deaths",
                                              "Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(outcome)




# Define UI for application that draws plot

outcome_age = byage %>% distinct(outcome) %>% pull()
outcome_age1 = byage %>% distinct(outcome) %>% pull() #new
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
  rename("2_persons" = person_2,
         "1_person" = person_1,
         "3_persons"  = person_3,
         "4_persons" = person_4,
         "5_persons" = person_5,
         "6_persons" = person_6,
         "7_persons_or_more" = person_7_or_more)
household_for_pie = left_join(household_raw, data_by_modzcta) %>% 
  select(zipcode, neighborhood_name, borough_group, everything()) %>% 
  rename("size_2" = person_2,
         "size_1" = person_1,
         "size_3"  = person_3,
         "size_4" = person_4,
         "size_5" = person_5,
         "size_6" = person_6,
         "size_7_or_more" = person_7_or_more)

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
race_name = c("White Alone","Black or African American Alone",
              "Asian Alone","American Indian and Alaska Native Alone",
              "Native Hawaiian and Other Pacific Islander Alone",
              "Some Other Race Alone","Two or More Races")
house_name  = colnames(household)[5:11]

###Time Trend

finalMaydata <- read_csv("data/finalMaydata.csv") %>% 
  mutate(pop_denominator = round(pop_denominator),
         incidence_rate = (newcases/pop_denominator)*100000) %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,incidence_rate,newcases,pop_denominator) %>% 
  mutate(day = as.Date(day,format = "%m/%d/%y"))

final_Junedata <- read_csv("data/final_Junedata.csv") %>% 
  dplyr::rename(newcases = newcases_june) %>% 
  mutate(pop_denominator = round(pop_denominator),
         incidence_rate = (newcases/pop_denominator)*100000) %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,incidence_rate,newcases,pop_denominator)
#popula_infor = final_Junedata <- read_csv("data/final_Junedata.csv") %>% 
#  dplyr::rename(newcases = newcases_june) %>% 
#  select(zipcode, pop_denominator) %>% distinct() %>% 
#  mutate(pop_denominator = round(pop_denominator))

#Aprildata_with_nebhod <- read_csv("data/Aprildata_with_nebhod.csv")

final_Julydata = read_csv("data/final_Julydata.csv") %>% 
  mutate(incidence_rate = (newcases/pop_denominator)*100000) %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,incidence_rate,newcases,pop_denominator) 

Augdata <- read_csv(paste0("./data/modzcta_until",month(today),day(today),".csv")) %>% 
  mutate(incidence_rate = (round(newcases/pop_denominator*100000, digits = 1))) %>% 
  select(zipcode,day,neighborhood_name,borough_group, positive,covid_case_rate, covid_death_count, covid_death_rate,incidence_rate,newcases,pop_denominator)

data <- rbind(finalMaydata,final_Junedata)
data <- rbind(data,final_Julydata)
data <- rbind(data,Augdata)

data.mvag.daily = data %>% 
  pivot_longer(positive:newcases,
               names_to = "type",
               values_to = "num") %>% 
  mutate(type = recode(type, 
                       "positive" = "Total Cases", 
                       "covid_case_rate" = "Case Rate (per 100,000 people)",
                       "covid_death_count" = "Total Deaths",
                       "covid_death_rate" = "Death Rate (per 100,000 people)",
                       "newcases" = "New Cases",
                       "incidence_rate" = "Incidence Rate (per 100,000 people)"))
#fill the blank in 0629 and 0819
data.mvag.daily0629  = data.mvag.daily %>% 
filter(day == "2020-06-28") %>% 
  mutate(date = as.Date("2020-06-29")) %>% 
  select(-day) %>% 
  rename(day = date) %>% 
  select(zipcode,day,everything())


data.mvag.daily0819  = data.mvag.daily %>% 
  filter(day == "2020-08-18")%>% 
  mutate(date = as.Date("2020-08-19")) %>% 
  select(-day) %>% 
  rename(day = date) %>% 
  select(zipcode,day,everything())

data.mvag.daily = rbind(data.mvag.daily,data.mvag.daily0629)
data.mvag.daily = rbind(data.mvag.daily,data.mvag.daily0819) %>% arrange(day)

data.mvag.daily$zipcode_new <- paste(data.mvag.daily$zipcode, data.mvag.daily$neighborhood_name)

######
Week <- unique(as.Date(cut(data$day, "week")) + 6)

weeklydf_ <- data %>% 
  mutate(zipcode = factor(zipcode)) %>% 
  filter(day %in% Week)


weeklydf_max <- weeklydf_ %>%
  filter(day == max(day)) %>% select(zipcode,neighborhood_name)

weeklydf_ <- weeklydf_ %>% rename(nbh = neighborhood_name) %>% select(-zipcode)

weeklydf <-cbind(weeklydf_max, weeklydf_) 

weeklydf$zipcode_new <- paste(weeklydf$zipcode, weeklydf$neighborhood_name)
weeklydf <- weeklydf %>% 
  pivot_longer(positive:newcases,
               names_to = "type",
               values_to = "num") %>% 
  mutate(type = recode(type, 
                       "positive" = "Total Cases", 
                       "covid_case_rate" = "Case Rate (per 100,000 people)",
                       "covid_death_count" = "Total Deaths",
                       "covid_death_rate" = "Death Rate (per 100,000 people)",
                       "newcases" = "New Cases",
                       "incidence_rate" = "Incidence Rate (per 100,000 people)"))

zip_nbh <- weeklydf %>% pull(zipcode_new) %>% unique()

data$week <- as.Date(cut(data$day, "week")) + 6
weeklynew <- aggregate(data$newcases, by=list(week=data$week, zipcode = data$zipcode), FUN=sum)
weeklynew <- weeklynew %>% rename(new_cases = x) %>% mutate(zipcode = factor(zipcode))

weeklynew <- left_join(weeklynew, weeklydf_max)
weeklynew$zipcode_new <- paste(weeklynew$zipcode, weeklynew$neighborhood_name)

##add moving average


# boro time trend written on Aug 19
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
         pop_num = ifelse(boro == "Bronx", 1434693, 
                          ifelse(boro == "Brooklyn",2582830, 
                                 ifelse(boro == "Manhattan",1611943, 
                                        ifelse(boro == "Staten Island", 476179,
                                               ifelse(boro == "Queens", 2288710, 2288710))))),
         rate = round(count/pop_num*1000000,1)) %>% 
  rename(Borough = boro,
         Date = week,
         Count = count,
         Rate = rate) %>% 
  mutate(type = str_replace_all(type,"Case Count", "Total Cases"),
         type = str_replace_all(type,"Death Count", "Total Deaths"),
         type = str_replace_all(type,"Hospitalization Count", "Total Hospitalizations"),
         newtype = str_replace_all(newtype,"Case Rate","Case Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Death Rate","Death Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Hospitalization Rate","Hospitalization Rate (per 100,000 people)"))%>% 
  mutate(type = factor(type, levels = c("Total Cases","Total Hospitalizations","Total Deaths")),
         newtype = factor(newtype, levels = c("Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(type) %>% 
  arrange(newtype)




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
         pop_num = ifelse(boro == "Bronx", 1434693, 
                          ifelse(boro == "Brooklyn",2582830, 
                                 ifelse(boro == "Manhattan",1611943, 
                                        ifelse(boro == "Staten Island", 476179,
                                               ifelse(boro == "Queens", 2288710, 2288710))))),
         rate = round(count/pop_num*1000000,1)) %>% 
  rename(Borough = boro,
         Date = date_of_interest,
         Count = count,
         Rate = rate) %>% 
  mutate(type = str_replace_all(type,"Case Count", "Total Cases"),
         type = str_replace_all(type,"Death Count", "Total Deaths"),
         type = str_replace_all(type,"Hospitalization Count", "Total Hospitalizations"),
         newtype = str_replace_all(newtype,"Case Rate","Case Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Death Rate","Death Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Hospitalization Rate","Hospitalization Rate (per 100,000 people)"))%>% 
  mutate(type = factor(type, levels = c("Total Cases","Total Hospitalizations","Total Deaths")),
         newtype = factor(newtype, levels = c("Case Rate (per 100,000 people)","Hospitalization Rate (per 100,000 people)","Death Rate (per 100,000 people)"))) %>% 
  arrange(type) %>% 
  arrange(newtype)




cum_case_count <- function(){
  temp1 <- weeklydf_cum %>% 
    ggplot(aes(x = Date, y = Count)) + 
    geom_line(aes(color = Borough)) +
    geom_point(aes(color = Borough)) +
    facet_wrap(.~type, scales = "free") +
    theme_minimal() +
    #theme(legend.position = "none")+
    #theme(panel.spacing.y=unit(2, "lines")) + 
    xlab("") + 
    ylab("")
  
  
  
  vline <- function(x = 0, color = "red") {
    list(
      type = "line", 
      y0 = 0, 
      y1 = 100000, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  
  
  ggplotly(temp1) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
           shapes = list(vline(as.Date("2020-03-01")), vline(as.Date("2020-03-20")),vline(as.Date("2020-06-22")),vline(as.Date("2020-07-06")),vline(as.Date("2020-07-20"))),
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

cum_case_rate <- function(){
  
  
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
      y1 = 100000, 
      yref = "paper",
      x0 = x, 
      x1 = x, 
      line = list(color = color)
    )
  }
  
  
  ggplotly(temp3) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
           shapes = list(vline(as.Date("2020-03-01")), vline(as.Date("2020-03-20")),vline(as.Date("2020-06-22")),vline(as.Date("2020-07-06")),vline(as.Date("2020-07-20"))),
           #xaxis = list(title = "",type = "date"),
           yaxis = list(title = ""),
           margin = list(b=100))
  
  
}

# weeklydf_new_positive <- weeklydf_new %>% filter(type == "Case Count")
# weeklydf_new_hos <- weeklydf_new %>% filter(type == "Hospitalization Count")
# weeklydf_new_death <- weeklydf_new %>% filter(type == "Death Count")

new_case_count <- function(){
  
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
  
  
  ggplotly(temp2) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  
  
  # ggplotly(temp2, height = 800) %>% 
  #   layout(legend = list(orientation = "h", x = 0.4, y = -0.2),
  #          grid=list(rows=1, columns=3),
  #          hovermode = "x unified",
  #          xaxis = list(spikemode = "across",
  #                       spikedash = "dash"),
  #          hoverlabel = list(font = list(size = 10)))
}

new_case_rate <- function(){
  
  
  temp4 <- weeklydf_new %>%
    ggplot(aes(x = Date, y = Rate)) +
    geom_line(aes(color = Borough)) +
    geom_point(aes(color = Borough)) +
    facet_wrap(.~newtype, scales = "free") +
    theme(panel.spacing.y=unit(3, "lines")) + 
    theme_minimal() +
    xlab("") +
    ylab("")
  
  ggplotly(temp4) %>% 
    layout(legend = list(orientation = "h", x = 0.4, y = -0.2))
  
  
}

##############
# This is for the moving average for trends tab
# we use borocase_new <- read_csv("data/boro_newcase_trend.csv") %>% select(-1)
#1st clean the data

boro_incidence_daily = borocase_new %>% 
  mutate(boro = factor(boro)) %>% 
  rename(hospitalization_count = hospitalized_count) %>% 
  pivot_longer(case_count:death_count,
               names_to = "type",
               values_to = "count") %>% 
  mutate(#type = str_replace_all(type, "cum_", ""),
    type = str_to_title(str_replace_all(type, "_", " ")),
    newtype = str_replace_all(paste(type, "Rate"),"Count ",""),
    pop_num = ifelse(boro == "Bronx", 1434693, 
                     ifelse(boro == "Brooklyn",2582830, 
                            ifelse(boro == "Manhattan",1611943, 
                                   ifelse(boro == "Staten Island", 476179,
                                          ifelse(boro == "Queens", 2288710, 2288710))))),
    rate = round(count/pop_num*1000000,1)) %>% 
  rename(Borough = boro,
         Date = date_of_interest,
         Count = count,
         Rate = rate) %>% 
  mutate(type = str_replace_all(type,"Case Count", "New Cases"),
         type = str_replace_all(type,"Death Count", "New Deaths"),
         type = str_replace_all(type,"Hospitalization Count", "New Hospitalizations"),
         newtype = str_replace_all(newtype,"Case Rate","Incidence Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Death Rate","New Death Rate (per 100,000 people)"),
         newtype = str_replace_all(newtype,"Hospitalization Rate","New Hospitalization Rate (per 100,000 people)"))%>% 
  mutate(type = factor(type, levels = c("New Cases","New Hospitalizations","New Deaths")),
         newtype = factor(newtype, levels = c("Incidence Rate (per 100,000 people)","New Hospitalization Rate (per 100,000 people)","New Death Rate (per 100,000 people)"))) %>% 
  arrange(type) %>% 
  arrange(newtype)

mvag_case = boro_incidence_daily %>% select(-newtype,-Rate,-pop_num) 
mvag_rate = boro_incidence_daily %>% select(-type,-Count,-pop_num) %>% 
  rename(type = newtype,
         Count = Rate)
boro_incidence_daily_new = rbind(mvag_case,mvag_rate)

####
#NBH_trends1 = boro_incidence_daily %>% distinct(Borough) %>% pull()
#choices_trend_mvag = c("New Cases","New Hospitalizations", "New Deaths",
#                       "Incidence Rate (per 100,000 people)","New Hospitalization Rate (per 100,000 people)", "New Death Rate (per 100,000 people)") 
choices_trend_mvag = c("New Cases","New Hospitalizations", "New Deaths") 

####

#### new data 2020-11-11

new_trend_data = read_csv("./data/data-by-day.csv") %>% 
  janitor::clean_names() %>% 
  rename(date = date_of_interest) %>% 
  mutate(date = as.Date(date, format = "%m/%d/%Y"))

new_trend_data = new_trend_data %>% 
  select(-incomplete,-case_count,-case_count_7day_avg,
         -death_count,-probable_death_count,-death_count_7day_avg,
         -hospitalized_count,-hosp_count_7day_avg) %>% 
  pivot_longer(bx_case_count:si_death_count_7day_avg,
               names_to = "type",
               values_to = "count") %>% 
  separate(type, into = c("boro","display"), sep = 3) %>% 
  mutate(boro = str_replace_all(boro,"bx_","Bronx"),
         boro = str_replace_all(boro,"bk_","Brooklyn"),
         boro = str_replace_all(boro,"mn_","Manhattan"),
         boro = str_replace_all(boro,"qn_","Queens"),
         boro = str_replace_all(boro,"si_","Staten Island")) %>% 
  mutate(display = str_replace_all(display,"case_count", "New Cases"),
         display = str_replace_all(display,"death_count", "New Deaths"),
         display = str_replace_all(display,"hospitalized_count", "New Hospitalizations")
        ) %>% 
  #mutate(display = factor(display, levels = c("New Cases","New Hospitalizations","New Deaths"))) %>% 
  arrange(display) 


### write the functions to draw the map
###positive
positive = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(covid_case_count))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$covid_case_count)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Total Number of COVID-19 Case: ", as.character(data_to_plot_geo$covid_case_count)
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
                     "Death Rate of COVID-19: ", as.character(data_to_plot_geo$covid_death_rate)
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
    mutate(new_case = as.numeric(new_case)) %>% 
    filter(new_case >= 0)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(new_case))
  pal <- colorNumeric("Greens", domain=data_to_plot_geo$new_case)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Total Number of New Case: ", as.character(data_to_plot_geo$new_case)
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
    mutate(incidence_rate = as.numeric(incidence_rate)) %>% 
    filter(incidence_rate >= 0)
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(incidence_rate))
  pal <- colorNumeric("Greens", domain=data_to_plot_geo$incidence_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Incidence Rate: ", as.character(data_to_plot_geo$incidence_rate)
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

### Total test
total_test_fc = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(total_covid_tests))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$total_covid_tests)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "Total Number of PCR Tests: ", as.character(data_to_plot_geo$total_covid_tests)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$total_covid_tests), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$total_covid_tests, 
              position = "bottomright", 
              title = "Number") 
  
  p1
}
### Total test rate
test_rate_fc = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(total_test_rate))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$total_test_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "PCR Test Rate: ", as.character(data_to_plot_geo$total_test_rate)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$total_test_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$total_test_rate, 
              position = "bottomright", 
              title = "Number") 
  
  p1
}
### new test
new_test_fc = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(new_test))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$new_test)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "New PCR Test: ", as.character(data_to_plot_geo$new_test)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$new_test), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$new_test, 
              position = "bottomright", 
              title = "Number") 
  
  p1
}

### new test rate
new_test_rate_fc = function(date){
  data_to_plot = data_to_plot %>% filter(date == max(data_to_plot$date))
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modified_zcta")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(new_test_rate))
  pal <- colorNumeric("Blues", domain=data_to_plot_geo$new_test_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modified_zcta),
                     "<br>", 
                     "New PCR Test Rate: ", as.character(data_to_plot_geo$new_test_rate)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$new_test_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$new_test_rate, 
              position = "bottomright", 
              title = "Number") 
  
  p1
}
###read the antibody data
antibody = read_csv(paste0("./data/antibody_zcta",month(today),day(today),".csv"))
##### Antibody test

antibody_test_rate = function(date){
  data_to_plot = antibody
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modzcta_first")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(test_rate))
  pal <- colorNumeric("Reds", domain=data_to_plot_geo$test_rate)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modzcta_first),
                     "<br>", 
                     "Antibody Test Rate: ", as.character(data_to_plot_geo$test_rate)
  )
  
  p1 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$test_rate), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$test_rate, 
              position = "bottomright", 
              title = "Number")
  p1
}

antibody_num_posit = function(date){
  data_to_plot = antibody
  data_to_plot_geo = geo_join(spdf,data_to_plot,"MODZCTA","modzcta_first")
  data_to_plot_geo = subset(data_to_plot_geo, !is.na(num_peop_pos))
  pal <- colorNumeric("Reds", domain=data_to_plot_geo$num_peop_pos)
  
  popup_sb <- paste0("Neighborhood Name: ", as.character(data_to_plot_geo$neighborhood_name),
                     "<br>", 
                     "MODZCTA: ", as.character(data_to_plot_geo$modzcta_first),
                     "<br>", 
                     "Total Number of Positive Antibody Test: ", as.character(data_to_plot_geo$num_peop_pos)
  )
  
  p2 = leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    setView(lng = -73.99653, lat = 40.75074, zoom = 10) %>% 
    addPolygons(data =  data_to_plot_geo , 
                fillColor = ~pal(data_to_plot_geo$num_peop_pos), 
                fillOpacity = 0.7, 
                weight = 0.2, 
                smoothFactor = 0.2, 
                popup = ~popup_sb) %>%
    addLegend(pal = pal, 
              values =  data_to_plot_geo$num_peop_pos, 
              position = "bottomright", 
              title = "Number")
  p2
}



######projection data#########
train_data =read_xlsx("./data/WeeklyProjections20201204.xlsx",sheet = 3) %>% 
  janitor::clean_names() %>% 
  separate(col = new_cases, into = c("new_cases_value","new_cases_lower","new_cases_upper")) %>%
  mutate(new_cases_value = as.numeric(new_cases_value),
         new_cases_lower = as.numeric(new_cases_lower),
         new_cases_upper = as.numeric(new_cases_upper))%>% 
  separate(col = new_deaths, into = c("new_deaths_value","new_deaths_lower","new_deaths_upper")) %>%
  mutate(new_deaths_value = as.integer(new_deaths_value),
         new_deaths_lower = as.integer(new_deaths_lower),
         new_deaths_upper = as.integer(new_deaths_upper)
  ) %>% 
  separate(col = new_total_hospitalizations, into = c("new_hosp_value","new_hosp_lower","new_hosp_upper")) %>%
  mutate(new_hosp_value = as.integer(new_hosp_value),
         new_hosp_lower = as.integer(new_hosp_lower),
         new_hosp_upper = as.integer(new_hosp_upper),
         date = as.Date(date,format = "%m/%d/%y")
  ) %>% 
  mutate(seasonality = str_replace_all(seasonality,"Seasonality assumed","Seasonality Assumed"),
         seasonality = str_replace_all(seasonality,"No seasonality","No Seasonality"),
         location = str_replace_all(location, "city","City")
  )

project_data = read_xlsx("./data/WeeklyProjections20201204.xlsx",sheet = 4) %>% 
  janitor::clean_names() %>% 
  separate(col = new_infections, into = c("new_infections_value","new_infections_lower","new_infections_upper")) %>%
  mutate(new_infections_value = as.numeric(new_infections_value),
         new_infections_lower = as.numeric(new_infections_lower),
         new_infections_upper = as.numeric(new_infections_upper)
  ) %>% 
  separate(col = new_cases, into = c("new_cases_value","new_cases_lower","new_cases_upper")) %>%
  mutate(new_cases_value = as.numeric(new_cases_value),
         new_cases_lower = as.numeric(new_cases_lower),
         new_cases_upper = as.numeric(new_cases_upper)
  ) %>% 
  separate(col = new_deaths, into = c("new_deaths_value","new_deaths_lower","new_deaths_upper")) %>%
  mutate(new_deaths_value = as.integer(new_deaths_value),
         new_deaths_lower = as.integer(new_deaths_lower),
         new_deaths_upper = as.integer(new_deaths_upper)) %>% 
  separate(col = new_total_hospitalizations, into = c("new_hosp_value","new_hosp_lower","new_hosp_upper")) %>%
  mutate(new_hosp_value = as.integer(new_hosp_value),
         new_hosp_lower = as.integer(new_hosp_lower),
         new_hosp_upper = as.integer(new_hosp_upper),
         date = as.Date(date,format = "%m/%d/%y"),
         week = format(date, format="%Y-%U")) %>% 
  mutate(seasonality = str_replace_all(seasonality,"Seasonality assumed","Seasonality Assumed"),
         seasonality = str_replace_all(seasonality,"No seasonality","No Seasonality"),
         location = str_replace_all(location, "city","City")
  ) %>% 
  mutate(loca = location) %>% 
  separate(loca,into = c("uhf","name")) %>% 
  mutate(uhf = as.integer(uhf)) %>% 
  mutate(intervention = str_replace_all(intervention, "Worst case", "Worst Case"),
         intervention = str_replace_all(intervention, "Ctrl 1: moderate redn in trans", "Ctrl 1"),
         intervention = str_replace_all(intervention, "Ctrl 3: large redn in trans", "Ctrl 2"),
         intervention = str_replace_all(intervention, "Rebound 1: moderate incr in trans", "Rebound 1"),
         intervention = str_replace_all(intervention, "Rebound 2: large incr in trans", "Rebound 2"))

zipcodedata = read_csv("./data/nyc.zips.uhfs.csv") %>% 
  mutate(num = "zipcode") %>% 
  pivot_wider(values_from = zip,
              names_from = num) 

projt_data = left_join(project_data, zipcodedata,by = "uhf")

projt_data = projt_data %>% 
  mutate(zipcode = str_replace_all(zipcode,"NULL","ALL")) %>% 
  mutate(zipcode = as.character(zipcode),
         zipcode = str_replace_all(zipcode,"c",""),
         zipcode = str_replace_all(zipcode,"\\(",""),
         zipcode = str_replace_all(zipcode,"\\)",""))


#select seasonality
season = train_data %>% distinct(seasonality) %>% pull()
#select location
location = train_data %>% distinct(location) %>% pull()
#select intervention
intervention = c("As Is","Rebound 1","Rebound 2","Ctrl 1","Ctrl 2","Worst Case")
#choose the zcta
zcta = projt_data %>% distinct(zipcode) %>% pull()




####################

## ui
ui <- navbarPage(
  theme = "shiny.css",
  title = div(img(src='whitelogo.png',style="margin-top: -14px; padding-right:10px;padding-bottom:10px", height = 50)),
  windowTitle = "NYC covid-19 dashboard",
  id = 'menus',
  tabPanel('Home',
           shinyjs::useShinyjs(),
           fluidRow(
             column(width = 5, offset = 1, div(img(src = "newlogo3.png", height = "100%",width = "100%"),
                                               style="text-align: center;")),
             column(width = 5,  div(img(src = "HomePagepic.png", height = "100%",width = "100%"),
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
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )
           
  ),
  
  
  tabPanel(
    # Application title
    title= "COVID-19 Tracker",
    # Sidebar with a slider input for number of bins 
    fluidRow(
      column(width = 10, offset = 1, h2("NYC COVID-19 Tracker")),
      
      #column(width = 10, offset = 1, h4("Summary Table")),
      
      #column(5, offset = 1,align="center",tableOutput(outputId = "summarytable1")),
      #column(5,offset = 1,align="center",tableOutput(outputId = "summarytable2")),
      
      column(width = 10, offset = 1, h4("Tracking yesterday’s COVID-19 cases, deaths, and tests by NYC ZIP Code Tabulation Areas (ZCTAs).")),
      
      column(width = 10, offset = 1, span(htmlOutput("Trackertext"), style="font-size: 15px; line-height:150%")),
      column(width = 10, offset = 1, helpText(paste("Last updated: ", max(data_today$date)))),
      column(width = 10, offset = 1, align="center",DT::dataTableOutput("table")),
      column(width = 10, offset = 1, helpText("* per 100,000 people")),
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
             style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
             
    )
  ),
  
  tabPanel(
    title = "COVID-19 Distribution",
    column(width = 10, offset = 1, h2("NYC COVID-19 Data by Neighborhoods and Demographics")),
    column(width = 10, offset = 1, h4("COVID-19 cases and deaths by NYC ZIP Code Tabulation Areas (ZCTAs)")),
    column(width = 10, offset = 1, span(htmlOutput("Distributionmaptext"), 
                                        style="font-size: 15px;  line-height:150%")),
    column(10, offset = 1, helpText(paste("Last updated: ", max(data_to_plot$date)))),
    column(width = 10,offset = 1,
           sidebarLayout(
             
             sidebarPanel(
               radioButtons(inputId = "outcome_selection",
                            label =  "Data Display:",   
                            c("Total Cases" = "positive",
                              "Case Rate (per 100,000 people)" = "case_rate", 
                              "Total Deaths" = "death_count", 
                              "Death Rate (per 100,000 people)" = "death_rate",
                              "New Cases" = "newcase",
                              "Incidence Rate (per 100,000 people)" = "incidencerate")),
               
               
               span(htmlOutput("Distributionmap_help_text"), 
                    style="font-size: 14px;line-height:150% ; color:grey")
             )
             ,
             
             mainPanel(column(10,leafletOutput(outputId = "map",width="120%",height="465px"))),
             position = c("left","right")
           )),
    ###2020-11-03 added
    column(width = 10, offset = 1, h4("COVID-19 Tests by NYC ZIP Code Tabulation Areas (ZCTAs)")),
    column(width = 10, offset = 1, span(htmlOutput("Distributionmaptext2"), 
                                        style="font-size: 15px;  line-height:150%")),
    column(10, offset = 1, helpText(paste("Last updated: ", max(data_to_plot$date)))),
    column(width = 10,offset = 1,
           sidebarLayout(
             
             sidebarPanel(
               radioButtons(inputId = "outcome_selection_test",
                            label =  "Data Display:",   
                            c("Total PCR Tests" = "total_test_fc",
                              "PCR Test Rate (per 100,000 people)" = "test_rate_fc", 
                              "New PCR Tests" = "new_test_fc", 
                              "New PCR Tests Rate (per 100,000 people)" = "new_test_rate_fc"
                              #,
                              #"Total Antibody Tests" = "antibody_num_posit",
                              #"Antibody Test Rate (per 100,000 people)" = "antibody_test_rate"
                              )),
               br(),
         
               span(htmlOutput("Distributionmap_help_text2"), 
                    style="font-size: 14px;line-height:150% ; color:grey")
             )
             ,
             
             mainPanel(column(10,leafletOutput(outputId = "testmap",width="120%",height="465px"))),
             position = c("left","right")
           )),
    
    
    
    br(),
    fluidPage(
      
      column(10,offset = 1, h4("COVID-19 cases, hospitalizations, and deaths by age groups and NYC boroughs.")),
      column(10, offset = 1, span(htmlOutput("DistribAgetext"), style="font-size: 15px; line-height:150%")),
      column(10, offset = 1,
             helpText(paste0("Age data last updated: ",as.character(max(byagetoday$day))))),
      # new on 11/19/2020
      column(4, offset = 1,
             selectInput("outcome_age1", 
                         label = "Data Display", 
                         choices = outcome_age1
             )),
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
      
      br(),
      column(10,offset = 1, h4("COVID-19 cases, hospitalizations, and deaths by gender groups and NYC boroughs.")),
      
      column(10, offset = 1, span(htmlOutput("DistribSextext"), style="font-size: 15px; line-height:150%")),
      column(10,offset = 1,
             helpText(paste0("Gender data last updated: ",as.character(max(bysextoday$day))))),
      
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
      
      
      br(),
      column(10,offset = 1, h4("COVID-19 cases, hospitalizations, and deaths by race groups and NYC boroughs.")),
      
      column(10, offset = 1, span(htmlOutput("DistribRacetext"), style="font-size: 15px; line-height:150%")),
      column(10,offset = 1,
             helpText(paste0("Race data last updated: ",as.character(max(byracetoday$day))))),
      
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
             style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
             
    )
  ),
  
  tabPanel(title="COVID-19 Trends",
           
           fluidRow(column(10, offset = 1, h2("NYC COVID-19 Trends"))),
           ######### Added by 2020-10-03 ############
           
           ####
           fluidRow(
            column(10, offset = 1, h4("Time trends by NYC ZIP Code Tabulation Areas (ZCTAs) with daily data and 7-day average")),
             column(10, offset = 1, helpText(paste("Last updated:",max(weeklydf$day)))),
             column(width = 4, offset = 1, selectInput("character_time_zip",
                                                       "Data Display",
                                                       c("New Cases" = "newcase",
                                                         "Incidence Rate (per 100,000 people)" = "incdrate",
                                                         "Total Cases" = "pocase", 
                                                         "Case Rate (per 100,000 people)" = "porate", 
                                                         "Total Deaths" = "death", 
                                                         "Death Rate(per 100,000 people)" = "derate"
                                                         
                                                         
                                                       ),
                                                       selected = NULL)),
             column(width = 5, "Weekly data and trends on COVID-19 in each of the NYC ZIP Code Tabulation Areas (ZCTAs). Data are updated every Sunday. 
                    Select available display options and choose a ZCTA to display the data.")
           ),
           
           
           #### Cumulative Cases Count
           conditionalPanel(
             condition = "input.character_time_zip == 'pocase'",
            
             fluidRow(
              column(width = 4, offset = 1,
                      pickerInput("zip_nbh1", 
                                  label = "Choose a ZCTA", 
                                  choices =zip_nbh,
                                  multiple = FALSE)),
               column(6,plotlyOutput("pocase", width="100%",height="500px"))
             )),
           
           #### Death Count
           conditionalPanel(
             condition = "input.character_time_zip == 'death'",
             #column(10, offset = 1, h4("Death Count")),
             fluidRow(
               column(width = 4,offset = 1,
                      pickerInput("zip_nbh2", label = "Choose a ZCTA", 
                                  choices =zip_nbh, 
                                  multiple = FALSE)),
               column(6,plotlyOutput("death", width="100%",height="500px")))
           ),
           
           #### Case Rate
           conditionalPanel(
             condition = "input.character_time_zip == 'porate'",
             #column(10, offset = 1, h4("Case Rate")),
             fluidRow(
               column(width = 4,offset = 1,
                      pickerInput("zip_nbh3", 
                                  label = "Choose a ZCTA", 
                                  choices =zip_nbh, 
                                  multiple = FALSE)),
               column(6, plotlyOutput("porate", width="100%",height="500px")))
           ),
           
           #### Death Rate
           conditionalPanel(
             condition = "input.character_time_zip == 'derate'",
            # #column(10, offset= 1, h4("Death Rate")),
             fluidRow(
               column(width = 4,offset = 1,
                      pickerInput("zip_nbh4", 
                                  label = "Choose a ZCTA", 
                                  choices =zip_nbh,
                                  multiple = FALSE)),
               column(6, plotlyOutput("derate", width="100%",height="500px")))
           ),
           
           #### New cases
           conditionalPanel(
            condition = "input.character_time_zip == 'newcase'",
            # #column(10, offset = 1, h4("New cases")),
             fluidRow(
               column(width = 4,offset = 1,pickerInput("zip_nbh5", 
                                                       label = "Choose a ZCTA", 
                                                       choices =zip_nbh,
                                                       multiple = FALSE)),
               column(6, plotlyOutput("newcases", width="100%",height="500px")))
           ),
           ##Incidence rate
           conditionalPanel(
             condition = "input.character_time_zip == 'incdrate'",
             #column(10, offset = 1, h4("Incidence Rate")),
             fluidRow(
               column(width = 4,offset = 1,pickerInput("zip_nbh6", 
                                                       label = "Choose a ZCTA", 
                                                       choices =zip_nbh,
                                                      multiple = FALSE)),
               column(6, plotlyOutput("incdrate", width="100%",height="500px")))
           ),
           
           
           #hr(),
           
           ######### Added by 2020-09-03 ############
           fluidRow(column(10, offset = 1, h4("Time trends of new cases, new hospitalizations, and new deaths by borough with daily data and 7-day moving average estimate"))),
           fluidRow(column(width = 10, offset = 1, span(htmlOutput("mvag_text"), style="font-size: 15px; line-height:150%"))),
           fluidRow(column(width = 10, offset = 1, helpText(paste("Last updated : ",max(new_trend_data$date))))),
           br(),
           fluidRow(column(width = 10,offset = 1 ,selectInput("choices_trend_mvag", 
                                                              label = "Data Display", 
                                                              choices = choices_trend_mvag
           )),
           
           column(width = 10,offset = 1 , 
                  plotlyOutput("trendsmoving_avg",width = "100%", height = "600px"))
           
           
           ),
           br(),
           #fluidRow(
           #         column(width = 10,offset = 1 ,selectInput("choices_trend_mvag_rate", 
           #                                                   label = "Data Display", 
           #                                                   choices = choices_trend_mvag_rate
           #)),
           #column(width = 10,offset = 1 , 
           #        plotlyOutput("trendsmoving_avg_rate",width = "100%", height = "600px"))
           
           #),
           br(),
           ##################
           fluidRow(column(10, offset = 1, h4("Time trends of cumulative counts of cases, hospitalizations, and deaths by borough with weekly summary data"))),
           
           fluidRow(column(width = 10, offset = 1, span(htmlOutput("borotrendtext"), style="font-size: 15px; line-height:150%"))),
           fluidRow(column(width = 10, offset = 1, helpText(paste("Last updated :",max(weeklydf_cum$Date))))), ## need to update !!!11-08
           br(),
           fluidRow(column(width = 2,offset = 1,
                           # radioButtons(inputId = "selection",
                           #              label =  "Data Display:",   
                           #              c("Total Count" = "cum_case_count",
                           #                "Incidence Count" = "new_case_count",
                           #                "Total Rate" = "cum_case_rate",
                           #                "Incidence Rate" = "new_case_rate")),
                           radioButtons(inputId = "selection1",
                                        label =  "Data Display:",   
                                        c("Total Count" = "cum_case_count",
                                          "Incidence Count" = "new_case_count"))),
                    column(width = 8, plotlyOutput(outputId = "boro_cases1"))),
           
           fluidRow(column(width = 2,offset = 1,
                           # radioButtons(inputId = "selection",
                           #              label =  "Data Display:",   
                           #              c("Total Count" = "cum_case_count",
                           #                "Incidence Count" = "new_case_count",
                           #                "Total Rate" = "cum_case_rate",
                           #                "Incidence Rate" = "new_case_rate")),
                           radioButtons(inputId = "selection2",
                                        label =  "Data Display:",   
                                        c("Total Rate" = "cum_case_rate",
                                          "Incidence Rate" = "new_case_rate"))),
                    column(width = 8, plotlyOutput(outputId = "boro_cases2"))),
           fluidRow(column(width = 10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data"))),
           
           hr(),
           ####zipcode trends
           ###New update###
           
           fluidRow(column(10, offset = 1, h4("Map videos of new cases, cumulative cases, and cumulative deaths by NYC Zip Code Tabulation Areas (ZCTAs)"))),
           fluidRow(column(width = 10, offset = 1, span(htmlOutput("trends_video_text"), style="font-size: 15px; line-height:150%"))),
           
           fluidRow(column(width = 10, offset = 1,htmlOutput("mapvideo"))),
           
           #fluidRow(column(10, offset = 1, h4("New Cases Map video by ZCTAs"))),
           fluidRow(column(width = 10, offset = 1,htmlOutput("mapvideo_newcase"))),
           #fluidRow(column(10, offset = 1, h4("Deaths Map video by ZCTAs"))),
           fluidRow(column(width = 10, offset = 1,htmlOutput("mapvideo_death"))),
           #fluidRow(column(width = 10, offset = 1, helpText(paste("Last updated : ",max(boro_incidence_daily$Date))))),
           br(),
           
           
           #####
           fluidRow(
             column(10, offset = 1, h4("Time trends by demographics and borough with weekly summary data")),
             ###update 11-08!!!!!
             column(10, offset = 1, helpText(paste("Last updated: 2020-11-08"))),
             column(width = 4, offset = 1, selectInput("character_timetrend",
                                                       "Data Display",
                                                       c("Total Cases" = "pocase_tt", 
                                                         "Total Deaths" = "death", 
                                                         "Case Rate (per 100,000 people)" = "porate", 
                                                         "Death Rate (per 100,000 people)" = "derate"
                                                         
                                                       ),
                                                       selected = NULL)),
             column(width = 5, "A look at how COVID-19 cases and deaths count and rate change over time by age, gender and race/ethnicity in each NYC borough. 
             Data are updated every Sunday. Select available display options to visualize the data.
")
           ),
           
           #### Cumulative Cases Count
           conditionalPanel(
             condition = "input.character_timetrend == 'pocase_tt'",
             
             
             fluidRow(
               column(10, offset = 1, h5("By Age Groups")),
               column(10, offset = 1, plotlyOutput("tt_age_cac", width="100%",height="80%")),
               column(10, offset = 1, h5("By Gender")),
               column(10, offset = 1, plotlyOutput("tt_sex_cac", width="100%",height="80%")),
               column(10, offset = 1, h5("By Race/Ethnicity")),
               column(10, offset = 1, plotlyOutput("tt_race_cac", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
           #### Death Count
           conditionalPanel(
             condition = "input.character_timetrend == 'death'",
             #fluidRow(column(10, offset = 1, h4("Death Count"))),
             
             fluidRow(
               column(10, offset = 1, h5("By Age Groups")),
               column(10, offset = 1, plotlyOutput("tt_age_dec", width="100%",height="80%")),
               column(10, offset = 1, h5("By Gender")),
               column(10, offset = 1, plotlyOutput("tt_sex_dec", width="100%",height="80%")),
               column(10, offset = 1, h5("By Race/Ethnicity")),
               column(10, offset = 1, plotlyOutput("tt_race_dec", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
           #### Case Rate
           conditionalPanel(
             condition = "input.character_timetrend == 'porate'",
             #fluidRow(column(10, offset = 1, h4("Case Rate (per 100,000 people)"))),
             
             fluidRow(
               column(10, offset = 1, h5("By Age Groups")),
               column(10, offset = 1, plotlyOutput("tt_age_carate", width="100%",height="80%")),
               column(10, offset = 1, h5("By Gender")),
               column(10, offset = 1, plotlyOutput("tt_sex_carate", width="100%",height="80%")),
               column(10, offset = 1, h5("By Race/Ethnicity")),
               column(10, offset = 1, plotlyOutput("tt_race_carate", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
           #### Death Rate
           conditionalPanel(
             condition = "input.character_timetrend == 'derate'",
             #fluidRow(column(10, offset = 1, h4("Death Rate (per 100,000 people)"))),
             
             fluidRow(
               column(10, offset = 1, h5("By Age Groups")),
               column(10, offset = 1, plotlyOutput("tt_age_derate", width="100%",height="80%")),
               column(10, offset = 1, h5("By Gender")),
               column(10, offset = 1, plotlyOutput("tt_sex_derate", width="100%",height="80%")),
               column(10, offset = 1, h5("By Race/Ethnicity")),
               column(10, offset = 1, plotlyOutput("tt_race_derate", width="100%",height="80%")),
               column(10, offset = 1, helpText("Data Sources: https://github.com/nychealth/coronavirus-data")))
           ),
           
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
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )
  ),
  
  tabPanel(title = "COVID-19 Projection",
           fluidRow(column(10, offset = 1, h2("NYC COVID-19 Projection")),
                    #column(width = 10, offset = 1, h4("Model by Columbia")),
                    br(),
                    column(width = 10, offset = 1, span(htmlOutput("Projecttext"), style="font-size: 15px; line-height:150%")),
                    column(width = 10, offset = 1, helpText(paste("Last updated: ", min(project_data$date)))),
                    column(width = 10, offset = 1,
                           sidebarLayout(
                             
                             sidebarPanel(
                               
                               selectInput("season", 
                                           label = "Choose Seasonality Assumption", 
                                           choices = season),
                               selectInput("interve", 
                                           label = "Choose Intervention", 
                                           choices = intervention),
                               selectInput("loc_proj", 
                                           label = "Choose a Location", 
                                           choices = location),
                               
                              #selectInput("zcta_proj", 
                              #             label = "Choose a Location", 
                              #             choices = zcta),
                               htmlOutput("zipuhf"),
                               
                               htmlOutput("uhftext")
                               
                             ),
                             
                             mainPanel(
                               column(width = 12, span(htmlOutput("Projecttext2"), style="font-size: 15px; line-height:150%"))),
                             
                             position = c("left","right")
                           )),
                    
                    
                    column(10, offset = 1, h3("New Infections")),
                    column(10, offset = 1, plotlyOutput("proj_line_infec", width="100%",height="80%")),
                    column(10, offset = 1, h3("New Cases")),
                    column(10, offset = 1, plotlyOutput("proj_line_case", width="100%",height="80%")),
                    column(10, offset = 1, h3("New Total Hospitalizations")),
                    column(10, offset = 1, plotlyOutput("proj_line_hosp", width="100%",height="80%")),
                    column(10, offset = 1, h3("New Deaths")),
                    column(10, offset = 1, plotlyOutput("proj_line_deat", width="100%",height="80%")),
                    
                    column(width = 10, offset = 1, span(htmlOutput("ProRefertext"), style="font-size: 12px; line-height:150%")),
                    
                    #column(10, offset = 1, helpText("Data Sources: https://github.com/wan-yang/COLUMBIA-COVID19-PROJECTIONS-FOR-NYC"))
           ),
           
           
           fluidRow(align="center",
                    span(htmlOutput("bannertext7", style="color:white;font-family: sans-serif, Helvetica Neue, Arial;
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
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )),
  
  tabPanel(title = "Neighborhoods",
           fluidRow(column(10, offset = 1, h2("NYC Neighborhood Characteristics"))),
           hr(),
           fluidRow(
             column(width = 4, offset = 1, selectInput("character",
                                                       "Data Display (more coming soon)",
                                                       c("Race" = "race",
                                                         "Income" = "income",
                                                         "Household Size" = "house"))),
             column(width = 6, "Select available data display options to see and compare neighborhood characteristics. Data sources: Census 2010.")
           ),
           hr(),
           
           #### Race
           conditionalPanel(
             condition = "input.character == 'race'",
             
             fluidRow(
               column(10, offset = 1, h4("Compare a NYC ZIP Code Tabulation Area (ZCTA) to the NYC borough it belongs to and NYC as a whole.")),
               column(width = 4,offset = 1,selectInput("nbhid1", 
                                                       label = "Choose a ZCTA", 
                                                       choices =nbh_name, 
                                                       selected = NULL)),
               
               column(width = 10, offset = 1, plotlyOutput("race_nbh",width = "100%"))
             ),
             hr(),
             column(10, offset = 1, h4(" Compare neighborhood characteristics among ZCTAs")),
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
                                           choices = race_name,
                                           multiple = TRUE,
                                           selected = race_name[1],
                                           options = list(`actions-box` = TRUE)
                               ))
                        
                      )),
               column(width = 6,leafletOutput("race_map", width="100%",height="700px")))
           ),
           
           
           #### Household
           conditionalPanel(
             condition = "input.character == 'house'",
             
             fluidRow(
               column(10, offset = 1, h4("Compare a NYC ZIP Code Tabulation Area (ZCTA) to the NYC borough it belongs to and NYC as a whole.")),
               
               column(width = 4, offset = 1,selectInput("nbhid2", 
                                                        label = "Choose a ZCTA", 
                                                        choices =nbh_name, 
                                                        selected = NULL)),
               column(width = 10, offset = 1, plotlyOutput("household_nbh", width="100%"))),
             
             
             hr(),
             column(10, offset = 1, h4(" Compare neighborhood characteristics among ZCTAs")),
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
               column(width = 6,leafletOutput("household_map", width="100%",height="700px")))
             
           ),
           
           #### income
           conditionalPanel(
             condition = "input.character == 'income'",
             
             fluidRow(
               column(10, offset = 1, h4("Compare a NYC ZIP Code Tabulation Area (ZCTA) to the NYC borough it belongs to and NYC as a whole.")),
               
               column(width = 4,offset = 1,selectInput("nbhid3", 
                                                       label = "Choose a ZCTA", 
                                                       choices =nbh_name, 
                                                       selected = NULL)),
               column(width = 10, offset = 2,
                      plotlyOutput("income_nbh", width="80%",height="600px"))),
             
             hr(),
             
             
             fluidRow(
               column(10, offset = 1, h4(" Compare neighborhood characteristics among ZCTAs")),
               column(4,offset = 1, "Use this map to see how the selected neighborhood characteristics vary by NYC ZCTAs."),
               column(width = 6, leafletOutput("income_map", width="100%",height="700px")))
             
           ),
           
           br(),
           br(),br(),
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
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
           )),
  
  tabPanel("About Us",
           fluidRow(column(10, offset = 1, h2("About Us")),
                    column(10, offset = 1, div(img(src = "msph-n-bios2.JPG", height = "100%",width = "100%"),
                                               style="text-align: center;")),
                    column(10, offset = 1,helpText("MSPH photo source: https://globalcenters.columbia.edu/content/yusuf-hamied-fellowships-program")),
                    column(10, offset = 1,span(htmlOutput("abouttext",style = "font-size: 15px; line-height:150%"))),
                    column(10, offset = 1,span(htmlOutput("abouttext2",style = "font-size: 15px; line-height:150%")))),
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
                    style = "background-color:#225091;padding-top:40px;padding-bottom:40px;"
                    
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
  output$bannertext7 = renderText({
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
      "The NYC Neighborhoods COVID-19 Dashboard is a tracker and data visualization tool to provide continuously updated sources of COVID-19 data in NYC for lay public, essential workers, policymakers, and researchers. 
 <br><br>
 
 There are five tools available (located at the top navigation menu): <br>
<b> <span>&#8226;</span>  COVID-19 Tracker </b> provides daily tracking of the local development for COVID-19 cases, deaths, and tests in 177 NYC ZIP Code Tabulation Areas (ZCTAs). <br>
<b> <span>&#8226;</span>  COVID-19 Distribution </b> provides a data visualization of COVID-19 cases, hospitalizations, and deaths in NYC ZCTAs and by age, gender, and race/ethnicity. <br>
<b> <span>&#8226;</span>  COVID-19 Trends </b> shows the time trends for COVID-19 cases, hospitalizations, and deaths by NYC boroughs, ZCTAs, and demographics. <br>
<b> <span>&#8226;</span>  COVID-19 Projection </b> provides projection of COVID-19 new cases, new hospitalizations, and new deaths in the next 8 weeks by NYC United Hospital Fund neighborhood. <br>
<b> <span>&#8226;</span>  Neighborhoods </b> shows and compares the neighborhood characteristics of NYC ZCTAs."
      
      
    )
  })
  
  
  output$Trackertext = renderText({
    return(
      "Data are presented as total cases, new cases, incidence rate (per 100,000 people), total deaths, new deaths, and total tests. 
      <b> Total cases, total deaths, and total tests </b> are total cumulative numbers of COVID-19 cases, deaths, and tests, respectively. 
      <b> New cases and new deaths </b> are the single day new reported confirmed COVID-19 positive cases and deaths. There might be some negative values in new cases and new deaths due to adjustments to lags with surveillance data.
      <b> Incidence rate </b> is calculated as new cases divided by ZCTA population size and multiplied by 100,000, and interpreted as daily new cases per 100,000 people in a ZCTA. 
      The ZCTA population size is based on the intercensal population estimates from the U.S. Census Bureau and NYC Department of City Planning. 
      Population size data were last updated in 2019. 
      <br> <br>
      Choose number of records to show in each page or search neighborhoods using the search box. The data can be sorted within columns. 

      <br> <br>
      "
    )
  })
  output$Distributionmap_help_text = renderText({
    return(
      "<span>&#8226;</span>  Total cases and total deaths are total cumulative numbers of COVID-19 cases and deaths since the start of the outbreak. 
      <br>
     <span>&#8226;</span>  New cases are single day new reported confirmed COVID-19 cases on the updated date.  
     <br>
     <span>&#8226;</span>  Case rate, death rate, and incidence rate are calculated using total cases, total deaths, and new cases divided by ZCTA population size and multiplied by 100,000.

    "
    )
  })
  
  output$Distributionmap_help_text2 = renderText({
    return(
      "<span>&#8226;</span>  Total tests are total cumulative numbers of people tested for COVID-19 with a PCR test since the start of the outbreak. 
      <br>
     <span>&#8226;</span>  New tests are single day new reported numbers of people tested for COVID-19 with a PCR test on the updated date.  
     <br>
     <span>&#8226;</span>  Test rate, and new test rate are calculated using total tests, and new tests divided by ZCTA population size and multiplied by 100,000.
"
    )
  })
  
  output$borotrendtext = renderText({
    return(
      "A look at how cumulative counts of COVID-19 cases, hospitalizations, and deaths change over time by NYC borough. 
      Total count shows cumulative counts of COVID-19 cases, hospitalizations, and deaths since the start of the outbreak. 
      Total rate is calculated using total count divided by borough population size and multiplied by 100,000. 
      Data are updated every Sunday. 
      Select available display options to visualize the data.
"     
    )
  })
  
  output$Distributionmaptext = renderText({
    return(
      "This map may be used to visualize how COVID-19 cases and deaths vary by NYC ZCTAs. 
      Select available display options to visualize the data.
      Click a ZCTA on the map to see the data. These data are updated daily.<br> "
    )
  })
  
  output$Distributionmaptext2 = renderText({
    return(
      "This map may be used to visualize how COVID-19 PCR tests vary by NYC ZCTAs. 
      Select available display options to visualize the data.
      Click a ZCTA on the map to see the data. These data are updated daily.<br> "
    )
  })
  
  
  output$DistribAgetext = renderText({
    return("The bar charts present counts and rates per 100,000 people. 
    The pie charts show percentage of age groups in each NYC borough. 
    Total hospitalizations are total hospitalized COVID-19 patients since the start of the outbreak. 
    Hospitalization rate is calculated using total hospitalizations divided by borough population size and multiplied by 100,000.
<br>")
    
  })
  
  output$DistribRacetext = renderText({
    return("See how COVID-19 cases, hospitalization and deaths differ by race/ethnicity and NYC boroughs. 
           The bar charts present counts and rates per 100,000 people. 
           The pie charts show percentage of race and ethnicity groups in each NYC borough.<br>")
  })
  
  output$DistribSextext = renderText({
    return("See how COVID-19 cases, hospitalization and deaths differ by Gender and NYC boroughs. 
    The bar charts present counts and rates per 100,000 people.
    The pie charts show percentage of gender groups in each NYC borough. <br>
")
  })
  
  output$NeighborhoodsText = renderText({
    return("Choose a NYC ZCTA neighborhood. See how the selected neighborhood differs from NYC as a whole and the NYC borough it belongs to. 
Keep one decimal for all numbers.")
  })
  
  
  output$abouttext = renderUI({
    urlzzq = a("Ziqi Zhou",href = "https://www.linkedin.com/in/ziqi-zhou-1b448a145/")
    urlzmy = a("Mengyu Zhang",href = "https://www.linkedin.com/in/mengyu-zhang-553421197")
    urlyyz = a("Yuanzhi Yu", href = "https://www.linkedin.com/in/yuanzhi（fisher）-yu-a1529918a/")
    urlqyc = a("Yuchen Qi",href = "https://www.linkedin.com/in/yuchen-qi/")
    urlcqx = a("Qixuan Chen",href = "https://www.publichealth.columbia.edu/people/our-faculty/qc2138")
    
    tagList("The NYC Neighborhoods COVID-19 Dashboard is developed by Professor Qixuan Chen’s research team in the Department of Biostatistics at Columbia University Mailman School of Public Health: 
    ",urlzzq,",",urlzmy,",",urlyyz,",",urlqyc,",",urlcqx,"."
    )
    
  })  
  
  output$abouttext2 = renderText({
    return("<br>
    We are thankful to Professor Wan Yang for allowing us to use her COVID-19 projection data, Cynthia Liu who designed the dashboard logo, and our colleagues in the Mailman School of Public Health for comments and suggestions. 
    We hope that you find the dashboard useful.
    <br><br>
    Disclaimer: We assume no responsibility or liability for any errors or omissions in the content of this site. If you believe there is an error in our data, please feel free to contact us. 
")
  })
  
  ###########
  ##
  
  output$summarytable1 <- renderTable(
    summary_table1
  )
  
  output$summarytable2 <- renderTable(
    summary_table2
  )
  
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
  
  output$testmap = renderLeaflet({
    
    plot = switch (input$outcome_selection_test,
                   total_test_fc = total_test_fc,
                   test_rate_fc = test_rate_fc,
                   new_test_fc = new_test_fc,
                   new_test_rate_fc = new_test_rate_fc
                   #antibody_num_posit = antibody_num_posit,
                   #antibody_test_rate = antibody_test_rate
    )
    
    plot(input$date_choice)
  })

  output$barchart_age = renderPlotly({
    
    a =  byagetoday %>% filter(day == max(byagetoday$day) & group != "Boroughwide"  
                               & outcome == input$outcome_age1) %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(panel.spacing.y=unit(1, "lines")) + 
      xlab("") +
      ylab("") + 
      facet_wrap(outcome ~ ., scales = "free")
    
    ggplotly(a) %>% layout(legend = list(title=list(text='Age'),orientation = "h", x = 0.4, y = 1.2))
    
  })
  
  output$barchart_sex = renderPlotly({
    
    b =  bysextoday %>%filter(day == max(bysextoday$day) & group != "Boroughwide") %>% 
      ggplot(aes(fill = group, y = count, x = boro)) + 
      geom_bar(position="stack", stat="identity") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(legend.title = element_blank()) +
      theme(panel.spacing.y=unit(1, "lines")) + 
      xlab("") +
      ylab("") + 
      facet_wrap(outcome ~ ., scales = "free")
    
    ggplotly(b) %>% layout(legend = list(orientation = "h", x = 0.5, y = 1.2))
    
    
  })
  
  
  output$barchart_race = renderPlotly({
    
    c =  byracetoday %>%  filter(day == max(byracetoday$day) & group != "Boroughwide") %>% 
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
      theme(panel.spacing.y=unit(1, "lines")) + 
      xlab("") +
      ylab("") + 
      facet_wrap(outcome ~ ., scales = "free")
    
    ggplotly(c) %>% layout(legend = list(orientation = "h", x = 0.5, y = 1.2))
    
    
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
    
    pie1data = bysextoday %>% 
      filter(boro == "Bronx"& day == max(bysextoday$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie3data = bysextoday %>% 
      filter(boro == "Manhattan"& day == max(bysextoday$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie2data = bysextoday %>% 
      filter(boro == "Brooklyn"& day == max(bysextoday$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie4data = bysextoday %>% 
      filter(boro == "Queens"& day == max(bysextoday$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    pie5data = bysextoday %>% 
      filter(boro == "Staten Island"& day == max(bysextoday$day) &group != "Boroughwide" & outcome ==input$outcome_sex)
    
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
    
    pie1data = byracetoday %>% 
      filter(boro == "Bronx"& day == max(byracetoday$day) &group != "Boroughwide" & outcome ==input$outcome_race) %>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie3data = byracetoday %>% 
      filter(boro == "Manhattan"& day == max(byracetoday$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie2data = byracetoday %>% 
      filter(boro == "Brooklyn"& day == max(byracetoday$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie4data = byracetoday %>% 
      filter(boro == "Queens"& day == max(byracetoday$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
      mutate(group = factor(group, levels = c("White", "Black/African-American","Asian/Pacific-Islander","Hispanic/Latino"))) %>% 
      arrange(group)
    pie5data = byracetoday %>% 
      filter(boro == "Staten Island"& day == max(byracetoday$day) &group != "Boroughwide" & outcome ==input$outcome_race)%>% 
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
                labels = str_replace_all(race_nyc$race,"_"," "),
                values = race_nyc$pop,
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
      select(zipcode:total, str_replace_all(str_to_lower(input$raceid)," ","_"))
    
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
                title = "Proportions in ZCTAs")
    
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
      mutate(level = c("ZCTA", "Borough", "NYC")) %>% 
      mutate(level = factor(level, levels = c("ZCTA", "Borough", "NYC"))) %>% 
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
                title = "Proportions in ZCTAs")
    
  })
  
  
  output$household_nbh <- renderPlotly({
    
    house_nbh = household_for_pie %>% 
      filter(neighborhood_name == input$nbhid2) %>%
      pivot_longer(size_1:size_7_or_more, names_to = "size", values_to = "number") %>%
      group_by(neighborhood_name, size) %>% 
      summarise(num = sum(number))  %>% 
      mutate(size = str_replace_all(size,"size_1","1 person"),
             size = str_replace_all(size,"size_2","2 persons"),
             size = str_replace_all(size,"size_3","3 persons"),
             size = str_replace_all(size,"size_4","4 persons"),
             size = str_replace_all(size,"size_5","5 persons"),
             size = str_replace_all(size,"size_6","6 persons"),
             size = str_replace_all(size,"size_7_or_more","7 persons or more")) %>% 
      mutate(size = factor(size)) %>% 
      drop_na()
    
    which_boro = race %>% filter(neighborhood_name == input$nbhid2) %>% select(borough_group) %>% unique()
    
    house_gp = household_for_pie %>% 
      filter(borough_group == which_boro$borough_group) %>% 
      pivot_longer(size_1:size_7_or_more, names_to = "size", values_to = "number") %>% 
      group_by(size) %>% 
      summarise(num = sum(number)) %>% 
      mutate(size = str_replace_all(size,"size_1","1 person"),
             size = str_replace_all(size,"size_2","2 persons"),
             size = str_replace_all(size,"size_3","3 persons"),
             size = str_replace_all(size,"size_4","4 persons"),
             size = str_replace_all(size,"size_5","5 persons"),
             size = str_replace_all(size,"size_6","6 persons"),
             size = str_replace_all(size,"size_7_or_more","7 persons or more")) %>% 
      mutate(size = factor(size)) %>% 
      drop_na()
    
    
    house_nyc = household_for_pie %>% 
      pivot_longer(size_1:size_7_or_more, names_to = "size", values_to = "number") %>%
      group_by(size) %>% 
      summarise(num = sum(number)) %>% 
      mutate(size = str_replace_all(size,"size_1","1 person"),
             size = str_replace_all(size,"size_2","2 persons"),
             size = str_replace_all(size,"size_3","3 persons"),
             size = str_replace_all(size,"size_4","4 persons"),
             size = str_replace_all(size,"size_5","5 persons"),
             size = str_replace_all(size,"size_6","6 persons"),
             size = str_replace_all(size,"size_7_or_more","7 persons or more")) %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
    a = weeklyage %>% 
      filter(group != "Boroughwide" & outcome == "Total Cases") %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
    a = weeklysex %>% 
      filter(group != "Boroughwide" & outcome == "Total Cases") %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
    a = weeklyrace %>% 
      filter(group != "Boroughwide" & outcome == "Total Cases") %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
    a = weeklyage %>% 
      filter(group != "Boroughwide" & outcome == "Total Deaths") %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
    a = weeklysex %>% 
      filter(group != "Boroughwide" & outcome == "Total Deaths") %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
    a = weeklyrace %>% 
      filter(group != "Boroughwide" & outcome == "Total Deaths") %>% 
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
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
    
    break.vec <- c(x_min_us, seq(x_min_us, x_max_us, by = "14 days"))
    
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
  
  output$boro_cases1 = renderLeaflet({
    
    plot = switch (input$selection1,
                   cum_case_count = cum_case_count,
                   new_case_count = new_case_count
                   
    )
    
    plot()
  })
  
  output$boro_cases2 = renderLeaflet({
    
    plot = switch (input$selection2,
                   cum_case_rate = cum_case_rate,
                   new_case_rate = new_case_rate
                   
    )
    
    plot()
  })
  
  #######
  
  output$pocase <- renderPlotly({
    mvag_zcta_df = data.mvag.daily %>% 
      filter(zipcode_new %in% input$zip_nbh1,
             type == "Total Cases")
    N = nrow(mvag_zcta_df)
    cases = pull(mvag_zcta_df,num)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = cases[i]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = mvag_zcta_df  %>% 
      mutate(ave_cases = round(ave.cases))
    
    
    
    ggplot(df.ave, aes(x = day, y = num)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + 
      geom_line(aes(x = day, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0("Cumulative Cases by day in ",df.ave$zipcode)) + theme_classic() + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14)
    )
    
    
    #weeklydf %>% 
    #  filter(zipcode_new %in% input$zip_nbh1,
    #         type == "Total Cases") %>%
    #  plot_ly(x = ~day,
    #          y = ~num,
    #          type="scatter",
    #         mode = 'lines+markers',
    #          colors= "Blues") %>% 
    #  layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
    #         xaxis = list(title = "",type = "date"),
    #         yaxis = list(title = ""))
    
    
    
  })
  
  output$death <- renderPlotly({
    mvag_zcta_df = data.mvag.daily %>% 
      filter(zipcode_new %in% input$zip_nbh2,
             type == "Total Deaths")
    N = nrow(mvag_zcta_df )
    cases = pull(mvag_zcta_df,num)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = cases[i]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = mvag_zcta_df  %>% 
      mutate(ave_cases = round(ave.cases))
    
    
    
    ggplot(df.ave, aes(x = day, y = num)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + 
      geom_line(aes(x = day, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0("Cumulative Death Cases by day in ",df.ave$zipcode)) + theme_classic() + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14)
    )
    
    #weeklydf %>% 
    #filter(zipcode_new %in% input$zip_nbh2,
    #       type == "Total Deaths") %>%
    #plot_ly(x = ~day,
    #        y = ~num,
    #        type="scatter",
    #        mode = 'lines+markers',
    #        colors= "Blues") %>% 
    #layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
    #       xaxis = list(title = "",type = "date"),
    #       yaxis = list(title = ""))
    
  }) 
  
  output$porate <- renderPlotly({
    mvag_zcta_df = data.mvag.daily %>% 
      filter(zipcode_new %in% input$zip_nbh3,
             type == "Case Rate (per 100,000 people)")
    N = nrow(mvag_zcta_df )
    cases = pull(mvag_zcta_df,num)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = cases[1]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = mvag_zcta_df  %>% 
      mutate(ave_cases = round(ave.cases))
    
    
    
    ggplot(df.ave, aes(x = day, y = num)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + 
      geom_line(aes(x = day, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0("Case Rate (per 100,000 people) by day in ",df.ave$zipcode)) + theme_classic() + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14)
    )
    #weeklydf %>% 
    #  filter(zipcode_new %in% input$zip_nbh3,
    #         type == "Case Rate (per 100,000 people)") %>%
    #  plot_ly(x = ~day,
    #          y = ~num,
    #          type="scatter",
    #          mode = 'lines+markers',
    #          colors= "Blues") %>% 
    #  layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
    #         xaxis = list(title = "",type = "date"),
    #         yaxis = list(title = ""))
  }) 
  
  output$derate <- renderPlotly({
    mvag_zcta_df = data.mvag.daily %>% 
      filter(zipcode_new %in% input$zip_nbh4,
             type == "Death Rate (per 100,000 people)")
    N = nrow(mvag_zcta_df )
    cases = pull(mvag_zcta_df,num)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = cases[1]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = mvag_zcta_df  %>% 
      mutate(ave_cases = round(ave.cases))
    
    
    
    ggplot(df.ave, aes(x = day, y = num)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + 
      geom_line(aes(x = day, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0("Death Rate (per 100,000 people) by day in ",df.ave$zipcode)) + 
      theme_classic() + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14)
    )
    
    #weeklydf %>% 
    #  filter(zipcode_new %in% input$zip_nbh4,
    #         type == "Death Rate (per 100,000 people)") %>%
    #  plot_ly(x = ~day,
    #          y = ~num,
    #          type="scatter",
    #          mode = 'lines+markers',
    #          colors= "Blues") %>% 
    #  layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
    #         xaxis = list(title = "",type = "date"),
    #         yaxis = list(title = ""))
  })
  
  output$newcases <- renderPlotly({
    mvag_zcta_df = data.mvag.daily %>% 
      filter(zipcode_new %in% input$zip_nbh5,
             type == "New Cases")
    N = nrow(mvag_zcta_df)
    
    cases = pull(mvag_zcta_df,num)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = cases[1]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = mvag_zcta_df  %>% 
      mutate(ave_cases = round(ave.cases))
    
    
    #for (i in 1:N) {
    #  if (df.ave$num[i] < 0) {
    #    df.ave$num[i] = 0
    #  }
    #}
    
    ggplot(df.ave, aes(x = day, y = num)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + 
      geom_line(aes(x = day, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0("New Cases by day in ",df.ave$zipcode)) + theme_classic() + theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14)
    )
    
    #weeklydf %>% 
    #  filter(zipcode_new %in% input$zip_nbh5,
    #         type == "New Cases") %>%
    #  plot_ly(x = ~day,
    #          y = ~num,
    #          type="scatter",
    #          mode = 'lines+markers',
    #          colors= "Blues") %>% 
    #  layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
    #         xaxis = list(title = "",type = "date"),
    #         yaxis = list(title = ""))
  })
  
  output$incdrate <- renderPlotly({
    mvag_zcta_df = data.mvag.daily %>% 
      filter(zipcode_new %in% input$zip_nbh6,
             type == "Incidence Rate (per 100,000 people)")
    N = nrow(mvag_zcta_df)
    #for (i in 1:N) {
    #  if (mvag_zcta_df$num[i] < 0) {
    #    mvag_zcta_df$num[i] = 0
    #  }
    #}
    
    cases = pull(mvag_zcta_df,num)
    #deaths = pull(df_totalcase_bx ,deaths)
    ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    
    for (i in 4:(N-3)) {
      ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    }
    for (i in 1:3) {
      ave.cases[i] = cases[1]
      #ave.deaths[i] = ave.deaths[1]
    }
    for (i in (N-2):N) {
      ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    }
    
    df.ave = mvag_zcta_df  %>% 
      mutate(ave_cases = round(ave.cases))
    
    
    
    ggplot(df.ave, aes(x = day, y = num)) + geom_col(color = "#F6BDBC", fill = "#F6BDBC", alpha = 0.8) + 
      geom_line(aes(x = day, y = ave.cases), color = "red", size = 1) + 
      ggtitle(paste0("Incidence Rate(per 100,000 people) by day in ",df.ave$zipcode)) + 
      theme_classic() + 
      theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(hjust = 0, size = 14)
    )
    
    #weeklydf %>% 
    #  filter(zipcode_new %in% input$zip_nbh6,
    #         type == "Incidence Rate (per 100,000 people)") %>%
    #  plot_ly(x = ~day,
    #          y = ~num,
    #          type="scatter",
    #          mode = 'lines+markers',
    #          colors= "Blues") %>% 
    #  layout(legend=list(title=list(text='<b> Zipcode </b>'), orientation = 'h', xanchor = "center", x = 0.5, y = -0.5),
    #         xaxis = list(title = "",type = "date"),
    #         yaxis = list(title = ""))
  })
  
  ###added by 2020-09-15
  output$trendstitle = renderText({
    return(list(div(img(src = "newlogo3.png", height = "100%",width = "100%"),
                    style="text-align: center;"),
                "COVID-19 Trends")
    )
  })
  
  ###Added by 2020-09-03
  ### Moving average in Trends
  output$mvag_text = renderText({
    return("A look at how COVID-19 new cases, new hospitalizations, and new deaths change over time by NYC borough. 
    Each bar shows the <b> single day new numbers </b> of COVID-19 cases, hospitalizations, and deaths. 
    Solid lines show the 7-day moving average. 
    Incidence rate, new hospitalization rate, and new death rate (per 100,000 people) are calculated using counts of new cases, new hospitalizations, and new deaths divided by borough population size and multiplied by 100,000. 
    Data are updated every Sunday.
    Select available display options to visualize the data.")
  })
  
  output$trendsmoving_avg = renderPlotly({
    
    bar_data = new_trend_data %>% 
      filter(display == input$choices_trend_mvag)
    
    line_data = new_trend_data %>% 
      filter(display == paste0(input$choices_trend_mvag,"_7day_avg")) 
    
    bar_data$avg_7 = line_data$count
    
    
    if (input$choices_trend_mvag == "New Cases") {
      clr = "#F6BDBC"
    } else if (input$choices_trend_mvag == "New Deaths") {
      clr = "#87c7ed"
    } else if (input$choices_trend_mvag == "New Hospitalizations") {
      clr = "#aaed93"
    } 
    
    #else if (input$choices_trend_mvag == "Incidence Rate (per 100,000 people)") {
    #  clr = "#F6BDBC"
    #}else if (input$choices_trend_mvag == "New Death Rate (per 100,000 people)") {
    #  clr = "#87c7ed"
    #} else if (input$choices_trend_mvag == "New Hospitalization Rate (per 100,000 people)") {
    #  clr = "#aaed93"
    #} 
    
    if (input$choices_trend_mvag == "New Cases") {
      cll = "red"
    } else if (input$choices_trend_mvag == "New Deaths") {
      cll = "blue"
    } else if (input$choices_trend_mvag == "New Hospitalizations") {
      cll = "green"
    } 
    
    #else if (input$choices_trend_mvag == "Incidence Rate (per 100,000 people)") {
    #  cll = "red"
    #} else if (input$choices_trend_mvag == "New Death Rate (per 100,000 people)") {
    #  cll = "blue"
    #} else if (input$choices_trend_mvag == "New Hospitalization Rate (per 100,000 people)") {
    #  cll = "green"
    #}
    
    #N = nrow(borocase_new_day_bx_case)
    #cases = pull(borocase_new_day_bx_case ,Count)
    #deaths = pull(df_totalcase_bx ,deaths)
    #ave.cases = rep(0, N-6)
    #ave.deaths = rep(0, N-6)
    
    #for (i in 4:(N-3)) {
    #  ave.cases[i] = mean(cases[(i-3):(i+3)])
      #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
    #}
    #for (i in 1:3) {
    #  ave.cases[i] = ave.cases[1]
      #ave.deaths[i] = ave.deaths[1]
    #}
    #for (i in (N-2):N) {
     # ave.cases[i] = ave.cases[N-3]
      #ave.deaths[i] = ave.deaths[N-3]
    #}
    
   # df.ave = borocase_new_day_bx_case  
   #   mutate(ave_cases = round(ave.cases))
    
    x_min = min(bar_data$date)		
    x_max = max(bar_data$date)		
    if (as.numeric(x_max - x_min) < 15 ) {		
      break.vec <- seq( x_min, x_max, by = "day")		
    } else {		
      if (as.numeric(x_max - x_min) %% 3 == 2) {		
        break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + x_min, x_max, by = "3 days"))		
      } else {		
        break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + 3 + x_min, x_max, by = "3 days"))		
      }		
    }
    
    break.vec = pretty(break.vec,n=9)
    break.vec = c(x_min,break.vec[break.vec>x_min+1 & break.vec<x_max])
    length(break.vec)
    
    a = ggplot(bar_data, aes(x = date, y = count)) + geom_col(color = clr, fill = clr, alpha = 0.5) + 
      geom_line(aes(x = date, y = avg_7), color = cll, size = 1) + 
      ggtitle(paste0(input$choices_trend_mvag, " in different Boroughs")) + theme_classic() + 
      theme_bw() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0, size = 14)) +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 7.5)) +
      theme(legend.title = element_blank()) +
      theme(panel.spacing.y=unit(1, "lines")) + 
      facet_wrap(.~ boro, scales = "free") + 
      scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
      xlab("") + 
      ylab("")
    
    ggplotly(a)
    
    
  })
  
  
 #output$trendsmoving_avg_rate = renderPlotly({
    
 #   borocase_new_day_bx_case = boro_incidence_daily %>% 
 #     filter(newtype == input$choices_trend_mvag_rate ) %>% 
 #     select(-newtype,-Rate)
    
    
  #  if (input$choices_trend_mvag_rate == "Incidence Rate (per 100,000 people)") {
 #     clr = "#F6BDBC"
  #  } else if (input$choices_trend_mvag_rate == "New Death Rate (per 100,000 people)") {
  #    clr = "#87c7ed"
  #  } else if (input$choices_trend_mvag_rate == "New Hospitalization Rate (per 100,000 people)") {
  #    clr = "#aaed93"
  #    
  #  }
    
  #  if (input$choices_trend_mvag_rate == "Incidence Rate (per 100,000 people)") {
  #    cll = "red"
  #  } else if (input$choices_trend_mvag_rate == "New Death Rate (per 100,000 people)") {
  #    cll = "blue"
  #  } else if (input$choices_trend_mvag_rate == "New Hospitalization Rate (per 100,000 people)") {
  #    cll = "green"
  #    
  #  }
    
  #  N = nrow(borocase_new_day_bx_case )
  #  cases = pull(borocase_new_day_bx_case ,Count)
  #  #deaths = pull(df_totalcase_bx ,deaths)
  #  ave.cases = rep(0, N-6)
  #  #ave.deaths = rep(0, N-6)
  #  
  #  for (i in 4:(N-3)) {
  #    ave.cases[i] = mean(cases[(i-3):(i+3)])
  #    #ave.deaths[i] = mean(deaths[(i-3):(i+3)])
  #  }
  #  for (i in 1:3) {
  #    ave.cases[i] = ave.cases[1]
  #    #ave.deaths[i] = ave.deaths[1]
  #  }
  #  for (i in (N-2):N) {
  #    ave.cases[i] = ave.cases[N-3]
  #    #ave.deaths[i] = ave.deaths[N-3]
  #  }
  #  
  #  df.ave = borocase_new_day_bx_case  %>% 
  #    mutate(ave_cases = round(ave.cases))
  #  
  #  x_min = min(df.ave$Date)		
  #  x_max = max(df.ave$Date)		
  #  if (as.numeric(x_max - x_min) < 15 ) {		
  #    break.vec <- seq( x_min, x_max, by = "day")		
  #  } else {		
  #    if (as.numeric(x_max - x_min) %% 3 == 2) {		
  #      break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + x_min, x_max, by = "3 days"))		
  #    } else {		
  #      break.vec <- c(x_min, seq( as.numeric(x_max - x_min) %% 3 + 3 + x_min, x_max, by = "3 days"))		
  #    }		
  #  }
  #  
  #  break.vec = pretty(break.vec,n=9)
  #  break.vec = c(x_min,break.vec[break.vec>x_min+1 & break.vec<x_max],x_max-1)
  #  length(break.vec)
    
    
  # a = ggplot(df.ave, aes(x = Date, y = Count)) + geom_col(color = clr, fill = clr, alpha = 0.5) + 
  #    geom_line(aes(x = Date, y = ave.cases), color = cll, size = 1) + 
  #    ggtitle(paste0(input$choices_trend_mvag_rate, " in different Boroughs")) + theme_classic() + 
  #    theme_bw() +
  #    theme(
  #      axis.title.x = element_blank(),
  #      axis.title.y = element_blank(),
  #      plot.title = element_text(hjust = 0, size = 14)) +
  #    theme(panel.border = element_blank()) +
  #    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
  #    theme(axis.line = element_line(colour = "black")) +
  #    theme(strip.background = element_blank()) + 
  #    theme(axis.text.x = element_text(angle = 65, hjust = 1, size = 7.5)) +
  #    theme(legend.title = element_blank()) +
  #    theme(panel.spacing.y=unit(1, "lines")) + 
  #    scale_x_date(breaks = break.vec, date_labels = "%m-%d") +
  #   facet_wrap(.~Borough, scales = "free") + 
  #    xlab("") + 
  #    ylab("")
  #  
  #  ggplotly(a)
    
  #})
  
  
  output$trends_video_text = renderText({
    return("The map videos below can be used to visualize how the changes of COVID-19 new cases, cumulative cases, and cumulative deaths over time vary by NYC ZCTAs.")
  })
  
  output$mapvideo = renderUI({
    h6(tags$iframe(src = "https://www.youtube.com/embed/FtaFXzIiq8A",frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA, width="560", height="315"))
    
  })
  
  output$mapvideo_newcase = renderUI({
    h6(tags$iframe(src = "https://www.youtube.com/embed/QI-PtumOVcY",frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA, width="560", height="315"))
    
  })
  output$mapvideo_death = renderUI({
    h6(tags$iframe(src = "https://www.youtube.com/embed/16eTyLLjCF0",frameborder="0", allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA, width="560", height="315"))
    
  })
  
  output$Projecttext = renderText({
    return("<b> Projection of COVID-19 new infections, new cases, new hospitalizations, and new deaths in the next 8 weeks </b> by NYC United Hospital Fund (UHF) neighborhood. 
    The projection data are based on the COVID-19 Epidemic Outcomes and Healthcare Demands for NYC model developed by Professor Wan Yang at Columbia University <sup> 1,2,3 </sup>.
    The red curves represent median estimates and the gray bands represent the first and third quarters of the estimates. 
    Choose different modeling assumptions to visualize the projection data.
    ")
  })
  
  output$Projecttext2 = renderText({
    return("<b> Seasonality Assumption </b> contains 2 options: <br>
    <p style='margin-left:5%; '> 1. <b> Seasonality Assumed </b>: with seasonal changes to virus transmissibility. <br> 
    2. <b> No Seasonality </b>: without seasonality changes to virus transmissibility.</p>
    
    <b> Intervention </b> contains 6 scenarios. <br>
    <p style='margin-left:5%; '> 1. <b> As Is </b>: Status quo. Current level of control.
    <br>
    2. <b> Rebound 1 </b>: 10% increase in the transmission rate compared to the <b> As Is </b> scenario.
    <br>
    3. <b> Rebound 2 </b>: 25% increase in the transmission rate compared to the <b> As Is </b> scenario.
    <br>
    4. <b> Ctrl 1 </b>: 10% reduction in the transmission rate compared to the <b> As Is </b> scenario.
    <br>
    5. <b> Ctrl 2 </b>: 25% reduction in the transmission rate compared to the <b> As Is </b> scenario.
    <br>
    6.<b> Worst Case </b>: No Control. Worst Case. </p>
   
    <b> Location </b> contains 42 United Hospital Fund (UHF) neighborhoods in NYC. 
    Zip Codes that the selected UHF contains are displayed below the location selection box. 
    ")
  })
  
  output$ProRefertext = renderText({
    return("1.	https://github.com/wan-yang/COLUMBIA-COVID19-PROJECTIONS-FOR-NYC.<br>
           2.	Yang W, Kandula S, Huynh M, et al. Estimating the infection fatality risk of COVID-19 in New York City, March 1-May 16, 2020. medRxiv 2020: 2020.06.27.20141689. 
           https://www.medrxiv.org/content/10.1101/2020.06.27.20141689v1 <br>
           3.	Yang W, Shaff J, Shaman J. COVID-19 transmission dynamics and effectiveness of public health interventions in New York City during the 2020 spring pandemic wave. medRxiv. 2020:2020.09.08.20190710. 
           https://www.medrxiv.org/content/10.1101/2020.09.08.20190710v1 <br>
")
  })
  
  output$proj_line_case = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    
    x_min_us = min(train_data_sel$date)
    x_max_us = max(season_city_newcases$date)
    break.train = c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    
    b = ggplot() + 
      #geom_line(data = train_data_sel ,aes(x = date, y = new_cases_value,group = 1)) +
     # geom_point(data = train_data_sel , aes(x = date, y = new_cases_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_cases_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_cases_lower, 
                                                   ymax=new_cases_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      scale_x_date(breaks = break.train, date_labels = "%m-%d") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      xlab("") + 
      ylab("") #+ 
    #ggtitle(paste0("New Total Hospitalizations Under ", input$input$interve," Control Scenarios"))
    
    ggplotly(b)
  })
  
  output$proj_line_deat = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    
    x_min_us = min(train_data_sel$date)
    x_max_us = max(season_city_newcases$date)
    break.train = c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    b = ggplot() + 
     # geom_line(data = train_data_sel ,aes(x = date, y = new_deaths_value,group = 1)) +
     # geom_point(data = train_data_sel , aes(x = date, y = new_deaths_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_deaths_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_deaths_lower, 
                                                   ymax=new_deaths_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      scale_x_date(breaks = break.train, date_labels = "%m-%d") + 
      xlab("") + 
      ylab("") #+ 
    #ggtitle(paste0("New Deaths Under", input$input$interve,"Control Scenarios"))
    
    ggplotly(b)
  })
  
  output$proj_line_hosp = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    
    x_min_us = min(train_data_sel$date)
    x_max_us = max(season_city_newcases$date)
    break.train = c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    
    b = ggplot() + 
      #geom_line(data = train_data_sel ,aes(x = date, y = new_hosp_value,group = 1)) +
     # geom_point(data = train_data_sel , aes(x = date, y = new_hosp_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_hosp_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_hosp_lower, 
                                                   ymax=new_hosp_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      scale_x_date(breaks = break.train, date_labels = "%m-%d") + 
      xlab("") + 
      ylab("")  
    #ggtitle(paste("New Total Hospitalizations Under", input$input$interve,"Control Scenarios"))
    
    ggplotly(b)
  })
  
  output$proj_line_infec = renderPlotly({
    train_data_sel = train_data %>% 
      filter(seasonality == input$season  & location == input$loc_proj) 
    season_city_newcases = project_data %>% 
      filter(seasonality == input$season  & intervention ==input$interve & location == input$loc_proj)
    
    x_min_us = min(train_data_sel$date)
    x_max_us = max(season_city_newcases$date)
    break.train = c(x_min_us, seq(x_min_us, x_max_us, by = "7 days"))
    
    
    b = ggplot() + 
      #geom_line(data = train_data_sel ,aes(x = date, y = new_hosp_value,group = 1)) +
      # geom_point(data = train_data_sel , aes(x = date, y = new_hosp_value)) +
      theme_minimal() +
      #theme(legend.position = "none")+
      #theme(panel.spacing.y=unit(2, "lines")) + 
      #ggplot(season_city_newcases) + 
      geom_path(data = season_city_newcases,aes(y=new_infections_value, x=date, colour = "Projected Median Number",group = 1))+
      geom_ribbon(data = season_city_newcases, aes(ymin=new_infections_lower, 
                                                   ymax=new_infections_upper, 
                                                   x=date, fill = "IQR"), alpha = 0.2,group = 1) + 
      scale_colour_manual("",values="red") + 
      scale_fill_manual("",values="grey12") + 
      theme_bw() +
      theme(panel.border = element_blank()) +
      theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank()) +
      theme(axis.line = element_line(colour = "black")) +
      theme(strip.background = element_blank()) + 
      theme(axis.text.x = element_text(angle = 65, hjust = 1)) + 
      theme(panel.spacing.y=unit(1, "lines")) + 
      theme(legend.position = "none")+
      scale_x_date(breaks = break.train, date_labels = "%m-%d") + 
      xlab("") + 
      ylab("")  
    #ggtitle(paste("New Total Hospitalizations Under", input$input$interve,"Control Scenarios"))
    
    ggplotly(b)
  })
  
  output$zipuhf = renderText({
    return("The selected location contains Zip Codes: ")
  })
  output$uhftext = renderText({
    
    lo = projt_data %>% filter(location == input$loc_proj)
    
    return(lo$zipcode[1])
  })
  
}

shinyApp(ui, server)




