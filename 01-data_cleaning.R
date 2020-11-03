#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [https://usa.ipums.org/usa-action/data_requests/download,
# and https://www.voterstudygroup.org/publication/nationscape-data-set]
# Author: Ziqing Lyu, Xue Shan, Yaozhong Zhang, 
# Data: 30 October 2020
# Contact: zi.lyu@mail.utoronto.ca 
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!

#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/rachellyu/Desktop/STA304PS3_new")
# load survey data 
raw_Surveydata <- read_dta("ns20200625/ns20200625.dta")
# label in row data 
raw_Surveydata <- labelled::to_factor(raw_Surveydata)
# keep  some variables that we want 
select_Surveydata <- 
  raw_Surveydata %>% 
  select(registration,
         vote_intention,
         vote_2020,
         race_ethnicity,
         state,
         age)
#change age type
select_Surveydata$age<-as.numeric(select_Surveydata$age)

# select observations which registered and also vote trump or biden
Surveydata<- select_Surveydata %>% 
  filter(registration=="Registered"&
           vote_intention!="No, I will not vote but I am eligible"&
           vote_intention!="No, I am not eligible to vote"&
           (vote_2020=="Donald Trump"|vote_2020=="Joe Biden"))
# drop NA 
Surveydata <-na.omit(Surveydata)

#load census data 
raw_Censusdata <- read_dta("usa_00003.dta")
# label census data 
raw_Censusdata <- labelled::to_factor(raw_Censusdata)
# keep some variables 
select_Censusdata <- 
  raw_Censusdata %>% 
  select(perwt,
         age,
         sex, 
         stateicp,
         race)
# change age tpye
select_Censusdata$age<-as.numeric(select_Censusdata$age)
# select observation which at least 18 years old 
Censusdata<- select_Censusdata %>% filter(age>=18)
# drop NAs 
Censusdata <-na.omit(Censusdata)


#change the variables to the same style:

#Create Age group in both datasets
Surveydata<- Surveydata %>% 
  mutate(Age_group = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 40 ~ '21 to 40',
                              age >40  & age <= 60 ~ '41 to 60',
                              age >60  & age <= 80 ~ '61 to 80',
                              age >80 ~ 'above 80')) 
Censusdata<- Censusdata %>% 
  mutate(Age_group = case_when(age <=20 ~ '20 or less',
                              age >20  & age <= 40 ~ '21 to 40',
                              age >40  & age <= 60 ~ '41 to 60',
                              age >60  & age <= 80 ~ '61 to 80',
                              age >80 ~ 'above 80')) 
#check if they are the same length and name 
unique(Censusdata $Age_group)
unique(Surveydata $Age_group)

#change state to same style
Censusdata <- Censusdata %>% 
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arkansas"~"AR",
                           stateicp=="arizona"~"AZ",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="district of columbia"~"DC",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="iowa"~"IA",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="maryland"~"MD",
                           stateicp=="maine"~"ME",
                           stateicp=="minnesota"~"MN",
                           stateicp=="michigan"~"MI",
                           stateicp=="missouri"~"MO",
                           stateicp=="mississippi"~"MS",
                           stateicp=="montana"~"MT",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="nebraska"~"NE",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="nevada"~"NV",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new york"~"NY",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="ohio"~"OH",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="virginia"~"VA",
                           stateicp=="vermont"~"VT",
                           stateicp=="washington"~"WA",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wyoming"~"WY")) 
Censusdata$stateicp<-NULL
# check if they are the same length and name 
unique(Censusdata$state)
unique(Surveydata$state)

#change race to same style
asian_other<-c("Asian (Asian Indian)","Asian (Korean)","Asian (Vietnamese)","Asian (Filipino)","Asian (Other)",
               "Pacific Islander (Samoan)","Pacific Islander (Native Hawaiian)",
               "Pacific Islander (Guamanian)","Pacific Islander (Other)")
# in survey data 
Surveydata <- Surveydata %>% 
  mutate(race = case_when(race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
                          race_ethnicity =="Asian (Japanese)" ~ 'Japanese',
                          race_ethnicity =="White" ~ 'White',
                          race_ethnicity =="Black, or African American" ~ 'Black, or African American',
                          race_ethnicity =="American Indian or Alaska Native"~"American Indian or Alaska Native",
                          race_ethnicity =="Some other race" ~ 'Other race',
                          race_ethnicity =="Other race "~"Other race",
                          race_ethnicity %in% asian_other ~"Other asian or pacific islander")) 
Surveydata$race_ethnicity<-NULL

#in census data 
Censusdata<- Censusdata %>% 
  mutate(race2 = case_when(race=="chinese"~"Chinese",
                           race=="white"~"White",
                           race=="japanese"~"Japanese",
                           race=="black/african american/negro"~"Black, or African American",
                           race=="american indian or alaska native"~"American Indian or Alaska Native",
                           race=="other asian or pacific islander"~"Other asian or pacific islander",
                           race=="two major races"~"Other race",
                           race=="other race, nec"~"Other race",
                           race=="three or more major races"~"Other race")) 
Censusdata$race<-Censusdata$race2
Censusdata$race2<-NULL

# check if they are the same style 
unique(Censusdata$race)
unique(Surveydata$race)


# add columns that vote Trump and Biden on survey data 
Surveydata<-
  Surveydata %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))
Surveydata<-
  Surveydata %>%
  mutate(vote_Biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

# select variables to create two small datasets 
Surveydata<- Surveydata%>% 
  select(vote_2020,age,Age_group,state,race,vote_trump,vote_Biden)
Censusdata<- Censusdata%>% 
  select(perwt,age,Age_group,state,race,sex)


write_csv(Surveydata, "Survey_data2.csv")
write_csv(Censusdata, "census_data2.csv")



