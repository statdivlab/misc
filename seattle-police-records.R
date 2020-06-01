# Amy Willis, Ph.D. (Statistics)
# June 1 2020
# An analysis of use of force incidents by Seattle Police through racial equity lens

# Read in latest data
library(tidyverse)
library(lubridate)
uof <- read_csv("https://data.seattle.gov/api/views/ppi5-g2bj/rows.csv?accessType=DOWNLOAD")%>%
  separate(Occured_date_time, into = c("Day", "Time", "AM-PM"), sep=" ") %>%
  mutate(Date = mdy(Day, quiet = TRUE)) %>%
  mutate(Year = year(Date)) 

## since there are issues with double-recording, 
## take only one UOF incident per Officer_ID-Subject_ID combo
uof %>%
  filter(Year == 2019) %>%
  group_by(Officer_ID, Subject_ID, Subject_Race) %>%
  summarise(n = n()) %>%
  ungroup %>%
  select(-n) %>%
  group_by(Subject_Race) %>%
  summarise(n = n()) %>%
  mutate(percentage = 100*n/sum(n))

# In 2019, 28% of UOF incidents against Blacks or African Americans

# 2014-2018 American Community Survey (ACS) 5-Year Estimates (U.S. Census Bureau)
# https://www.seattle.gov/opcd/population-and-demographics/about-seattle#raceethnicity
# 6.8% Black or African American

uof %>%
  filter(Year == 2020) %>%
  filter(Date >= "2020-01-01") %>%
  filter(Date < "2020-04-01") %>%
  group_by(Officer_ID, Subject_ID, Subject_Race) %>%
  summarise(n = n()) %>%
  ungroup %>%
  select(-n) %>%
  group_by(Subject_Race) %>%
  summarise(n = n()) %>%
  mutate(percentage = 100*n/sum(n))
# In Q1 2020, 29% of UOF incidents against Blacks or African Americans

