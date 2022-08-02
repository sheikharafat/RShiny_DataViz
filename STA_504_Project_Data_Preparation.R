
# --------------------------------------------------------------------------------#
#                                    Group Project                                #
#                                                                                 #
# Title: Transitions of Covid-19 Cases and Deaths of the United States 
#        over Time and States                                                     #
#                                                                                 #
# Student Name:Sheikh Yasir Arafat                            #                   #
#                                                                                 #
# STA 504: Advanced Data Visualization                                            #
#---------------------------------------------------------------------------------#


#-----------------------------------Set Working Directory-------------------------#
setwd("E:/Miami/Spring_22/STA 504/Group_Project")
#---------------------------------------------------------------------------------#
#-----------------------------Load the required Library---------------------------#
library(tidyverse, warn.conflicts = FALSE)
library(lubridate)
library(maps)
library(readxl)
library(ggthemes)
library(mapproj)
library(stringr)
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#----------------------------------Read the data sets-----------------------------#
data_case<-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")

data_dead<-read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv")
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
######### -------------------- Data Manipulation------------------------- #########
####                      Data Preparation for Bar Graph                  #########
#-----------------------Manipulating the total Cases------------------------------#
Data_Case_date<- data_case %>% 
  pivot_longer(cols = 12:839,
               names_to = "Date_u_case",values_to = "Case_u") %>%
  mutate(State=tolower(Province_State)) %>%
  group_by(State) %>% 
  mutate(Total_case=Case_u-lag(Case_u, default = 0)) %>%
  mutate(Total_case=replace(Total_case, which(Total_case < 0), 0)) %>%
  mutate(Date_u11=mdy(Date_u_case)) %>%
  mutate(Date_u_year=year(Date_u11)) %>% 
  mutate(Date_u_month=month(Date_u11,label=TRUE)) %>% 
  select(State,Date_u_year,Date_u_month,Total_case) %>%
  group_by(State,Date_u_year,Date_u_month) %>%
  summarise(Final_Case=sum(Total_case),.groups = "keep")
  
#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#-----------------------Manipulating the total Deaths-----------------------------#
Data_Dead_date<- data_dead %>% 
  pivot_longer(cols = 13:840,
               names_to = "Date_u_dead",values_to = "Dead_u") %>%
  mutate(State=tolower(Province_State)) %>% 
  group_by(State) %>% 
  mutate(Total_dead=Dead_u-lag(Dead_u, default = 0)) %>%
  mutate(Total_dead=replace(Total_dead, which(Total_dead < 0), 0)) %>%
  mutate(Date_u22=mdy(Date_u_dead)) %>%
  mutate(Date_u_year=year(Date_u22)) %>% 
  mutate(Date_u_month=month(Date_u22,label=TRUE)) %>% 
  select(State,Date_u_year,Date_u_month,Total_dead) %>%
  group_by(State,Date_u_year,Date_u_month) %>% 
  summarise(Final_Death=sum(Total_dead),.groups = "keep")


#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#-----------------------Combining the two data sets-------------------------------#
Data_both<-left_join(Data_Case_date,Data_Dead_date,by=c("State"="State",
                                                        "Date_u_year"="Date_u_year",
                                                        "Date_u_month"="Date_u_month")) %>% 
  rename(Year=Date_u_year,Month=Date_u_month)

#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
#---------------Transform the cumulative sums to individual obs-------------------#

Data_both_new<- Data_both %>% 
  group_by(State) %>% 
  mutate(Final_Case=Total_case-lag(Total_case, default = 0)) %>%
  mutate(Final_Death=Total_dead-lag(Total_dead, default = 0)) %>% 
  mutate(Final_Case=replace(Final_Case, which(Final_Case < 0), 0)) %>%
  mutate(Final_Death=replace(Final_Death, which(Final_Death < 0), 0)) %>% 
  select(State,Year,Month,Final_Case,Final_Death)
#---------------------------------------------------------------------------------#
#------------------------Save the created data set--------------------------------#
write.csv(Data_both,'Data_bar_final.csv', row.names = FALSE)

#---------------------------------------------------------------------------------#
#---------------------------------------------------------------------------------#
######### -------------------- Data Manipulation------------------------- #########
#                             Data preparation for map                            #
#---------------------------------------------------------------------------------#
#------------------------Read the states maps data--------------------------------#
state_outlines <- map_data("state") 
#---------------Combine the map data with covid cases and deaths------------------#
Data_date_map<-left_join(state_outlines,Data_both,
                         by=c("region"="State")) %>% 
  rename(State=region) 
#--------------------------Remove the unnecessary Variables-----------------------#
Data_final<-Data_date_map[,-c(4,6)]
#-----------------------------Save the final data set-----------------------------#
write.csv(Data_final,'Data_map_final.csv', row.names = FALSE)
######### --------------------------------------------------------------- #########
######### --------------------------------------------------------------- #########




#==================================================================================#
#==================================================================================#
#                                       END                                        #
#==================================================================================#
#==================================================================================#