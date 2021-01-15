#_______________________________________________________________________________
#_______________________________________________________________________________
# Environmental Sciences
# Data Collection, Storage & Management
#_______________________________________________________________________________
#_______________________________________________________________________________

sessionInfo()

# Exercise 1 ----

# preparing raw data upload

rm(list=ls(all=TRUE))

#install.packages("rmarkdown")

library(readr)
#library(dplyr)
library(tidyverse)
library(rio)
library(dygraphs)
library(rmarkdown)

hoboraw <- read_csv("C:/Users/neiss/Desktop/2020FreiburgEnvironmentalSciences/Semester/Semester1/Data- Storage Collection & Managment/data/raw-data/10347371_2020.12.21_15.26.csv")

# 
hoboraw$`Date Time, GMT+01:00` <- as.POSIXct(hoboraw$`Date Time, GMT+01:00`, format="%d/%m/%Y %H:%M:%OS")
#view(hoboraw)

# period subset 
hobop<- hoboraw[hoboraw$`Date Time, GMT+01:00` >= '2020/11/30 00:00:00' & 
                  hoboraw$`Date Time, GMT+01:00` <= '2020/12/20 23:50:00',]


# remove unwanted columns

str(hobop)

hobop <- hobop %>% select(-'Bad Battery (LGR S/N: 10347371)')
hobop <- hobop %>% select(-('Coupler Attached (LGR S/N: 10347371)':'End Of File (LGR S/N: 10347371)'))


# rename columns

names(hobop) <- c('id','date_time','temp','lux')


# df_complete %>% 
#  rename(datetime = dttm) %>% 
#  mutate(hour = hour(datetime), .before = "temp") %>% 
#-  rename(hr = hour)



# assign new id's 

#hobop$id <- c(1:3024)
hobop$id <- c(1:nrow(hobop))

# exporting dataframe

write.csv(hobop,'C:/Users/neiss/Desktop/2020FreiburgEnvironmentalSciences/Semester/Semester1/Data- Storage Collection & Managment/data/raw-data/10347371.csv', row.names = FALSE , quote=F)

#_______________________________________________________________________________

# verifying data file

dat <- read.csv('C:/Users/neiss/Desktop/2020FreiburgEnvironmentalSciences/Semester/Semester1/Data- Storage Collection & Managment/data/raw-data/10347371.csv')


#_______________________________________________________________________________

# data exploration 

dat$`date_time` <- as.POSIXct(dat$`date_time`, format="%Y-%m-%d %H:%M:%OS")

plot(dat$date_time,dat$lux)
