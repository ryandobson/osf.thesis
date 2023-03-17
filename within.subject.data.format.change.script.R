
#EDITNG MY DATA FRAME SO MY WITHIN SUBJECTS VARIABLE IS IN LONG FORMAT 

#Doing this so I can create graphs/charts where I split my data up by the within
#subject condition 

library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")

source("factor.script.R")
str(data)


#Transforming the data frame so that all of my emotions are stacked on top of 
#each other! 

#Taking each emotion separately and pivoting them to long format 
#also grabbing the other variables I need for analysis here so I have them later 
jealous <- data %>% pivot_longer(c(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous), 
                      names_to = "within.subject",
                      values_to = "jealous") %>% 
  select(jealous,
         within.subject,
         gender,
         single.or.dating,
         osf.closer.ssf,
         relationship.status.participant, 
          )  
              
angry <- data %>% pivot_longer(c(comp.ssf.angry.upset, 
                                 comp.osf.angry.upset, 
                                 comp.rom.angry.upset), 
                                       names_to = "within.subject",
                                       values_to = "angry") %>% 
  select(angry
  )  

happy <- data %>% pivot_longer(c(ssf.happy, osf.happy, rom.happy), 
                                 names_to = "within.subject",
                                 values_to = "happy") %>% 
  select(happy
  )  


sad <- data %>% pivot_longer(c(ssf.sad, osf.sad, rom.sad), 
                               names_to = "within.subject",
                               values_to = "sad") %>% 
  select(sad
  )  

within.data <- bind_cols(jealous, angry, happy, sad)


#Getting some descriptives to check and ensure that things combined properly 
#within.data %>% group_by(gender, within.subject) %>% 
 # summarize(sad.mean = mean(sad, na.rm = TRUE),
  #          happy.mean = mean(happy, na.rm = TRUE))

#str(within.data)


#Changing the within.subject column to numbers so I can change it into a factor
#later 
within.data <- within.data %>% 
  mutate(within.subject = replace(within.subject, within.subject == "comp.ssf.jealous", 1),
within.subject = replace(within.subject, within.subject == "comp.osf.jealous", 2),
within.subject = replace(within.subject, within.subject == "comp.rom.jealous", 3))
#`1 = SSF 
# 2 = OSF
# 3 = Rom `


#Reordering variables so the data set looks more clean 
long.data <- within.data %>% select(within.subject, gender, single.or.dating,
                                    relationship.status.participant, 
                                    osf.closer.ssf,
                                    everything())

#Removing unecessary objects to clean up the environment 
rm(jealous, happy, sad, angry, within.data, data)


#Saving my new data set 

long.data %>%  write_csv("data/long.analyze.osf.data.csv")




