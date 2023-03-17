
#Recreating my main descriptive statistics table! 

library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")

#?source

#Sourcing in my factors for data analysis 
source("factor.script.R")
str(data)
### Checking out the structure of the data frame! 
#Did this to confirm that my columns are numeric and my levels are factors 


### STARTING THE TABLE WITH JEALOUS

jealous <- data %>% gather(key = emotion.gathered, 
                    value = score,
                    c(ssf.jealous, osf.jealous, rom.jealous),
                    factor_key = TRUE) %>% 
          select(gender, emotion.gathered, score) %>% 
          group_by(gender, emotion.gathered) %>% 
          summarize(mean = mean(score, na.rm = TRUE))%>% 
          mutate_at(vars("mean"), round, 2) %>% 
          pivot_wider(names_from = emotion.gathered,
                      values_from = mean) %>% 
          pivot_wider(names_from = gender, 
                      values_from = c(ssf.jealous, osf.jealous, rom.jealous)) %>% 
          mutate(Emotion = "Jealous") %>% 
          select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
jealous <- jealous %>% rename(ssf.men = ssf.jealous_Men,
                              ssf.women = ssf.jealous_Women,
                              osf.men = osf.jealous_Men,
                              osf.women = osf.jealous_Women,
                              rom.men = rom.jealous_Men,
                              rom.women = rom.jealous_Women)


### ADDING SAD 

sad <- data %>% gather(key = emotion.gathered, 
                           value = score,
                           c(ssf.sad, osf.sad, rom.sad),
                           factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.sad, osf.sad, rom.sad)) %>% 
  mutate(Emotion = "Sad") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sad <- sad %>% rename(ssf.men = ssf.sad_Men,
                          ssf.women = ssf.sad_Women,
                          osf.men = osf.sad_Men,
                          osf.women = osf.sad_Women,
                          rom.men = rom.sad_Men,
                          rom.women = rom.sad_Women)


### ADDING HAPPY 

happy <- data %>% gather(key = emotion.gathered, 
                       value = score,
                       c(ssf.happy, osf.happy, rom.happy),
                       factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.happy, osf.happy, rom.happy)) %>% 
  mutate(Emotion = "Happy") %>% 
  select(Emotion, everything())
  
#Renaming the table headers so that they do not specify an emotion and are general 
  happy <- happy %>% rename(ssf.men = ssf.happy_Men,
                      ssf.women = ssf.happy_Women,
                      osf.men = osf.happy_Men,
                      osf.women = osf.happy_Women,
                      rom.men = rom.happy_Men,
                      rom.women = rom.happy_Women)
        
  ### ADDING Surprised  
  
  surprised <- data %>% gather(key = emotion.gathered, 
                           value = score,
                           c(ssf.surprised, osf.surprised, rom.surprised),
                           factor_key = TRUE) %>% 
    select(gender, emotion.gathered, score) %>% 
    group_by(gender, emotion.gathered) %>% 
    summarize(mean = mean(score, na.rm = TRUE))%>% 
    mutate_at(vars("mean"), round, 2) %>% 
    pivot_wider(names_from = emotion.gathered,
                values_from = mean) %>% 
    pivot_wider(names_from = gender, 
                values_from = c(ssf.surprised, osf.surprised, rom.surprised)) %>% 
    mutate(Emotion = "Surprised") %>% 
    select(Emotion, everything())
  
  #Renaming the table headers so that they do not specify an emotion and are general 
  surprised <- surprised %>% rename(ssf.men = ssf.surprised_Men,
                            ssf.women = ssf.surprised_Women,
                            osf.men = osf.surprised_Men,
                            osf.women = osf.surprised_Women,
                            rom.men = rom.surprised_Men,
                            rom.women = rom.surprised_Women)
  
  
  ### ADDING Upset  
  
  upset <- data %>% gather(key = emotion.gathered, 
                               value = score,
                               c(ssf.upset, osf.upset, rom.upset),
                               factor_key = TRUE) %>% 
    select(gender, emotion.gathered, score) %>% 
    group_by(gender, emotion.gathered) %>% 
    summarize(mean = mean(score, na.rm = TRUE))%>% 
    mutate_at(vars("mean"), round, 2) %>% 
    pivot_wider(names_from = emotion.gathered,
                values_from = mean) %>% 
    pivot_wider(names_from = gender, 
                values_from = c(ssf.upset, osf.upset, rom.upset)) %>% 
    mutate(Emotion = "Upset") %>% 
    select(Emotion, everything())
  
  #Renaming the table headers so that they do not specify an emotion and are general 
  upset <- upset %>% rename(ssf.men = ssf.upset_Men,
                                    ssf.women = ssf.upset_Women,
                                    osf.men = osf.upset_Men,
                                    osf.women = osf.upset_Women,
                                    rom.men = rom.upset_Men,
                                    rom.women = rom.upset_Women)
  
  
  
  
  ### ADDING Angry  
  
  angry <- data %>% gather(key = emotion.gathered, 
                           value = score,
                           c(ssf.angry, osf.angry, rom.angry),
                           factor_key = TRUE) %>% 
    select(gender, emotion.gathered, score) %>% 
    group_by(gender, emotion.gathered) %>% 
    summarize(mean = mean(score, na.rm = TRUE))%>% 
    mutate_at(vars("mean"), round, 2) %>% 
    pivot_wider(names_from = emotion.gathered,
                values_from = mean) %>% 
    pivot_wider(names_from = gender, 
                values_from = c(ssf.angry, osf.angry, rom.angry)) %>% 
    mutate(Emotion = "Angry") %>% 
    select(Emotion, everything())
  
  #Renaming the table headers so that they do not specify an emotion and are general 
  angry <- angry %>% rename(ssf.men = ssf.angry_Men,
                            ssf.women = ssf.angry_Women,
                            osf.men = osf.angry_Men,
                            osf.women = osf.angry_Women,
                            rom.men = rom.angry_Men,
                            rom.women = rom.angry_Women)
  
  ### ADDING threatened  
  
  threatened <- data %>% gather(key = emotion.gathered, 
                           value = score,
                           c(ssf.threatened, osf.threatened, rom.threatened),
                           factor_key = TRUE) %>% 
    select(gender, emotion.gathered, score) %>% 
    group_by(gender, emotion.gathered) %>% 
    summarize(mean = mean(score, na.rm = TRUE))%>% 
    mutate_at(vars("mean"), round, 2) %>% 
    pivot_wider(names_from = emotion.gathered,
                values_from = mean) %>% 
    pivot_wider(names_from = gender, 
                values_from = c(ssf.threatened, osf.threatened, rom.threatened)) %>% 
    mutate(Emotion = "Threatened") %>% 
    select(Emotion, everything())
  
  #Renaming the table headers so that they do not specify an emotion and are general 
threatened <- threatened %>% rename(ssf.men = ssf.threatened_Men,
                            ssf.women = ssf.threatened_Women,
                            osf.men = osf.threatened_Men,
                            osf.women = osf.threatened_Women,
                            rom.men = rom.threatened_Men,
                            rom.women = rom.threatened_Women)
  
### ADDING relieved  

relieved <- data %>% gather(key = emotion.gathered, 
                              value = score,
                              c(ssf.relieved, osf.relieved, rom.relieved),
                              factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.relieved, osf.relieved, rom.relieved)) %>% 
  mutate(Emotion = "Relieved") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
relieved <- relieved %>% rename(ssf.men = ssf.relieved_Men,
                                    ssf.women = ssf.relieved_Women,
                                    osf.men = osf.relieved_Men,
                                    osf.women = osf.relieved_Women,
                                    rom.men = rom.relieved_Men,
                                    rom.women = rom.relieved_Women)



### ADDING jealous composite  

comp.jealous <- data %>% gather(key = emotion.gathered, 
                            value = score,
                            c(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous),
                            factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous)) %>% 
  mutate(Emotion = "Comp Jealous") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
comp.jealous <- comp.jealous %>% rename(ssf.men = comp.ssf.jealous_Men,
                                ssf.women = comp.ssf.jealous_Women,
                                osf.men = comp.osf.jealous_Men,
                                osf.women = comp.osf.jealous_Women,
                                rom.men = comp.rom.jealous_Men,
                                rom.women = comp.rom.jealous_Women)
  
### ADDING anger composite  

comp.anger <- data %>% gather(key = emotion.gathered, 
                                value = score,
                                c(comp.ssf.angry.upset, 
                                  comp.osf.angry.upset, 
                                  comp.rom.angry.upset),
                                factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(comp.ssf.angry.upset, 
                              comp.osf.angry.upset, 
                              comp.rom.angry.upset)) %>% 
  mutate(Emotion = "Comp Anger") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
comp.anger <- comp.anger %>% rename(ssf.men = comp.ssf.angry.upset_Men,
                                        ssf.women = comp.ssf.angry.upset_Women,
                                        osf.men = comp.osf.angry.upset_Men,
                                        osf.women = comp.osf.angry.upset_Women,
                                        rom.men = comp.rom.angry.upset_Men,
                                        rom.women = comp.rom.angry.upset_Women)



### Bind all of the rows last 
#It gets funky if you try to add one emotion at a time. 
  #Remember, you need the same exact row names for the rows to bind properly 
mean.table <- bind_rows(surprised, 
                   upset, 
                   angry, 
                   jealous, 
                   happy,
                   threatened,
                   relieved,
                   sad,
                   comp.jealous,
                   comp.anger)

rm(surprised, 
   upset, 
   angry, 
   jealous, 
   happy,
   threatened,
   relieved,
   sad,
   comp.jealous,
   comp.anger)



### ATTEMPTING TO ADD IN THE SD's 
#This was an interesting attempt at trying to add the SD's in. 
#I found out that it is impossible to format. Oh well. 
#Save the formatting for later. 

#jealous.1 <- data %>% gather(key = emotion.gathered, 
 #                          value = score,
  #                         c(ssf.jealous, osf.jealous, rom.jealous),
   #                        factor_key = TRUE) %>% 
  #select(gender, emotion.gathered, score) %>% 
  #group_by(gender, emotion.gathered) %>% 
  #summarize(mean = mean(score, na.rm = TRUE),
   #         sd = sd(score, na.rm= TRUE)) %>% 
  #mutate_at(vars("mean","sd"), round, 2) %>% 
  #unite("mean(sd)", mean:sd, sep = " \t\r\n") %>% 
  #pivot_wider(names_from = emotion.gathered,
   #           values_from = "mean(sd)") %>% 
  #pivot_wider(names_from = gender, 
   #           values_from = c(ssf.jealous, osf.jealous, rom.jealous)) %>% 
  #mutate(Emotion = "Jealous") %>% 
  #select(Emotion, everything()) 
#rm(jealous.1)



### CREATING A SD TABLE 


sd.jealous <- data %>% gather(key = emotion.gathered, 
                           value = score,
                           c(ssf.jealous, osf.jealous, rom.jealous),
                           factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(sd = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("sd"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = sd) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.jealous, osf.jealous, rom.jealous)) %>% 
  mutate(Emotion = "Jealous") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.jealous <- sd.jealous %>% rename(ssf.men = ssf.jealous_Men,
                              ssf.women = ssf.jealous_Women,
                              osf.men = osf.jealous_Men,
                              osf.women = osf.jealous_Women,
                              rom.men = rom.jealous_Men,
                              rom.women = rom.jealous_Women)


### ADDING SAD 

sd.sad <- data %>% gather(key = emotion.gathered, 
                       value = score,
                       c(ssf.sad, osf.sad, rom.sad),
                       factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(sd = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("sd"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = sd) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.sad, osf.sad, rom.sad)) %>% 
  mutate(Emotion = "Sad") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.sad <- sd.sad %>% rename(ssf.men = ssf.sad_Men,
                      ssf.women = ssf.sad_Women,
                      osf.men = osf.sad_Men,
                      osf.women = osf.sad_Women,
                      rom.men = rom.sad_Men,
                      rom.women = rom.sad_Women)


### ADDING HAPPY 

sd.happy <- data %>% gather(key = emotion.gathered, 
                         value = score,
                         c(ssf.happy, osf.happy, rom.happy),
                         factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.happy, osf.happy, rom.happy)) %>% 
  mutate(Emotion = "Happy") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.happy <- sd.happy %>% rename(ssf.men = ssf.happy_Men,
                          ssf.women = ssf.happy_Women,
                          osf.men = osf.happy_Men,
                          osf.women = osf.happy_Women,
                          rom.men = rom.happy_Men,
                          rom.women = rom.happy_Women)

### ADDING Surprised  

sd.surprised <- data %>% gather(key = emotion.gathered, 
                             value = score,
                             c(ssf.surprised, osf.surprised, rom.surprised),
                             factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.surprised, osf.surprised, rom.surprised)) %>% 
  mutate(Emotion = "Surprised") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.surprised <- sd.surprised %>% rename(ssf.men = ssf.surprised_Men,
                                  ssf.women = ssf.surprised_Women,
                                  osf.men = osf.surprised_Men,
                                  osf.women = osf.surprised_Women,
                                  rom.men = rom.surprised_Men,
                                  rom.women = rom.surprised_Women)


### ADDING Upset  

sd.upset <- data %>% gather(key = emotion.gathered, 
                         value = score,
                         c(ssf.upset, osf.upset, rom.upset),
                         factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.upset, osf.upset, rom.upset)) %>% 
  mutate(Emotion = "Upset") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.upset <- sd.upset %>% rename(ssf.men = ssf.upset_Men,
                          ssf.women = ssf.upset_Women,
                          osf.men = osf.upset_Men,
                          osf.women = osf.upset_Women,
                          rom.men = rom.upset_Men,
                          rom.women = rom.upset_Women)




### ADDING Angry  

sd.angry <- data %>% gather(key = emotion.gathered, 
                         value = score,
                         c(ssf.angry, osf.angry, rom.angry),
                         factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.angry, osf.angry, rom.angry)) %>% 
  mutate(Emotion = "Angry") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.angry <- sd.angry %>% rename(ssf.men = ssf.angry_Men,
                          ssf.women = ssf.angry_Women,
                          osf.men = osf.angry_Men,
                          osf.women = osf.angry_Women,
                          rom.men = rom.angry_Men,
                          rom.women = rom.angry_Women)

### ADDING threatened  

sd.threatened <- data %>% gather(key = emotion.gathered, 
                              value = score,
                              c(ssf.threatened, osf.threatened, rom.threatened),
                              factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.threatened, osf.threatened, rom.threatened)) %>% 
  mutate(Emotion = "Threatened") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.threatened <- sd.threatened %>% rename(ssf.men = ssf.threatened_Men,
                                    ssf.women = ssf.threatened_Women,
                                    osf.men = osf.threatened_Men,
                                    osf.women = osf.threatened_Women,
                                    rom.men = rom.threatened_Men,
                                    rom.women = rom.threatened_Women)

### ADDING relieved  

sd.relieved <- data %>% gather(key = emotion.gathered, 
                            value = score,
                            c(ssf.relieved, osf.relieved, rom.relieved),
                            factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.relieved, osf.relieved, rom.relieved)) %>% 
  mutate(Emotion = "Relieved") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.relieved <- sd.relieved %>% rename(ssf.men = ssf.relieved_Men,
                                ssf.women = ssf.relieved_Women,
                                osf.men = osf.relieved_Men,
                                osf.women = osf.relieved_Women,
                                rom.men = rom.relieved_Men,
                                rom.women = rom.relieved_Women)



### ADDING jealous composite  

sd.comp.jealous <- data %>% gather(key = emotion.gathered, 
                                value = score,
                                c(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous),
                                factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous)) %>% 
  mutate(Emotion = "Comp Jealous") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.comp.jealous <- sd.comp.jealous %>% rename(ssf.men = comp.ssf.jealous_Men,
                                        ssf.women = comp.ssf.jealous_Women,
                                        osf.men = comp.osf.jealous_Men,
                                        osf.women = comp.osf.jealous_Women,
                                        rom.men = comp.rom.jealous_Men,
                                        rom.women = comp.rom.jealous_Women)

### ADDING anger composite  

sd.comp.anger <- data %>% gather(key = emotion.gathered, 
                              value = score,
                              c(comp.ssf.angry.upset, 
                                comp.osf.angry.upset, 
                                comp.rom.angry.upset),
                              factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = sd(score, na.rm = TRUE))%>% 
  mutate_at(vars("mean"), round, 2) %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = mean) %>% 
  pivot_wider(names_from = gender, 
              values_from = c(comp.ssf.angry.upset, 
                              comp.osf.angry.upset, 
                              comp.rom.angry.upset)) %>% 
  mutate(Emotion = "Comp Anger") %>% 
  select(Emotion, everything())

#Renaming the table headers so that they do not specify an emotion and are general 
sd.comp.anger <- sd.comp.anger %>% rename(ssf.men = comp.ssf.angry.upset_Men,
                                    ssf.women = comp.ssf.angry.upset_Women,
                                    osf.men = comp.osf.angry.upset_Men,
                                    osf.women = comp.osf.angry.upset_Women,
                                    rom.men = comp.rom.angry.upset_Men,
                                    rom.women = comp.rom.angry.upset_Women)



### Bind all of the rows last 
#It gets funky if you try to add one emotion at a time. 
#Remember, you need the same exact row names for the rows to bind properly 
sd.table <- bind_rows(sd.surprised, 
                      sd.upset, 
                      sd.angry, 
                      sd.jealous, 
                      sd.happy,
                      sd.threatened,
                      sd.relieved,
                      sd.sad,
                      sd.comp.jealous,
                      sd.comp.anger)

rm(sd.surprised, 
   sd.upset, 
   sd.angry, 
   sd.jealous, 
   sd.happy,
   sd.threatened,
   sd.relieved,
   sd.sad,
   sd.comp.jealous,
   sd.comp.anger)


### Combining my mean and sd tables! 

mean.sd.table <- bind_cols(mean.table, sd.table)

### Reorganizing my mean and sd table and renaming columns 

mean.sd.table <- mean.sd.table %>% select(Emotion...1,
                         ssf.men...2,
                         ssf.men...9,
                         ssf.women...3,
                         ssf.women...10,
                         osf.men...4,
                         osf.men...11,
                         osf.women...5,
                         osf.women...12,
                         rom.men...6,
                         rom.men...13,
                         rom.women...7,
                         rom.women...14
                         ) %>%
   rename(Emotion = Emotion...1,
          mean.ssf.men = ssf.men...2,
          sd.ssf.men = ssf.men...9,
          mean.ssf.women= ssf.women...3,
          sd.ssf.women = ssf.women...10,
          mean.osf.men = osf.men...4,
          sd.osf.men = osf.men...11,
          mean.osf.women = osf.women...5,
          sd.osf.women= osf.women...12,
          mean.rom.men = rom.men...6,
          sd.rom.men = rom.men...13,
          mean.rom.women = rom.women...7,
          sd.rom.women = rom.women...14)


library(kableExtra)
library(knitr)


mean.sd.table %>% kbl() %>% 
  kable_classic() %>% 
  add_header_above(c("Same Sex" = 1:3, "Same Sex" = 10))

?kable
?kbl

mean.sd.table %>% kbl(
  caption = "Means and Standard Deviations of Scores Split by Gender and Interloper Type"
) %>% 
  kable_classic()

