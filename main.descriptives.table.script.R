
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
        

### Bind all of the rows last 
#It gets funky if you try to add one emotion at a time. 
  #Remember, you need the same exact row names for the rows to bind properly 
table <- bind_rows(jealous, happy, sad)


### ATTEMPTING TO ADD IN THE SD's 

jealous.1 <- data %>% gather(key = emotion.gathered, 
                           value = score,
                           c(ssf.jealous, osf.jealous, rom.jealous),
                           factor_key = TRUE) %>% 
  select(gender, emotion.gathered, score) %>% 
  group_by(gender, emotion.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE),
            sd = sd(score, na.rm= TRUE)) %>% 
  mutate_at(vars("mean","sd"), round, 2) %>% 
  unite("mean(sd)", mean:sd, sep = " \t\r\n") %>% 
  pivot_wider(names_from = emotion.gathered,
              values_from = "mean(sd)") %>% 
  pivot_wider(names_from = gender, 
              values_from = c(ssf.jealous, osf.jealous, rom.jealous)) %>% 
  mutate(Emotion = "Jealous") %>% 
  select(Emotion, everything()) 

separate_


?mutate_at
?separate
?unite
?round

  ?format

?format.data.frame