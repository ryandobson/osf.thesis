
library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")

#?source

#Sourcing in my factors for data analysis 
source("factor.script.R")
str(data)
### Checking out the structure of the data frame! 
#Did this to confirm that my columns are numeric and my levels are factors 


str(composites)


composites <- data %>% gather(key = emotion, 
                              value = score,
                              comp.osf.jealous:comp.rom.sex,
                              factor_key = TRUE) 

composites <- composites %>% select(emotion, gender, score)


str(composites)

comp.stats <- composites %>% group_by(emotion, gender) %>% 
  summarize(mean = mean(score, na.rm = TRUE)
  )

comp.stats<- comp.stats %>% spread(key = gender, 
                                   value = mean)


test <- data %>% gather(key = jealousy.gathered, 
                        value = score,
                        c(ssf.jealous, osf.jealous, rom.jealous),
                        factor_key = TRUE) %>% 
  select(gender, jealousy.gathered, score)

new.comp.stats <- test %>% group_by(gender, jealousy.gathered) %>% 
  summarize(mean = mean(score, na.rm = TRUE)
  )


newest.comp.stats <- new.comp.stats %>% pivot_wider(names_from = jealousy.gathered,
                                                    values_from = mean, 
) 

final.comp <- newest.comp.stats %>% pivot_wider(names_from = gender, 
                                                values_from = c(ssf.jealous, osf.jealous, rom.jealous))








final.comp <- final.comp %>% rename(ssf.men = ssf.jealous_Men,
                      ssf.women = ssf.jealous_Women,
                      osf.men = osf.jealous_Men,
                      osf.women = osf.jealous_Women,
                      rom.men = rom.jealous_Men,
                      rom.women = rom.jealous_Women)


?mutate

### PROCESS OF CREATING A TABLE


#Jealousy <- data %>% gather(key = emotion, 
 #                             value = score,
  #                            c(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous),
   #                           factor_key = TRUE) 
#Jealousy <- Jealousy %>% select(emotion, gender, score)




#jeal.comp <- Jealousy %>% group_by(emotion, gender) %>% 
 # summarize(mean = mean(score, na.rm = TRUE)
  #)
#
#jeal.comp <- jeal.comp %>% pivot_wider(names_from = emotion, 
 #                                    values_from = c("Men", "Women"))


#test %>% drop_na(score) %>% pivot_wider(names_from = gender, 
 #                                       values_from = score, 
  #                                      values_fn = mean)



?mutate


###COULD I PARSE OUT MALE AND FEMALE PARTICIPANTS AND THEN TIE THEM BACK 
#TOGETHER LATER?? 

###OR COULD I PARSE OUT THE WITHIN SUBJECTS VARIABLE AND THEN BRING THINGS BACK 
#TOGETHER SOMEHOW? 


simple.data <- data %>% select(comp.ssf.jealous,
                               comp.osf.jealous,
                               comp.rom.jealous,
                               comp.ssf.angry.upset,
                               comp.osf.angry.upset,
                               comp.rom.angry.upset,
                               ssf.happy,
                               osf.happy,
                               rom.happy,
                               ssf.sad,
                               osf.sad,
                               rom.sad,
                               gender, 
                               relationship.status.participant,
                               osf.closer.ssf,
                               comp.rom.sex)

simple.data %>% group_by(gender) %>% 
  summarize(mean = mean(comp.ssf.jealous, na.rm = TRUE)
  )

simple.data %>% pivot_wider(names_from = gender, 
            values_from = c("comp.ssf.jealous", "comp.osf.jealous"))



