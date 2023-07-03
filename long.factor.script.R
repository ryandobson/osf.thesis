
## Coding for my factors on my OSF honors thesis 

#THIS FILE IS FOR THE LONG DATA SET

#Run this BEFORE performing analyses on the primary data file. 
#This is done because when you try to save a data file with factors 
#It does not load the factors back into the program 
#Thus, to make sure the factors are retained, I can just run this file before
#performing any analyses 

#This file tidy's up my analyses files so they don't have an extra chunk of 
#random code. 


#Turning specific data frames into factors for easier future use!


long.data <- long.data %>% 
  mutate(gender = factor(gender) %>%  
           recode_factor("1" = "Men", 
                         "2" = "Women"))

long.data <- long.data %>% 
  mutate(single.or.dating = factor(single.or.dating) %>%  
           recode_factor("1" = "Single", 
                         "2" = "Relationship"))

long.data <- long.data %>% 
  mutate(relationship.status.participant = factor(relationship.status.participant) %>% 
           recode_factor("1" = "Single", 
                         "2" = "Casually Dating",
                         "3" = "Commited Relationship",
                         "4" = "Engaged/Married"))


long.data <- long.data %>% 
  mutate(within.subject = factor(within.subject) %>% 
           recode_factor("1" = "SSF",
                         "2" = "OSF",
                         "3" = "Romantic Partner")
  )

long.data <- long.data %>% 
  mutate(osf.closer.ssf = factor(osf.closer.ssf) %>% 
           recode_factor("1" = "not.closer",
                         "2" = "closer")
  )




