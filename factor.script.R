

## Coding for my factors on my OSF honors thesis 

#Run this BEFORE performing analyses on the primary data file. 
#This is done because when you try to save a data file with factors 
#It does not load the factors back into the program 
#Thus, to make sure the factors are retained, I can just run this file before
#performing any analyses 

#This file tidy's up my analyses files so they don't have an extra chunk of 
#random code. 


#Turning specific data frames into factors for easier future use!
#Renaming the factor levels while recoding! 

data <- data %>% 
  mutate(gender = factor(gender) %>%  
           recode_factor("1" = "Men", 
                         "2" = "Women"))

data <- data %>% 
  mutate(single.or.dating = factor(single.or.dating) %>%  
           recode_factor("1" = "Single", 
                         "2" = "Relationship"))

data <- data %>% 
  mutate(relationship.status.participant = factor(relationship.status.participant) %>% 
           recode_factor("1" = "Single", 
                         "2" = "Casually Dating",
                         "3" = "Commited Relationship",
                         "4" = "Engaged/Married"))

data <- data %>% 
  mutate(sexual.orientation.participant = factor(sexual.orientation.participant) %>% 
           recode_factor("1" = "Bisexual",
                         "2" = "Heterosexual",
                         "3" = "Homosexual",
                         "4" = "Non-binary/other"
           )
  )

data <- data %>% 
  mutate(sexual.orientation.friend = factor(sexual.orientation.friend) %>% 
           recode_factor("1" = "Bisexual",
                         "2" = "Heterosexual",
                         "3" = "Homosexual",
                         "4" = "Non-binary/other")
  )
