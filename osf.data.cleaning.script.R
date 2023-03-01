
library(tidyverse)

read_csv("osf.thesis/raw.osf.thesis.data.csv")

raw.data <-  read_csv("osf.thesis/data/raw.osf.thesis.data.csv")

glimpse(raw.data)

#Overwrite data and adding a participant id column
data.organized <- raw.data %>% mutate(participant.id = 1:164)

#Overwrite data and renaming all variables to lowercase 
data.organized <- data.organized %>% rename_all(tolower)


#Removing columns that I won't analyze 

data.organized <- data.organized %>% 
  select(-meeting.type, -ends_with("communication.type"))


#Moving participant id column to the front of the data set
#selected participant_id column by  name and then selected all other columns by number

data.organized <- data.organized %>% select(participant.id, 1:37)


#Checking for the extra participant that I removed 

data.organized %>% select(participant.id,
                          age.participant) %>%
  arrange(desc(age.participant))

#Found a participant who's age I entered incorrectly. 
#participant is not "211" years old. Participant is "21" years old. 

#Updating participant age 

data.organized <- data.organized %>% 
  mutate(age.participant = replace(age.participant, age.participant == 211, 21))


#Removing participants  

#removing participants older than 24 and who did not report a gender 
data.removed.participants <- data.organized %>% 
  filter(age.participant <= 24,
                    gender != 3, )

#finding participants who are dating their osf 
dating.osf <- data.removed.participants %>% 
  select(participant.id, clearly.dating.their.osf) %>%
  arrange(desc(clearly.dating.their.osf == 1))

#participant.id = 123 is dating osf and should be removed 
#removing participant 
          
data.removed.participants <- data.removed.participants %>% 
  filter(participant.id != 123)

#removing data object "dating.osf" because it is unnecessary 

rm(dating.osf)


#removing column "clearly.dating.their.osf" 

data.removed.participants <- data.removed.participants %>% 
  select(-clearly.dating.their.osf)

#mean age of the sample 
summarize(data.removed.participants, age.mean = mean(age.participant))

### Shortened Version of Above Code 

#
#
#
#
#

raw.data <-  read_csv("osf.thesis/data/raw.osf.thesis.data.csv")

glimpse(raw.data)


### DATA CLEANING

#Overwrite data and adding a participant id column
data.1 <- raw.data %>% mutate(participant.id = 1:164) %>% 
  rename_all(tolower)  %>%  #renaming all variables to lowercase 
    select(-meeting.type, -ends_with("communication.type")) %>% #removing columns I won't analyze
    select(participant.id, 1:37) #moving participant.id to front of data set


#Checking for the extra participant that I removed (because age entered wrong)

data.1 %>% select(participant.id,
                          age.participant) %>%
  arrange(desc(age.participant))

#Found a participant who's age I entered incorrectly. 
#participant is not "211" years old. Participant is "21" years old. 

#Updating participant age 

data.1 <- data.organized %>% 
  mutate(age.participant = replace(age.participant, age.participant == 211, 21))


#Removing participants  

#finding participants who are dating their osf 
dating.osf <- data.1 %>% 
  select(participant.id, clearly.dating.their.osf) %>%
  arrange(desc(clearly.dating.their.osf == 1))


#removing data object "dating.osf" because it is unnecessary 
rm(dating.osf)

#removing participants based on several criteria 
data.2 <- data.1 %>% 
  filter(age.participant <= 24,
         gender != 3,
         participant.id != 123) %>% 
  select(-clearly.dating.their.osf) #removing column "clearly.dating.their.osf" 



#mean age of the sample 
summarize(data.removed.participants, age.mean = mean(age.participant))


