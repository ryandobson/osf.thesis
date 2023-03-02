

##Initial Data Cleaning 3/1/2023 

library(tidyverse)

getwd()

#loading the raw data set and writing it to a data object to manipulate it 
raw.data <- 
  read_csv("data/raw.osf.thesis.data.csv")

#renaming all of the variables to lowercase for clarity 
raw.data <- raw.data %>% rename_all(tolower)

#Removing communication type variables since I don't plan on analyzing them 
raw.data <- raw.data %>% select(
 c(-"meeting.type", -"inperson.communication.type", -"phone.communication.type", -"email.communication.type", -"socialnetwork.communication.type") 
)

#Creating a participant ID variable so I can look at people and sort through things 
#And moving the participant.id variable to the front of the data set for easy access 
raw.data <- raw.data %>% mutate(participant.id = 1:164) %>% 
  select(participant.id, everything())


#Searching for participants who are outside of the age limit 
raw.data %>% select(participant.id, age.participant) %>% 
  arrange(desc(age.participant))

#Participant 73 has an incorrect number typed in! 
#need to change participant 73's age to 21 instead of 211 

raw.data <- raw.data %>% 
  mutate(age.participant = replace(age.participant, age.participant == 211, 21))


#Participant 13 is over the age limit, 24. Need to remove participant 13 as they are 38. 
#Also removing participants who reported being a gender other than male or female 
raw.data <- raw.data %>% 
  filter(age.participant <= 24, #include all people less than or equal to 24 years old 
         gender != 3,) #include all people who are not a "3" 

#Finding participants who are dating their OSF! 
raw.data %>% select(participant.id, clearly.dating.their.osf) %>% 
  arrange(clearly.dating.their.osf)

#Participants 3, 123, and 128 appear to be dating their OSF. 
#I just removed one of them because that was the only participant I could confirm it with because they 
#wrote on their questionnaire that they were dating their OSF. 

#Removing participant 3 

raw.data <- raw.data %>% 
  filter(participant.id != 3) #include all of the people other than the participant who has the id "3" 


#Saving an edited version of the data set with final participants 

raw.data %>% write_csv("data/clean.osf.data.csv")

#Reading new data set and finishing editing it for analysis 

data <- read_csv("data/clean.osf.data.csv")


#removing variable because I don't need it anymore 
data <- data %>% select(-clearly.dating.their.osf) 

#removing contact frequency variable because I don't need it 
data <- data %>% select(-contact.frequency)

#Adding a re coded relationship status variable 
#1 = single 
#2 = in a relationship 

data <- data %>% 
  mutate(single.or.dating = relationship.status.participant) %>% 
  mutate(single.or.dating = replace(single.or.dating, single.or.dating == 2, 1),
         single.or.dating = replace(single.or.dating, single.or.dating == 4, 2), 
         single.or.dating = replace(single.or.dating, single.or.dating == 3, 2),
  )

#Getting n's for participants who are single versus dating     
data %>% group_by(single.or.dating) %>% 
  summarize(n = n())

#Getting n's for participants relationship status 
data %>% group_by(relationship.status.participant) %>% 
  summarize(n = n())


#Creating Composite Variables! 

#Computing composite variables 
comp.var <- data %>% 
  transmute ( 
    comp.osf.jealous = (osf.jealous + osf.threatened) / 2,
    comp.ssf.jealous = (ssf.jealous + ssf.threatened) / 2,
    comp.rom.jealous = (rom.jealous + rom.threatened) / 2,
    comp.osf.angry.upset = (osf.angry + osf.upset) / 2, 
    comp.ssf.angry.upset = (ssf.angry + ssf.upset) / 2,
    comp.rom.angry.upset = (rom.angry + rom.upset) / 2, 
    
  )

#saved to a new variable to double check that they computed correctly. 


#Stiching comp.var into data! 

data <- bind_cols(data, comp.var)

?bind_rows

#Removing the specific composite variables because they are not needed anymore! 
rm(comp.var)


#Computing romantic and physical attraction composite 
comp.var <- data %>% 
  transmute ( 
    comp.rom.phys = (romantic.attraction.tofriend + sexual.attraction.tofriend) / 2)

#Binding romantic attraction variable and removing the old variable 

data <- bind_cols(data, comp.var)
rm(comp.var)

data <- data %>% rename(comp.rom.sex = comp.rom.phys)


#Writing the new data object to a csv file for future use! 

data %>% write_csv("data/analyze.osf.data.csv")







