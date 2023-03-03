
library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")

?source
 
#Sourcing in my factors for data analysis 
source("factor.script.R")
str(data)
### Checking out the structure of the data frame! 
#Did this to confirm that my columns are numeric and my levels are factors 


#This is great to get a quick statistical overview! 
summary(data)
#Can get participant age mean and range. 

#Getting basic information about my data frame 
names(data) #returns the names of all my variables 

ncol(data) #returns the number of columns in the data frame 

length(data) #returns the length of the data frame(same as ncol)

dim(data) #gives number of rows and then number of columns 

nrow(data) #gives number of rows 

#NOTE: names(data) and dim(data) are really the only two needed 


### STEP 1: REPRODUCE ALL OF THE DESCRIPTIVE STATISTICS 

#Sample Size 

#total 
data %>% summarize (n = n())
#n = 158 

#by gender 
data %>% group_by(gender) %>% 
  summarize (n = n())
#1 = male 77 
#2 = female 81

### 

#Relationship status and sexual orientation n's  

data %>% group_by(gender) %>% 
  count(relationship.status.participant)
data %>% count(sexual.orientation.participant)

#mean age of the sample 

data %>% reframe(age.mean = mean(age.participant),
                   sd = sd(age.participant),
                   range = range(age.participant))


data %>% group_by(gender) %>% 
  summarize(age.mean = mean(age.participant),
            sd = sd(age.participant)
            ) 
  


data %>% group_by(relationship.status.participant) %>% 
  summarize(age.mean = mean(age.participant),
            sd = sd(age.participant)
  )

data %>% summarize( ro.att = mean(romantic.attraction.tofriend, na.rm = TRUE),
                   sd = sd(romantic.attraction.tofriend, na.rm = TRUE)
)


#An incredibly inconvienent table of values. Although it returns the values nicely enough, it looks awful. 
bothgender.mean.sd <- data %>% 
  group_by(gender) %>%
  summarize_at( 
    c("comp.osf.jealous", "comp.ssf.jealous", "comp.rom.jealous",
      "comp.osf.angry.upset", "comp.ssf.angry.upset", "comp.rom.angry.upset"), 
    list(mean = mean, sd = sd), na.rm = TRUE) 




