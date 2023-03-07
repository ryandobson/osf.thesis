
library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")

#?source
 
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


#Converting summary statistics to a data frame 
data.sum <- tibble(unclass(summary(data)),
                   check.names = FALSE)

data.sum

### ATTEMPS TO CREATE A USEFUL DATA FRAME TO VIEW DESCRIPTIVES 

data %>% group_by(gender) %>%  
   drop_na(osf.happy) %>% #can use this to drop.na's instead of specifying within!
  summarize(mean.osf.happy = mean(osf.happy),
            sd = sd(osf.happy, na.rm = TRUE), 
            osf.jealous = mean(osf.jealous)
            )
?summarize_each
?across


#A good way to summarize aross a few specific columns 
#data %>% summarize(across(c(osf.surprised, osf.angry), 
 #                         list(mean = mean, sd = sd), 
  #                        na.rm = TRUE, 
   #                       .names = "{col} {fn}"))
    

#A good way to summarize multiple statistics across several columns                                        
osf.stats <- data %>% group_by(gender) %>%  summarize(across(starts_with("osf."), 
                          list(mean = mean, sd = sd), 
                          na.rm = TRUE, 
                          .names = "{col} {fn}"))

rom.stats <- data %>% group_by(gender) %>% 
  summarize(across(starts_with("rom."), 
                   list(mean = mean, sd = sd), 
                   na.rm = TRUE, 
                   .names = "{col} {fn}"))

ssf.stats <- data %>% group_by(gender) %>% 
  summarize(across(starts_with("ssf."), 
                   list(mean = mean, sd = sd), 
                   na.rm = TRUE, 
                   .names = "{col} {fn}"))

comp.stats <- data %>% group_by(gender) %>% 
  summarize(across(starts_with("comp."), 
                   list(mean = mean, sd = sd), 
                   na.rm = TRUE, 
                   .names = "{col} {fn}"))


?tibble




#An incredibly inconvienent table of values. Although it returns the values nicely enough, it looks awful. 
bothgender.mean.sd <- data %>% 
  group_by(gender) %>%
  summarize_at( 
    c("comp.osf.jealous", "comp.ssf.jealous", "comp.rom.jealous",
      "comp.osf.angry.upset", "comp.ssf.angry.upset", "comp.rom.angry.upset"), 
    list(mean = mean), na.rm = TRUE) 


men <- data %>% filter(gender == "Men")

women <- data %>% filter(gender == "Women")


men <- men  %>% select(comp.ssf.jealous,
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

women <- women  %>% select(comp.ssf.jealous,
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