

library(tidyverse)

#Load Data 
data <- read_csv("data/osf.clean.data.file.csv")



#Sample Characteristics 
descriptives <- data %>% 
  group_by(gender) %>%
  summarize(n = n())

?summarize

#Computing composite variables 
comp.var <- data %>% 
  transmute (participant.id, gender, 
             osf.jealous = (osf.jealous + osf.threatened) / 2,
             ssf.jealous = (ssf.jealous + ssf.threatened) / 2,
             rom.jealous = (rom.jealous + rom.threatened) / 2,
             osf.angry.upset = (osf.angry + osf.upset) / 2, 
             ssf.angry.upset = (ssf.angry + ssf.upset) / 2,
             rom.angry.upset = (rom.angry + rom.upset) / 2,
             
  )

comp.var


# calculate means and standard deviations across gender 

mean.sd <- comp.var %>% summarize_at(
  c("osf.jealous", "ssf.jealous", "rom.jealous",
    "osf.angry.upset", "ssf.angry.upset", "rom.angry.upset"), 
  list(mean = mean, sd = sd), na.rm = TRUE)

# calculate means and sd split by gender 

men.mean.sd <- comp.var %>% 
  filter(gender == 1) %>% 
  summarize_at( 
    c("osf.jealous", "ssf.jealous", "rom.jealous",
    "osf.angry.upset", "ssf.angry.upset", "rom.angry.upset"), 
list(mean = mean, sd = sd), na.rm = TRUE) 

women.mean.sd <- comp.var %>% 
  filter(gender == 2) %>% 
  summarize_at( 
    c("osf.jealous", "ssf.jealous", "rom.jealous",
      "osf.angry.upset", "ssf.angry.upset", "rom.angry.upset"), 
    list(mean = mean, sd = sd), na.rm = TRUE) 


bothgender.mean.sd <- comp.var %>% 
  group_by(gender) %>%
  summarize_at( 
    c("osf.jealous", "ssf.jealous", "rom.jealous",
      "osf.angry.upset", "ssf.angry.upset", "rom.angry.upset"), 
    list(mean = mean, sd = sd), na.rm = TRUE) 





## Making Histograms of Data 

data %>% ggplot(mapping = aes(x = gender)) + geom_histogram() 

data %>% ggplot(mapping = aes(x = relationship.status.participant)) + geom_histogram()

?geom_histogram



### NOTE 
# Here I am struggling to determine how to format my data to put it into graphs 
#and into my descriptives properly. 
# I think that I need to transform something into long/wide format before doing
#things


#graphing osf jealousy 


comp.var %>% ggplot(mapping = aes(x = gender, y = osf.jealous)) +
  geom_boxplot() 





