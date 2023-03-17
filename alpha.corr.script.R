
#Running Alpha Reliabilities 

##install.packages("psych")

library(psych)

##install.packages("Hmisc")
library(Hmisc) #package for testing multiple correlations at once. 

library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")

#Sourcing in my factors for data analysis 
source("factor.script.R")
str(data)
### Checking out the structure of the data frame! 
#Did this to confirm that my columns are numeric and my levels are factors 

#Getting a glimpse of my data frame. 

data %>% glimpse 


#Sourcing in my LONG data for graphs 

long.data <- read_csv("data/long.analyze.osf.data.csv") 
source("long.factor.script.R")
str(long.data)


#To use alpha I first need to get my correlation matrix! 
#In my case, I just have two different composite variables to make matrix's off 

#Jealousy and Threatened correlation matrix to use to calculate alpha 
osf.jeal <- data %>% select(osf.jealous, osf.threatened) %>% 
     cor()
 
#Jealousy and Threatened correlation matrix to use to calculate alpha 
ssf.jeal <- data %>% select(ssf.jealous, ssf.threatened) %>% 
  cor(use = "complete.obs")

#Jealousy and Threatened correlation matrix to use to calculate alpha 
rom.jeal <- data %>% select(rom.jealous, rom.threatened) %>% 
  cor( use = "complete.obs")

#Using my matrix's to calculate alpha 
alpha(ssf.jeal)
alpha(osf.jeal)
alpha(rom.jeal)


#Angry and Upset correlation matrix's to use to calculate alpha 
ssf.anger <- data %>% select(ssf.angry, ssf.upset) %>% 
  cor(use = "complete.obs")

osf.anger <- data %>% select(osf.angry, osf.upset) %>% 
  cor(use = "complete.obs")

rom.anger <- data %>% select(rom.angry, rom.upset) %>% 
  cor(use = "complete.obs")

alpha(ssf.anger)
alpha(osf.anger)
alpha(rom.anger)


#Physical attraction to friend correlation matrix to use to calculate alpha 

cor.attr <- data %>% select(romantic.attraction.tofriend, sexual.attraction.tofriend) %>% 
  cor( use = "complete.obs")

alpha(cor.attr)


#Scatter plot matrices of jealous/threatened variable 
data %>% select(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous) %>% 
  pairs.panels()

#Scatter plot matrix of several opposite sex friend variables 
data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
  pairs.panels()

#Correlation plot for several variables! This essentially replaces any 
#correlation table that I might want for descriptives. 
data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
  corPlot(numbers = TRUE, upper = FALSE, diag = FALSE)
#This correlation plot does not have confidence intervals though. 

#Creating a correlation matrix of some variables to get my CI's and then 
#create a plot with correlations and CI's 
plot.cor <- data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
  cor()


#This generates the confidence intervals! 
ci <- data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
corCi()


#This plots the CI's on an upper and lower format 
#(lower CI is below diag)
#(upper CI is upper diag)
plot.ci <- corPlotUpperLowerCi(ci)


data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
  corCi(p = .05, n.iter = 5000, method = "pearson")

##Testing my correlations 


cor.test(data$osf.angry, data$osf.upset)


#Creating different filters to test the correlations split by men and women 
data.men <- data %>% filter(gender == "Men")

data.women <- data %>% filter(gender == "Women")

data.men %>% cor.test(data$comp.osf.jealous, data$sexual.attraction.tofriend)

cor.test(data.men$comp.osf.jealous, data.men$sexual.attraction.tofriend)
cor.test(data.women$comp.osf.jealous, data.women$sexual.attraction.tofriend)




