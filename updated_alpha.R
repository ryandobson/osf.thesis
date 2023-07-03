
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


#> This updated version is an attempt to calculate Cronbach's alphas with a 
#> function in order to simplify my code! 
#> 
#> The first step is creatin correlation matrices to analyze. ------

#Jealousy and Threatened correlation matrix to use to calculate alpha 
osf.jeal <- data %>% select(osf.jealous, osf.threatened) %>% 
  cor()

#Jealousy and Threatened correlation matrix to use to calculate alpha 
ssf.jeal <- data %>% select(ssf.jealous, ssf.threatened) %>% 
  cor(use = "complete.obs")

?cor

#> Then you simply just apply the alpha argument to the matrices. 
psych::alpha(osf.jeal)


#> Making the function -----

calc_alphas <- function(data, variables) {
  
  cor.matrix <- data |> 
                  select({{variables}}) |> 
                  cor(use = "complete.obs")
  
  psych::alpha(cor.matrix,
               check.keys = TRUE)

}
calc_alphas(data, c(osf.jealous, osf.threatened))

calc_alphas(data, osf.surprised:osf.sad)

#> The function works well and I just have to select the data frame and then 
#> the variables that I need for analysis. 
 





