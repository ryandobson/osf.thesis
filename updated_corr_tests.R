
#Testing Different Correlations and Returning Correlation Matrices 

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



#Correlation plot for several variables! ------ 
#This essentially replaces any 
#correlation table that I might want for descriptives. 
data |>  select(osf.angry, osf.upset, osf.sad, osf.happy) |>  
  corPlot(numbers = TRUE, upper = FALSE, diag = FALSE)

#This generates the confidence intervals! 
#> NOTE: you need to view the object to see the CI's 
ci <- data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
  corCi()
ci


#> Scatter Plot Matrices with lots of information ----
#Scatter plot matrices of jealous/threatened variable 
data %>% select(comp.ssf.jealous, comp.osf.jealous, comp.rom.jealous) %>% 
  pairs.panels()

#Scatter plot matrix of several opposite sex friend variables 
data %>% select(osf.angry, osf.upset, osf.sad, osf.happy) %>% 
  pairs.panels()


#> Turning the scatter plot matrices into a function ----- (since I might want lots 
#> of variations) 

plot.creation <- function(data, variables) {
  data |> select({{variables}}) |> 
    pairs.panels() 
  
}

plot.creation(data, osf.surprised:osf.sad)
plot.creation(data, ssf.surprised:ssf.sad)
plot.creation(data, rom.surprised:rom.sad)

plot.creation(data, comparison.attractive:sexual.attraction.tofriend)


?rcorr
#> Using rcorr ------
#> 
new.data <- data |> select(osf.surprised:osf.sad)

result <- rcorr(data$osf.angry, data$comparison.attractive)
result

#> NOTE: You are supposed to be able to use it by selecting one data frame with 
#> a set of variables. That is not working for me. But I could get it to work 
#> by selecting two variables separetly. 
#> Which does not make this function any more useful than the original cor.test
#> or just looking at upper and lower CI's, which I did above. 

cor.test(data$osf.angry, data$osf.upset)

?cor.test

#> An r generated function to use rcorr -----

library(Hmisc)

multi_cor_test <- function(data, vars, method="pearson") {
  cor_matrix <- rcorr(as.matrix(data[,vars]), type=method)
  p_values <- cor_matrix$P
  
  # Convert lower triangle of p-values matrix to NA
  p_values[lower.tri(p_values, diag=FALSE)] <- NA
  
  # Add row and column names
  rownames(p_values) <- colnames(p_values) <- vars
  
  # Return results as a data frame
  results <- data.frame(cor_matrix$r, p_values)
  colnames(results) <- paste0("corr_", colnames(results))
  results
}

#> NOTE: you have to select columns by number because they lose names when going
#> from a data frame to a matrix 
rcorr.results <- multi_cor_test(data, 2:8)

cor.test(data$osf.upset, data$osf.angry)
#> NOTE: this is somewhat useful for finding the correlation between all variables.
#> But it does not easily display the p-values or CI's 



