
#Creating Graphs and Functions for Graphs 

##install.packages("psych")


library(tidyverse)
library(psych)


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


#> Example Box Plot to create a function with ------
#A box plot of happy split by Interloper type and Gender 
long.data %>% ggplot(mapping = aes(x = within.subject, 
                                   y = happy, 
                                   color = gender
)
) +
  geom_boxplot(outlier.colour = "blue",
               
  ) + 
  coord_cartesian(                  ylim = c(1, 5)
  )  +
  theme_classic() +
  labs(x = "Interloper", y = "Happy")


#> Box plot function! ------
#> NOTE: this is a pretty specific function for this data set because of the 
#> within/between subjects nature of the design. 
#> Still a useful function though. And good practice. 

boxplot.function <- function(data, variable, group_var) {
  
  var.name <- rlang::englue("{{variable}}")

  data |> ggplot(mapping = aes(x = within.subject, 
                               y = {{variable}}, 
                               color = {{group_var}}
  )
  ) +
    geom_boxplot(outlier.colour = "blue",
                 
    ) + 
    coord_cartesian(                  ylim = c(1, 5)
    )  +
    theme_classic() +
    labs(x = "Interloper", y = var.name)
  
}

boxplot.function(long.data, angry, gender)
boxplot.function(long.data, jealous, osf.closer.ssf)


#> Scatter Plot Examples ------
#> 
data |> ggplot(mapping = aes(x = comp.osf.jealous,
                             y = comp.rom.angry.upset, 
                             color = gender)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE) +
  scale_x_continuous(
    breaks = seq(1, 5, by = 1)
  ) +
  theme_classic() +
  labs(
    title = "Correlation between Jealousy and Anger",
    x = "Anger",
    y = "Jealousy"
  ) +
  theme(plot.title = element_text(hjust = .5, vjust = 2))

#> Recreating one supplementary figure for romantic/physical attraction -----

#> A character string for my annotation on my graph
osf_att_cor <- "r = .55, 95% CI: [.42, .64]"


data |> ggplot(mapping = aes(x = comp.rom.sex,
                             y = comp.osf.jealous)) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "#ADD8E6")  +
  coord_cartesian(ylim = c(1, 5),
                  xlim = c(1, 7)) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  scale_y_continuous(breaks = seq(1, 5, by = 1)) +
  theme_classic() +
  labs(
    title = "Correlation Between Romantic/Physical Attraction with Jealousy",
    x = "Low --------------------------------------------------------------------------- High\nRomantic/Physical Attraction",
    y = "OSF Interloper Jealousy\nLow ------------------------------------------ High"
  ) +
  theme(plot.title = element_text(hjust = .5, vjust = 2,
                                  margin = margin(t = 8, b = 8)),
        axis.title.y = element_text(margin = margin(r = 8, l = 8)),
        axis.title.x = element_text(margin = margin(t = 8, b = 8)),
        axis.text = element_text(size = 11)) +
  annotate(
    geom = "label", x = 2, y = 4.2,
    label = osf_att_cor,
    hjust = "left", color = "#DC143C",
    
  )


osf_att_cor <- "r = .55, 95% CI: [.42, .64]"


?theme
?axis.title.y
?labs
?scale_x_continuous
?annotate

#> Attempting to turn my graph into a function -----

supp_scat_fun <- function(data, x_var, y_var) { 

data |> ggplot(mapping = aes(x = {{x_var}},
                             y = {{y_var}})) +
  geom_point() +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "#ADD8E6")  +
  coord_cartesian(ylim = c(1, 5),
                  xlim = c(1, 7)) +
  scale_x_continuous(breaks = seq(1, 7, by = 1)) +
  scale_y_continuous(breaks = seq(1, 5, by = 1)) +
  theme_classic() +
  labs(
    title = "Correlation Between Romantic/Physical Attraction with Jealousy",
    x = "Low --------------------------------------------------------------------------- High\nRomantic/Physical Attraction",
    y = "OSF Interloper Jealousy\nLow ------------------------------------------ High" 
  ) +
  theme(plot.title = element_text(hjust = .5, vjust = 2,
                                  margin = margin(t = 8, b = 8)),
        axis.title.y = element_text(margin = margin(r = 8, l = 8)),
        axis.title.x = element_text(margin = margin(t = 8, b = 8)),
        axis.text = element_text(size = 11)) +
  annotate(
    geom = "label", x = 2, y = 4.2,
    label = osf_att_cor,
    hjust = "left", color = "#DC143C",
    
  )
}
#> NOTE: the function works with the data, but the title, axis, and annotation
#> remain unchanged so far. 
supp_scat_fun(data, comp.rom.sex, comp.ssf.jealous)

#> NOTE: considering I'm only making three of these, its really fine to just copy
#> and paste and change things as needed. 
#> That is still much faster than changing some things in word. 



#> Creating some density graphs to look at how my variables are distributed. ----
#> 

data |> ggplot(aes(x = comp.osf.jealous)) +
  geom_density() +
  theme_classic()

#> Creating the density plot in a function -------
#>  so I can display multiple plots at once 
#> 

den.plot.fun <- function(data, variables) {
 

  data_long <- data |> select({{variables}}) |> 
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "value") 
  
   data_long |> ggplot(aes(x = value)) +
    geom_density() +
    theme_classic() + 
    facet_wrap(~variable)
  
} 
den.plot.fun(data, osf.surprised:osf.sad)


#> changing the plots to histograms -------
#>  since I don't really have continuous variables
hist.plot.fun <- function(data, variables) {
  
  
  data_long <- data |> select({{variables}}) |> 
    pivot_longer(cols = everything(),
                 names_to = "variable",
                 values_to = "value") 
  
  data_long |> ggplot(aes(x = value)) +
    geom_histogram(
    ) +
    theme_classic() + 
    facet_wrap(~variable)
  
} 
hist.plot.fun(data, osf.surprised:osf.sad)

hist.plot.fun(data, comp.osf.jealous:comp.rom.sex)


#> Creating a Bar Graph! -------
#> 

bar_graph_fun <- function(data, group_var, variable) {

mean_se <- data |> 
  filter(!is.na({{group_var}})) |> 
  group_by({{group_var}}) |> 
  summarize(mean = mean({{variable}}, na.rm = TRUE),
            se = (sd({{variable}}, na.rm = TRUE) / sqrt(length({{variable}}))) * 2)


mean_se |> ggplot(aes(x = {{group_var}}, y = mean)) +
                 geom_bar(
                   stat = "identity", 
                   fill = "lightblue",
                   width = .5) +
                 geom_errorbar(aes(ymin = mean - se,
                                   ymax = mean + se),
                                   width = .2,
                                   size = 1) + 
                 coord_cartesian(ylim = c(1, 5)) 


}



bar_graph_fun(data, gender, osf.surprised)

bar_graph_fun(data, gender, comp.osf.jealous)

bar_graph_fun(data, osf.closer.ssf, comp.osf.jealous)

bar_graph_fun(data, single.or.dating, comp.osf.jealous)


?geom_bar





