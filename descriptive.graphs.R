### Descriptive Graphs! 

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




#Different Types of Graphs That I Want: 

#A simple bar graph counting gender 
data %>% ggplot(mapping = aes( x = gender)
    ) +
  geom_bar()
#Pretty useless graph - just want the descriptive statistics
#Getting those descriptives 
data %>% group_by(gender) %>% summarize(n = n())


#Box Plots! 

#A box plot of jealousy composite split by Interloper type and Gender 
long.data %>% ggplot(mapping = aes(x = within.subject, 
                              y = jealous, 
                              color = gender
       )
) +
  geom_boxplot(outlier.colour = "blue",
              
  ) + 
  coord_cartesian(                  ylim = c(1, 5)
  )  +
  theme_classic() +
labs(x = "Interloper", y = "Jealousy")


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


### Creating Pirate Plots! 

library(ggpirate)

long.data %>% ggplot(mapping = aes(x = within.subject,
                        y = happy, 
                        color = gender)
                     ) + 
            geom_pirate( bars = FALSE
            ) + theme_classic() +
            theme(legend.position = "bottom")

?theme

#"labs" allows you to manually change the x and y axis labels 
#"scale_x_discrete" allows you to manipulate your x scale 
# within this, use "labels" to change to the names 
#"scale_y_continuous" allows you to manipulate your y scale 
# within this, set "limits" and "breaks" to increment the "seq" 
#"theme_bw()" is a black and white theme that is good 
#"theme" allows you to specify other aspects of how your plot should look 
#"panel.grid.major.x" set to "element_blank()" removes vertical lines 
#define color using manual hex values "scale_color_manual()" 
# and "scale_fill_manual()" 


#NOTE: IF I WANT TO CREATE BOX PLOTS/PIRATE PLOTS WHERE I CAN COMPARE THE WITHIN
#SUBJECTS DATA. I NEED TO EDIT MY DATA FRAME SO EACH CONDITION IS GATHERED 





#Creating a Violin plot (A pirate plot does this too) 

long.data %>% ggplot(mapping = aes(x = within.subject,
                                   y = happy, 
                                   color = gender)
) + 
  geom_violin()  + theme_classic() +
  theme(legend.position = "bottom")







