
library(tidyverse)

data <- read_csv("data/analyze.osf.data.csv")


#mean age of the sample 

data %>% summarize(age.mean = mean(age.participant),
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



