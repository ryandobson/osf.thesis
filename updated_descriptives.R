
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

### Means and SDs -------

data |> group_by(gender) |> 
  summarize(mean = mean(age.participant),
            sd = sd(age.participant)) 

data |> group_by(gender) |> 
  summarize(mean = mean(romantic.attraction.tofriend, na.rm = TRUE),
            sd = sd(romantic.attraction.tofriend, na.rm = TRUE))
  
#> A summary function that produces a mean column and SD column for every variable
#> This works great! (If I can add upper and lower CI's onto it that would be great)
mean_all <- function(summary_vars) { 
  mean.results <- data |>  summarize(across({{summary_vars}}, 
          \(x) (mean(x, na.rm = TRUE)))) |> 
    pivot_longer(cols = everything(), 
          names_to = "Variable",
          values_to = "Mean")
  sd.results <- data |>  summarize(across({{summary_vars}}, 
                                  \(x) (sd(x, na.rm = TRUE)))) |> 
     pivot_longer(
       cols = everything(), 
       names_to = "Variable",
       values_to = "SD")
 
 
 all.results <- mean.results |> left_join(sd.results)
 print(all.results)
}

mean_all(osf.surprised:rom.sad)

#>
#>
#>
#>
#>
### Attempting to group my successful mean/sd function -----

grouped_mean <- function(summary_vars) { 
  mean.results <- data |>  
    group_by(gender) |> 
    summarize(
       across({{summary_vars}}, 
          \(x) (mean(x, na.rm = TRUE)))) |> 
    pivot_longer(
      cols = {{summary_vars}},
      names_to = "Variable",
      values_to = "mean"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = mean
    )
  mean.results
  }  
  
grouped_mean(osf.surprised:rom.sad)


#> Now to add a group_mean and group_sd! -----
#> 
#> 
grouped_mean_sd <- function(summary_vars) { 
  mean.results <- data |>  
    group_by(gender) |> 
    summarize(
      across({{summary_vars}}, 
             \(x) (mean(x, na.rm = TRUE)))) |> 
    pivot_longer(
      cols = {{summary_vars}},
      names_to = "Variable",
      values_to = "mean"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = mean
    ) |> 
    rename(
      Men_Mean = Men,
      Women_Mean = Women
    )
 
  sd.results <- data |>  
    group_by(gender) |> 
    summarize(
      across({{summary_vars}}, 
             \(x) (sd(x, na.rm = TRUE)))) |> 
    pivot_longer(
      cols = {{summary_vars}},
      names_to = "Variable",
      values_to = "sd"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = sd
    ) |> 
    rename(
      Men_SD = Men,
      Women_SD = Women
    )

final.results <- mean.results |> left_join(sd.results)|> 
  relocate(Variable, Men_Mean, Men_SD, Women_Mean, Women_SD) |>
  mutate_if(is.numeric, ~round(., 2))
final.results
  }  

rom <- grouped_mean_sd(rom.surprised:rom.sad)
osf <- grouped_mean_sd(osf.surprised:osf.sad)
ssf <- grouped_mean_sd(ssf.surprised:ssf.sad)

rom |> mutate_if(is.numeric, ~round(., 2)) 

?mutate_if
grouped_mean_sd(comparison.attractive:sexual.attraction.tofriend)

?kbl
#>
#>
#>
#>
#>
#>
#>Attempting to calculate confidence intervals -----

data |>  
  summarize(
    mean = mean(osf.surprised),
    se = (sd(osf.surprised) / sqrt(length(osf.surprised))),
    se_2 = (sd(osf.surprised) / sqrt(length(osf.surprised))) * 2,
    up_ci = mean(osf.surprised, na.rm = TRUE) + 
      (sd(osf.surprised) / sqrt(length(osf.surprised))) * 2,
    lo_ci = mean(osf.surprised, na.rm = TRUE) - 
      (sd(osf.surprised) / sqrt(length(osf.surprised))) * 2
    ) 
#> Successfully calculated things from one variable! 



#>
#> Chat GPT Derived SEM function ------

# Function to calculate standard error of the mean using tidyverse
up_lo_ci <- function(data, variable) {
  up_ci <- data |> 
    summarize(across({{variable}}, 
                 \(x) sem = (mean(x, na.rm = TRUE)) + 
                      (sd(x, na.rm = TRUE) / sqrt(n())) * 2)) |> 
    pivot_longer(
      cols = everything(),
      names_to = "Variable",
      values_to = "up_ci"
    )
  
  lo_ci <- data |> 
    summarize(across({{variable}}, 
                     \(x) (mean(x, na.rm = TRUE)) - 
                       (sd(x, na.rm = TRUE) / sqrt(n())) * 2)) |> 
  pivot_longer(
    cols = everything(),
    names_to = "Variable",
    values_to = "lo_ci"
  )
  
  lo_up_ci <- lo_ci |> left_join(up_ci)
  lo_up_ci
  }


up_lo_ci(data, osf.surprised:osf.sad)


#>
#>
#>
#> Grouping the SEM function by gender --------

grouped_lo_up_ci <- function(data, variable) {
  up_ci <- data |> 
    group_by(gender) |> 
    summarize(across({{variable}}, 
                     \(x) (mean(x, na.rm = TRUE)) + 
                       (sd(x, na.rm = TRUE) / sqrt(n())) * 2)) |> 
     pivot_longer(
    cols = {{variable}},
    names_to = "Variable",
    values_to = "up_ci"
  ) |> 
    pivot_wider(
      names_from = gender,
      values_from = up_ci
    ) |> 
    rename(
      Men_up_ci = Men,
      Women_up_ci = Women
    )
  
  lo_ci <- data |> 
    group_by(gender) |> 
    summarize(across({{variable}}, 
                     \(x) (mean(x, na.rm = TRUE)) - 
                     (sd(x, na.rm = TRUE) / sqrt(n())) * 2)) |> 
  pivot_longer(
    cols = {{variable}},
    names_to = "Variable",
    values_to = "lo_ci"
  ) |> 
    pivot_wider(
      names_from = gender,
      values_from = lo_ci
    ) |> 
    rename(
      Men_lo_ci = Men,
      Women_lo_ci = Women
    )
  
  final.results <- lo_ci |> left_join(up_ci)|> 
    relocate(Variable, Men_lo_ci, Men_up_ci, Women_lo_ci, Women_up_ci)  
  final.results
  
  }


grouped_lo_up_ci(data, osf.surprised:osf.sad)




#> 
#> 
#> 
#> 
#>  The Holy Grail ----- 
#>  Means, SDs, and Upper and Lower CI's all in one function...

group_mean_sd_up_lo_ci <- function(variable) { 
  mean.results <- data |>  
    group_by(gender) |> 
    summarize(
      across({{variable}}, 
             \(x) (mean(x, na.rm = TRUE)))) |> 
    pivot_longer(
      cols = {{variable}},
      names_to = "Variable",
      values_to = "mean"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = mean
    ) |> 
    rename(
      Men_Mean = Men,
      Women_Mean = Women
    )
  
  sd.results <- data |>  
    group_by(gender) |> 
    summarize(
      across({{variable}}, 
             \(x) (sd(x, na.rm = TRUE)))) |> 
    pivot_longer(
      cols = {{variable}},
      names_to = "Variable",
      values_to = "sd"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = sd
    ) |> 
    rename(
      Men_SD = Men,
      Women_SD = Women
    )
  
  up_ci <- data |> 
    group_by(gender) |> 
    summarize(across({{variable}}, 
                     \(x) (mean(x, na.rm = TRUE)) + 
                       (sd(x, na.rm = TRUE) / sqrt(n())) * 2)) |> 
    pivot_longer(
      cols = {{variable}},
      names_to = "Variable",
      values_to = "up_ci"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = up_ci
    ) |> 
    rename(
      Men_up_ci = Men,
      Women_up_ci = Women
    )
  
  lo_ci <- data |> 
    group_by(gender) |> 
    summarize(across({{variable}}, 
                     \(x) (mean(x, na.rm = TRUE)) - 
                       (sd(x, na.rm = TRUE) / sqrt(n())) * 2)) |> 
    pivot_longer(
      cols = {{variable}},
      names_to = "Variable",
      values_to = "lo_ci"
    ) |> 
    pivot_wider(
      names_from = gender,
      values_from = lo_ci
    ) |> 
    rename(
      Men_lo_ci = Men,
      Women_lo_ci = Women
    )
  
  final.results <- mean.results |> 
    left_join(sd.results)|> 
    left_join(lo_ci) |> 
    left_join(up_ci) |> 
    relocate(Variable, 
             Men_lo_ci,
             Men_Mean,
             Men_up_ci,
             Men_SD, 
             Women_lo_ci,
             Women_Mean, 
             Women_up_ci,
             Women_SD)  
  final.results
}  

full.table <- group_mean_sd_up_lo_ci(osf.surprised:rom.sad)


#>
#>
#>
#> Crappy versions of a function to return means and sds -----


#> NOTE: the list version of this works but the list returns values in a crappy
#> format. 
#> It returns values all in one row. 
mean_sd <- function(summary_vars) { 
  data |> 
    summarize(across({{summary_vars}}, 
                      list(mean = mean,
                           sd = sd),
                      na.rm = TRUE)) |> 
    pivot_longer(cols = everything(), 
                 names_to = "Variable",
                 values_to = "Mean.Sd")
}

mean_sd(osf.surprised:rom.sad)

#> This one doesn't return any numbers for some reason. Still not in a useful
#> format either
mean_sd.2 <- function(summary_vars) { 
  data |> 
    summarize(across({{summary_vars}}, \(x) (mean(x, na.rm = TRUE))),
              across({{summary_vars}}, \(x) (sd(x, na.rm = TRUE))))
}

mean_sd.2(osf.surprised:osf.sad)

?across



#>
#>
#>
#>
#>

### Functions Generated by Chat GPT -------

mean_sd <- function(data, vars){
  result <- data |> 
    select(vars) |> 
    summarize(across(everything(), list(Mean = mean, SD = sd), na.rm = TRUE)) |> 
    pivot_longer(cols = everything(), names_to = c(".value", "Variable"), names_sep = "_") |> 
    arrange(Variable)
  
  return(result)
}


mean_sd(data, c("age.participant", "romantic.attraction.tofriend"))


mean_sd_grouped <- function(data, vars, group_var) {
  result <- data %>%
    group_by({{group_var}}) %>%
    summarize(across({{vars}}, list(Mean = mean, SD = sd), na.rm = TRUE)) %>%
    pivot_longer(cols = everything(), names_to = c(".value", "Variable"), names_sep = "_") %>%
    arrange({{group_var}}, Variable)
  
  return(result)
}

mean_sd_grouped(data, c("age.participant", "romantic.attraction.tofriend"), gender)



#>
#>
#>
#>
#> Chat GPT Function for Calculating Frequencies -----
#> This actually functions really well! 

freq_by_group <- function(data, var, group) {
  data %>%
    group_by({{group}}) %>%
    count({{var}}) %>%
    ungroup() %>%
    pivot_wider(names_from = {{var}}, values_from = n, values_fill = 0)
}

freq_by_group(data, relationship.status.participant, gender)

data |> count(gender) 
data |> count(relationship.status.participant)


#> Chat GPT Function for Counting and Calculating Percentages -----
#> This one also works pretty well!! 
#> 
count_percentage <- function(data, var) {
  data %>%
    group_by({{var}}) %>%
    summarize(n = n()) %>%
    mutate(percentage = n / sum(n) * 100)
}

data |> count_percentage(gender)



#> Another Chat GPT Function for count/percentage but with multiple variables-----
#> 
#>
count_perc <- function(df, vars) {
  df %>%
    select(all_of(vars)) %>%
    gather() %>%
    group_by(key, value) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    mutate(perc = count/sum(count))
}

var_freq <- count_perc(data, c("gender", 
                   "relationship.status.participant", 
                   "sexual.orientation.participant",
                   "osf.closer.ssf"
                   )) 

var_freq |> kbl() |> kable_stylying



