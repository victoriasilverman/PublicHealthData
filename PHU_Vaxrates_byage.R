
library(dplyr)
library(tidyverse)
library(formattable)

PHUdata<- read.csv(url("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/2a362139-b782-43b1-b3cb-078a2ef19524/download/vaccines_by_age_phu.csv"))


Today<-"2021-12-05"



PHU.data.t<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
  mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
  filter(Date==Today) %>% #filter to most recent date
  select(Agegroup,PHU.name,Percent_at_least_one_dose) %>% #select columns I want
  pivot_wider(names_from = Agegroup, values_from = Percent_at_least_one_dose) %>% #pivot it
  rename("5+" = Ontario_5plus) %>% #rename columns
  select(-c(Undisclosed_or_missing,Adults_18plus,Ontario_12plus)) %>% #remove the undisclosed column
  slice(-35)  %>% #remove the unkonwn PHU row
  relocate(PHU.name, after="5+")

 
  formattable(PHU.data.t, list(
    "05-11yrs" = color_tile("red", "blue"),
    "12-17yrs" = color_tile("red", "blue"),
    "18-29yrs" = color_tile("red", "blue"),
    "30-39yrs" = color_tile("red", "blue"),
    "40-49yrs" = color_tile("red", "blue"),
    "50-59yrs" = color_tile("red", "blue"),
    "70-79yrs" = color_tile("red", "blue"),   
    "60-69yrs" = color_tile("red", "blue"),
    "80+"  = color_tile("red", "blue"),
    "18+" = color_tile("red", "blue"),
    "12+"  = color_tile("red", "blue"),
    "5+" = color_tile("red", "blue"),
    Cosigned = formatter("span",
                         style = x ~ style(display = "block",
                                           "border-radius" = "4px",
                                           "padding-right" = "4px",
                                           color = sapply(x,color.picker),
                                           "background-color" = sapply(x,bg.picker)),
                         x ~ sprintf("%.2f (rank: %02d)", x, rank(-x)))
  ))
  




