
library(dplyr)
library(tidyr)
library(tidyverse)
library(formattable)

#PLACE IN TODAY'S DATE!!!
Today<-"2021-12-05"

PHUdata<- read.csv(url("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/2a362139-b782-43b1-b3cb-078a2ef19524/download/vaccines_by_age_phu.csv"))


PHU.data.t<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
  mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
  filter(Date==Today) %>% #filter to most recent date
  select(Agegroup,PHU.name,Percent_at_least_one_dose) %>% #select columns I want
  pivot_wider(names_from = Agegroup, values_from = Percent_at_least_one_dose) %>% #pivot it
  rename("5+" = Ontario_5plus, age5_11="05-11yrs") %>% #rename columns
  select(-c(Undisclosed_or_missing,Adults_18plus,Ontario_12plus)) %>% #remove the undisclosed column
  slice(-35)  %>% #remove the unkonwn PHU row
  relocate(PHU.name, after="5+")%>%
  arrange(-age5_11)%>%
  rename( "05-11yrs"= age5_11)  #rename columns

#output table if want to use other vis software EDIT THIS LINE
write.csv(PHU.data.t,"Path to export the DataFrame\\File Name.csv", row.names = FALSE)

 
#create colour coded table
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
    "5+" = color_tile("red", "blue"),
    Cosigned = formatter("span",
                         style = x ~ style(display = "block",
                                           "border-radius" = "4px",
                                           "padding-right" = "4px",
                                           color = sapply(x,color.picker),
                                           "background-color" = sapply(x,bg.picker)),
                         x ~ sprintf("%.2f (rank: %02d)", x, rank(-x)))
  ))
  

##want to plot it? Plotting for 5-11 ONLY. 

#create tibble needed
  PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
    mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
    filter(Date==Today & Agegroup=="05-11yrs") %>% #filter to most recent date
    select(-Date) %>%  #remove the undisclosed colu
    arrange(-Percent_at_least_one_dose)


#ggplot it UPPPP with some Tswift colours
ggplot(PHU.data.g) +
 aes(x = reorder(PHU.name,Percent_at_least_one_dose), weight = Percent_at_least_one_dose) +
 geom_bar(fill = "#A6836F") + geom_hline(yintercept= c(12.5,25),color='#400303',size=1)+
 coord_flip() +labs(title="At Least One Dose 5-11 by PHU", x="Public Health Unit",y= "Percentage with ONE dose", subtitle="by Victoria Silverman 2021", caption= "Data from OnGov, vertical lines at 12.5% and 25%")+
 theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black"))

    


