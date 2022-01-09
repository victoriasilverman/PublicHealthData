
library(tidyr)
library(tidyverse)
library(formattable)
library(ggplot2)
library(dplyr)

PHUdata<- read.csv(url("https://data.ontario.ca/dataset/752ce2b7-c15a-4965-a3dc-397bf405e7cc/resource/2a362139-b782-43b1-b3cb-078a2ef19524/download/vaccines_by_age_phu.csv"))


Today<-"2022-01-08"


# table of all ages at least 1 dose coloured
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


PHU.data.t<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Percent_fully_vaccinated, Agegroup) %>% #select columns I want
  mutate(.data=., "Percent_fully_vaccinated" = Percent_fully_vaccinated*100) %>% #create a column converting weight to lbs
  filter(Date==Today) %>% #filter to most recent date
  select(Agegroup,PHU.name,Percent_fully_vaccinated) %>% #select columns I want
  pivot_wider(names_from = Agegroup, values_from = Percent_fully_vaccinated) %>% #pivot it
  rename("5+" = Ontario_5plus, age5_11="05-11yrs") %>% #rename columns
  select(PHU.name,Adults_18plus) %>% #remove the undisclosed column
  slice(-35)  %>% #remove the unkonwn PHU row
  relocate(PHU.name, after="5+")%>%
  arrange(-age5_11)%>%
  rename( "05-11yrs"= age5_11)  #rename columns
  

 # table of all ages at least 1 dose coloured
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
  
  
  Yesterday<-"2021-12-08"
  
  PHU.data.t<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
    mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
    filter(Date==Today|Date==Yesterday) %>% #filter to most recent date
    select(Date, Agegroup,PHU.name,Percent_at_least_one_dose) %>% #select columns I want
     pivot_wider(names_from = Date, values_from = Percent_at_least_one_dose) %>%
    rename(Date1 = "2021-12-08", Date2="2021-12-07") %>% #rename columns
    mutate(.data=., "Difference" = Date1-Date2) %>% #create a column converting weight to lbs
    select(Difference, Agegroup,PHU.name) %>% #select columns I want
    pivot_wider(names_from = Agegroup, values_from = Difference) %>%
    rename("5+" = Ontario_5plus, age5_11="05-11yrs") %>% #rename columns
    select(-c(Undisclosed_or_missing,Adults_18plus,Ontario_12plus)) %>% #remove the undisclosed column
    slice(-35)  %>% #remove the unkonwn PHU row
    relocate(PHU.name, after="5+")%>%
    arrange(-age5_11)%>%
    rename( "05-11yrs"= age5_11)  #rename columns
  
  
  formattable(PHU.data.t, list(
    "05-11yrs" = color_tile("white", "green"),
    "12-17yrs" = color_tile("white", "green"),
    "18-29yrs" = color_tile("white", "green"),
    "30-39yrs" = color_tile("white", "green"),
    "40-49yrs" = color_tile("white", "green"),
    "50-59yrs" = color_tile("white", "green"),
    "70-79yrs" = color_tile("white", "green"),   
    "60-69yrs" = color_tile("white", "green"),
    "80+"  = color_tile("white", "green"),
    "5+" = color_tile("white", "green"),
    Cosigned = formatter("span",
                         style = x ~ style(display = "block",
                                           "border-radius" = "4px",
                                           "padding-right" = "4px",
                                           color = sapply(x,color.picker),
                                           "background-color" = sapply(x,bg.picker)),
                         x ~ sprintf("%.2f (rank: %02d)", x, rank(-x)))
  ))
  
  
  
  #nominal
  PHU.data.n<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date,PHU.name, Agegroup,At.least.one.dose_cumulative) %>% #select columns I want
    filter(Date==Today|Date==Yesterday) %>% #filter to most recent date
    select(Date, Agegroup,PHU.name,At.least.one.dose_cumulative) %>% #select columns I want
    pivot_wider(names_from = Date, values_from = At.least.one.dose_cumulative) %>%
    rename(Date1 = "2021-12-08", Date2="2021-12-07") %>% #rename columns
    mutate(.data=., "Difference" = Date1-Date2) %>% #create a column converting weight to lbs
    select(Difference, Agegroup,PHU.name) %>% #select columns I want
    pivot_wider(names_from = Agegroup, values_from = Difference) %>%
    rename("5+" = Ontario_5plus, age5_11="05-11yrs") %>% #rename columns
    select(-c(Undisclosed_or_missing,Adults_18plus,Ontario_12plus)) %>% #remove the undisclosed column
    slice(-35)  %>% #remove the unkonwn PHU row
    relocate(PHU.name, after="5+")%>%
    arrange(-age5_11)%>%
    rename( "05-11yrs"= age5_11)
  
  formattable(PHU.data.n, list(
    "05-11yrs" = color_tile("white", "blue"),
    "12-17yrs" = color_tile("white", "blue"),
    "18-29yrs" = color_tile("white", "blue"),
    "30-39yrs" = color_tile("white", "blue"),
    "40-49yrs" = color_tile("white", "blue"),
    "50-59yrs" = color_tile("white", "blue"),
    "70-79yrs" = color_tile("white", "blue"),   
    "60-69yrs" = color_tile("white", "blue"),
    "80+"  = color_tile("white", "blue"),
    "5+" = color_tile("white", "blue"),
    Cosigned = formatter("span",
                         style = x ~ style(display = "block",
                                           "border-radius" = "4px",
                                           "padding-right" = "4px",
                                           color = sapply(x,color.picker),
                                           "background-color" = sapply(x,bg.picker)),
                         x ~ sprintf("%.2f (rank: %02d)", x, rank(-x)))
  ))
  
  
  
  
  # all PHU 5+ 
  PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
    mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
    filter(Date==Today & Agegroup=="05-11yrs") %>% #filter to most recent date
    select(-Date) %>%  #remove the undisclosed colu
    arrange(-Percent_at_least_one_dose) %>%
    mutate( class = case_when(  #create the groups based on the bins outlined
      Percent_at_least_one_dose < 12.5 ~ "Group 1", 
      (Percent_at_least_one_dose >= 12.5 & Percent_at_least_one_dose < 25) ~ "Group 2", 
      (Percent_at_least_one_dose >= 25 & Percent_at_least_one_dose < 33) ~ "Group 3", 
      Percent_at_least_one_dose >= 33 ~ "Group 4")) 

  ON.data<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date, Agegroup,Total.population, At.least.one.dose_cumulative) %>% #select columns I want
    filter(Date==Today & Agegroup=="05-11yrs") %>% #filter to most recent date
    select(Total.population, At.least.one.dose_cumulative) %>% #select columns I want
    summarise_all(funs(sum)) %>%
    mutate(.data=., "Percent_at_least_one_dose" = (At.least.one.dose_cumulative/Total.population)*100, "PHU.name" = "ONTARIO", "Agegroup"="05-11yrs", "Region"= "ONTARIO" ) %>%#create a column converting weight to lbs
    select(Percent_at_least_one_dose, PHU.name,Region, Agegroup) #select columns I want
    
  ON.data<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date, Agegroup,Total.population, fully_vaccinated_cumulative) %>% #select columns I want
    filter(Date==Today & Agegroup=="Adults_18plus") %>% #filter to most recent date
    select(Total.population, fully_vaccinated_cumulative) %>% #select columns I want
    summarise_all(funs(sum)) %>%
    mutate(.data=., "Percent_double" = (fully_vaccinated_cumulative/Total.population)*100, "PHU.name" = "ONTARIO", "Agegroup"="18+", "Region"= "ONTARIO" ) %>%#create a column converting weight to lbs
    select(Percent_double, PHU.name,Region, Agegroup) #select columns I want
  
  
  
  
  
  
  
  
  PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
    mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
    filter(Date==Today & Agegroup=="Adults_18plus") %>% #filter to most recent date
    select(-Date) %>%  #remove the undisclosed colu
    arrange(-Percent_at_least_one_dose) %>%
    mutate( Region = case_when(  #create the groups based on the bins outlined
      PHU.name=="CITY OF OTTAWA"| 
      PHU.name=="KINGSTON, FRONTENAC, LENNOX & ADDINGTON"| 
      PHU.name=="HASTINGS & PRINCE EDWARD COUNTIES"|
      PHU.name=="RENFREW COUNTY AND DISTRICT" |
      PHU.name=="LEEDS, GRENVILLE AND LANARK DISTRICT"|
      PHU.name=="EASTERN ONTARIO"~ "EASTERN", 
      (  PHU.name=="ALGOMA DISTRICT"| 
           PHU.name=="NORTH BAY PARRY SOUND DISTRICT"| 
           PHU.name=="PORCUPINE"|
           PHU.name=="THUNDER BAY DISTRICT" |
           PHU.name=="NORTHWESTERN" |
           PHU.name=="SUDBURY AND DISTRICT"| 
           PHU.name=="TIMISKAMING" ) ~"NORTHERN", 
      ( PHU.name=="BRANT COUNTY"| 
          PHU.name=="CHATHAM-KENT"| 
          PHU.name=="WINDSOR-ESSEX COUNTY"|
          PHU.name=="GREY BRUCE" |
          PHU.name=="HALDIMAND-NORFOLK"|
          PHU.name=="HURON PERTH"| 
          PHU.name=="LAMBTON COUNTY"|
          PHU.name=="MIDDLESEX-LONDON"|
          PHU.name=="WATERLOO REGION"|
          PHU.name=="SOUTHWESTERN"|
          PHU.name=="WELLINGTON-DUFFERIN-GUELPH") ~ "WESTERN", 
      PHU.name=="TORONTO"| 
        PHU.name=="DURHAM REGION"| 
        PHU.name=="YORK REGION"|
        PHU.name=="HALIBURTON, KAWARTHA, PINE RIDGE" |
        PHU.name=="PEEL REGION"|
        PHU.name=="SIMCOE MUSKOKA DISTRICT"| 
        PHU.name=="CITY OF HAMILTON"|
        PHU.name=="PETERBOROUGH COUNTY-CITY"|
        PHU.name=="NIAGARA REGION"|
        PHU.name=="HALTON REGION"~ "CENTRAL")) %>% bind_rows(ON.data)

  
  
  PHU.data.g2<-as_tibble(PHUdata) %>% #convert it to a tibble
    select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
    mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
    filter(Date==Today & Agegroup=="05-11yrs") %>% #filter to most recent date
    select(-Date) %>%  #remove the undisclosed colu
    arrange(-Percent_at_least_one_dose) %>%
    mutate( Region = case_when(  #create the groups based on the bins outlined
      PHU.name=="CITY OF OTTAWA"| 
        PHU.name=="KINGSTON, FRONTENAC, LENNOX & ADDINGTON"| 
        PHU.name=="HASTINGS & PRINCE EDWARD COUNTIES"|
        PHU.name=="RENFREW COUNTY AND DISTRICT" |
        PHU.name=="LEEDS, GRENVILLE AND LANARK DISTRICT"|
        PHU.name=="EASTERN ONTARIO"|
        PHU.name=="ALGOMA DISTRICT"| 
           PHU.name=="NORTH BAY PARRY SOUND DISTRICT"| 
           PHU.name=="PORCUPINE"|
           PHU.name=="THUNDER BAY DISTRICT" |
           PHU.name=="NORTHWESTERN" |
           PHU.name=="SUDBURY AND DISTRICT"| 
           PHU.name=="TIMISKAMING" |
          PHU.name=="BRANT COUNTY"| 
          PHU.name=="CHATHAM-KENT"| 
          PHU.name=="WINDSOR-ESSEX COUNTY"|
          PHU.name=="GREY BRUCE" |
          PHU.name=="HALDIMAND-NORFOLK"|
          PHU.name=="HURON PERTH"| 
          PHU.name=="LAMBTON COUNTY"|
          PHU.name=="MIDDLESEX-LONDON"|
          PHU.name=="WATERLOO REGION"|
          PHU.name=="SOUTHWESTERN"|
          PHU.name=="WELLINGTON-DUFFERIN-GUELPH"|
      PHU.name=="TORONTO"| 
        PHU.name=="DURHAM REGION"| 
        PHU.name=="YORK REGION"|
        PHU.name=="HALIBURTON, KAWARTHA, PINE RIDGE" |
        PHU.name=="PEEL REGION"|
        PHU.name=="SIMCOE MUSKOKA DISTRICT"| 
        PHU.name=="CITY OF HAMILTON"|
        PHU.name=="PETERBOROUGH COUNTY-CITY"|
        PHU.name=="NIAGARA REGION"|
        PHU.name=="HALTON REGION"~ "PHU")) %>%
    bind_rows(ON.data)
  
   
library(RColorBrewer)  
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(PHU.data.g$class)
colScale <- scale_colour_manual(name = "class",values = myColors)
  
ggplot(PHU.data.g) +
 aes(x = reorder(PHU.name,Percent_at_least_one_dose), weight = Percent_at_least_one_dose) +
 geom_bar( color="black") + scale_fill_taylor(palette = "folklore", guide = "none")+ geom_hline(yintercept= c(12.5,25,33),color='#b8396b',size=1)+
 coord_flip() +labs(title="At Least One Dose 5-11 by PHU", x="Public Health Unit",y= "Percentage with ONE dose", subtitle="by Victoria Silverman, 2021-12-07", caption= "Data from OnGov, vertical lines at 12.5%, 25% and 33%")+
 theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black")) 


ggplot(PHU.data.g) +
  aes(x = reorder(PHU.name,Percent_at_least_one_dose), weight = Percent_at_least_one_dose, fill=Region) +
  geom_bar( color="black") + scale_fill_taylor(palette = "lover", guide = "none") + 
  coord_flip() +labs(title="At Least One Dose 5-11 by PHU", x="Public Health Unit",y= "Percentage with ONE dose", subtitle="by Victoria Silverman, 2021-12-12", caption= "Data from OnGov, vertical line at 33%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black")) +  geom_hline(yintercept= c(33),color='#871d20',size=1)


ggplot(PHU.data.g2) +
  aes(x = reorder(PHU.name,Percent_at_least_one_dose), weight = Percent_at_least_one_dose, fill=Region) +
  geom_bar( color="black") + scale_fill_taylor(palette = "folklore", guide = "none")  + 
  coord_flip() +labs(title="At Least One Dose 5-11 by PHU", x="Public Health Unit",y= "Percentage with ONE dose", subtitle="by Victoria Silverman, 2021-12-12", caption= "Data from OnGov, vertical lines at 33%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black")) +  geom_hline(yintercept= c(33),color='#871d20',size=1)+theme(legend.position = "none")

scale_fill_manual(values=c("#76bae0", "#ffd1d7"))


ggplot(PHU.data.g) +
  aes(x = reorder(PHU.name,Percent_at_least_one_dose), weight = Percent_at_least_one_dose) +
  geom_bar(position = position_dodge(width = 5), color="black") + geom_col(aes(y=Percent_at_least_one_dose,fill=Region))+  geom_text(aes(y= Percent_at_least_one_dose, label = Percent_at_least_one_dose), hjust = 0.7, nudge_x = .05)+
  scale_fill_taylor(palette = "lover", guide = "none")+  
  coord_flip() +labs(title="At Least One Dose 5-11 by PHU", x="Public Health Unit",y= "Percentage with ONE dose", subtitle="by Victoria Silverman, 2021-12-07", caption= "Data from OnGov, vertical lines at 12.5%, 25% and 33%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black")) 


geom_col(fill = "gray70") +
  geom_text(
    aes(label = perc), 
    ## make labels left-aligned
    hjust = 1, nudge_x = -.5
  ) +




ggplot(PHU.data.g) +
  aes(x = reorder(PHU.name,Percent_at_least_one_dose), weight = Percent_at_least_one_dose) +
  geom_bar(fill = "#A6836F") + geom_hline(yintercept= c(12.5,25),color='#400303',size=1)+
  coord_flip() +labs(title="At Least One Dose 5-11 by PHU", x="Public Health Unit",y= "Percentage with ONE dose", subtitle="by Victoria Silverman 2021", caption= "Data from OnGov, vertical lines at 12.5% and 25%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black"))


# overtime
PHU.data.g2<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
  mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
  filter( Agegroup=="05-11yrs") %>% #filter to most recent date
  mutate(.data=., Date = as.Date(Date))
    
ggplot(PHU.data.g2, aes(x=Date, y=Percent_at_least_one_dose, group=PHU.name, colour=PHU.name)) +
  geom_line() +theme(legend.position="bottom") +  scale_fill_taylor(palette = "", guide="none")
 


#### 5-11 versus 12+. 

# all PHU 5+ 
PHU.data.g3<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,Percent_at_least_one_dose) %>% #select columns I want
  mutate(.data=., "Percent_at_least_one_dose" = Percent_at_least_one_dose*100) %>% #create a column converting weight to lbs
  filter(Date==Today & (Agegroup=="05-11yrs"| Agegroup=="Ontario_12plus")) %>% #filter to most recent date
  select(-Date) %>%
  pivot_wider(names_from = Agegroup, values_from = Percent_at_least_one_dose) %>%
  slice(-35)%>%
  rename(Age_5_11 = '05-11yrs')%>%
  mutate(PHU.name2 = case_when(
    Ontario_12plus>=90 ~ as.character(PHU.name),
    Ontario_12plus<90 ~ ""))  #rename columns

  
library(ggrepel)
ggplot(data=PHU.data.g3, aes(x= Age_5_11, y=Ontario_12plus))+geom_point() + geom_hline(yintercept=90, color="red")+labs(title="Age 5-11 versus Age 12+") + theme(legend.position = "none") +geom_text_repel(label=ifelse(PHU.data.g3$Ontario_12plus>=90,as.character(PHU.data.g3$PHU.name),''),size = 2) 

ggplot(data=PHU.data.g3, aes(x= Age_5_11, y=Ontario_12plus))+geom_point() + geom_hline(yintercept=90, color="red")+labs(title="Age 5-11 versus Age 12+") + theme(legend.position = "none") +geom_text_repel(label=ifelse(as.character(PHU.data.g3$PHU.name)),size = 2) 


ggplot(data=PHU.data.g3, aes(x= Age_5_11, y=Ontario_12plus, label=PHU.name))+geom_point() + geom_hline(yintercept=90, color="red")+labs(title="Age 5-11 versus Age 12+") + theme(legend.position = "none") +geom_text_repel(size = 2, box.padding = 0.5, fill='white', xlim = c(-Inf, Inf), ylim = c(-Inf, Inf)) 




ggplot(PHU.data.g3, aes(x= Age_5_11, y= Ontario_12plus, label=PHU.name))+
  geom_point() +
  geom_text(aes(label=as.character(PHU.name)),hjust=0.5,vjust=-1,size=2)
  




ON.data<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date, Agegroup,Total.population, third_dose_cumulative) %>% #select columns I want
  filter(Date==Today & (Agegroup=="50-59yrs"|Agegroup=="60-69yrs"|Agegroup=="70-79yrs"|Agegroup=="80+")) %>% #filter to most recent date
  select(Total.population, third_dose_cumulative) %>% #select columns I want
  summarise_all(funs(sum)) %>%
  mutate(.data=., "Percent_3doses" = (third_dose_cumulative/Total.population)*100, "PHU.name" = "ONTARIO", "Agegroup"="50+", "Region"= "ONTARIO" ) %>%#create a column converting weight to lbs
  select(Percent_3doses, PHU.name,Region, Agegroup) #select columns I want

ON.data<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date, Agegroup,Total.population, third_dose_cumulative) %>% #select columns I want
  filter(Date==Today & Agegroup=="Adults_18plus") %>% #filter to most recent date
  select(Total.population, third_dose_cumulative) %>% #select columns I want
  summarise_all(funs(sum)) %>%
  mutate(.data=., "Percent_3doses" = (third_dose_cumulative/Total.population)*100, "PHU.name" = "ONTARIO", "Agegroup"="18+", "Region"= "ONTARIO" ) %>%#create a column converting weight to lbs
  select(Percent_3doses, PHU.name,Region, Agegroup) #select columns I want




PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,third_dose_cumulative,Total.population ) %>% #select columns I want
  filter(Date==Today & PHU.name!="UNKNOWN" & (Agegroup=="Adults_18plus")) %>% #filter to most recent date
  select(-Date) %>% 
  #slice(-35)%>%#remove the undisclosed colu
  mutate( Region = case_when(  #create the groups based on the bins outlined
    PHU.name=="CITY OF OTTAWA"| 
      PHU.name=="KINGSTON, FRONTENAC, LENNOX & ADDINGTON"| 
      PHU.name=="HASTINGS & PRINCE EDWARD COUNTIES"|
      PHU.name=="RENFREW COUNTY AND DISTRICT" |
      PHU.name=="LEEDS, GRENVILLE AND LANARK DISTRICT"|
      PHU.name=="EASTERN ONTARIO"~ "EASTERN", 
    (  PHU.name=="ALGOMA DISTRICT"| 
         PHU.name=="NORTH BAY PARRY SOUND DISTRICT"| 
         PHU.name=="PORCUPINE"|
         PHU.name=="THUNDER BAY DISTRICT" |
         PHU.name=="NORTHWESTERN" |
         PHU.name=="SUDBURY AND DISTRICT"| 
         PHU.name=="TIMISKAMING" ) ~"NORTHERN", 
    ( PHU.name=="BRANT COUNTY"| 
        PHU.name=="CHATHAM-KENT"| 
        PHU.name=="WINDSOR-ESSEX COUNTY"|
        PHU.name=="GREY BRUCE" |
        PHU.name=="HALDIMAND-NORFOLK"|
        PHU.name=="HURON PERTH"| 
        PHU.name=="LAMBTON COUNTY"|
        PHU.name=="MIDDLESEX-LONDON"|
        PHU.name=="WATERLOO REGION"|
        PHU.name=="SOUTHWESTERN"|
        PHU.name=="WELLINGTON-DUFFERIN-GUELPH") ~ "WESTERN", 
    PHU.name=="TORONTO"| 
      PHU.name=="DURHAM REGION"| 
      PHU.name=="YORK REGION"|
      PHU.name=="HALIBURTON, KAWARTHA, PINE RIDGE" |
      PHU.name=="PEEL REGION"|
      PHU.name=="SIMCOE MUSKOKA DISTRICT"| 
      PHU.name=="CITY OF HAMILTON"|
      PHU.name=="PETERBOROUGH COUNTY-CITY"|
      PHU.name=="NIAGARA REGION"|
      PHU.name=="HALTON REGION"~ "CENTRAL")) %>% 
  select(-PHU.name) %>% 
  group_by(Region) %>% 
  summarise(third_dose_cumulative = (sum(third_dose_cumulative)),Total.population = (sum(Total.population)))%>%
  mutate(.data=., "Percent_3doses" = (third_dose_cumulative/Total.population)*100 ) %>%#create a column converting weight to lbs
  bind_rows(ON.data)%>%
  arrange(-Percent_3doses) 


PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,third_dose_cumulative,Total.population ) %>% #select columns I want
  filter(Date==Today & PHU.name!="UNKNOWN" & (Agegroup=="Adults_18plus")) %>% #filter to most recent date
  select(-Date) %>% 
  #slice(-35)%>%#remove the undisclosed colu
  mutate( Region = case_when(  #create the groups based on the bins outlined
    PHU.name=="CITY OF OTTAWA"| 
      PHU.name=="KINGSTON, FRONTENAC, LENNOX & ADDINGTON"| 
      PHU.name=="HASTINGS & PRINCE EDWARD COUNTIES"|
      PHU.name=="RENFREW COUNTY AND DISTRICT" |
      PHU.name=="LEEDS, GRENVILLE AND LANARK DISTRICT"|
      PHU.name=="EASTERN ONTARIO"~ "EASTERN", 
    (  PHU.name=="ALGOMA DISTRICT"| 
         PHU.name=="NORTH BAY PARRY SOUND DISTRICT"| 
         PHU.name=="PORCUPINE"|
         PHU.name=="THUNDER BAY DISTRICT" |
         PHU.name=="NORTHWESTERN" |
         PHU.name=="SUDBURY AND DISTRICT"| 
         PHU.name=="TIMISKAMING" ) ~"NORTHERN", 
    ( PHU.name=="BRANT COUNTY"| 
        PHU.name=="CHATHAM-KENT"| 
        PHU.name=="WINDSOR-ESSEX COUNTY"|
        PHU.name=="GREY BRUCE" |
        PHU.name=="HALDIMAND-NORFOLK"|
        PHU.name=="HURON PERTH"| 
        PHU.name=="LAMBTON COUNTY"|
        PHU.name=="MIDDLESEX-LONDON"|
        PHU.name=="WATERLOO REGION"|
        PHU.name=="SOUTHWESTERN"|
        PHU.name=="WELLINGTON-DUFFERIN-GUELPH") ~ "WESTERN", 
    PHU.name=="TORONTO"| 
      PHU.name=="DURHAM REGION"| 
      PHU.name=="YORK REGION"|
      PHU.name=="HALIBURTON, KAWARTHA, PINE RIDGE" |
      PHU.name=="PEEL REGION"|
      PHU.name=="SIMCOE MUSKOKA DISTRICT"| 
      PHU.name=="CITY OF HAMILTON"|
      PHU.name=="PETERBOROUGH COUNTY-CITY"|
      PHU.name=="NIAGARA REGION"|
      PHU.name=="HALTON REGION"~ "CENTRAL")) %>% 
  mutate(.data=., "Percent_3doses" = (third_dose_cumulative/Total.population)*100 ) %>%#create a column converting weight to lbs
  bind_rows(ON.data)%>%
  arrange(-Percent_3doses)%>%
  filter(Region=="CENTRAL"|Region=="ONTARIO")


PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,third_dose_cumulative,Total.population ) %>% #select columns I want
  filter(Date==Today & PHU.name=="CITY OF OTTAWA" ) %>% #filter to most recent date
  mutate(.data=., "Percent_3dose" = (third_dose_cumulative/Total.population)*100) %>% #create a column converting weight to lbs
  slice(-13)

  ggplot(PHU.data.g) +
  aes(x = reorder(PHU.name,Percent_3doses), weight = Percent_3doses,fill=Region) +
    geom_bar( color="black") +  scale_fill_manual(name = "PHU.name", values=c( "#474247","#e89264"))  + coord_flip() +labs(title="3rd dose 18+ %", x="PHU",y= "Percentage with THREE doses", subtitle="by Victoria Silverman, 2022-01-07", caption= "Data from OnGov, vertical line at 33%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(legend.position="none", plot.caption=element_text(size=7, hjust=0.5, color="black")) +  geom_hline(yintercept= c(33),color='#871d20',size=1)





ggplot(PHU.data.g) +
  aes(x = reorder(Region,Percent_3doses), weight = Percent_3doses, fill=Region) +
  geom_bar( color="black") + scale_fill_taylor(palette = "evermore", guide = "none") + 
  coord_flip() +labs(title="3rd dose % 18+", x="Public Health Unit",y= "Percentage with THREE doses", subtitle="by Victoria Silverman, 2022-01-08", caption= "Data from OnGov, vertical line at 33%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(plot.caption=element_text(size=7, hjust=0.5, color="black")) +  geom_hline(yintercept= c(33),color='#871d20',size=1)




PHU.data.g<-as_tibble(PHUdata) %>% #convert it to a tibble
  select(Date,PHU.name, Agegroup,third_dose_cumulative,Total.population ) %>% #select columns I want
  filter(Date==Today & PHU.name!="UNKNOWN" & (Agegroup=="50-59yrs"|Agegroup=="60-69yrs"|Agegroup=="70-79yrs"|Agegroup=="80+")) %>% #filter to most recent date
  select(-Date) %>% 
  group_by(PHU.name) %>% 
  summarise(third_dose_cumulative = (sum(third_dose_cumulative)),Total.population = (sum(Total.population)))%>%
  mutate(.data=., "Percent_3doses" = (third_dose_cumulative/Total.population)*100 ) %>%#create a column converting weight to lbs
  #slice(-35)%>%#remove the undisclosed colu
  mutate( Region = case_when(  #create the groups based on the bins outlined
    PHU.name=="CITY OF OTTAWA"| 
      PHU.name=="KINGSTON, FRONTENAC, LENNOX & ADDINGTON"| 
      PHU.name=="HASTINGS & PRINCE EDWARD COUNTIES"|
      PHU.name=="RENFREW COUNTY AND DISTRICT" |
      PHU.name=="LEEDS, GRENVILLE AND LANARK DISTRICT"|
      PHU.name=="EASTERN ONTARIO"~ "EASTERN", 
    (  PHU.name=="ALGOMA DISTRICT"| 
         PHU.name=="NORTH BAY PARRY SOUND DISTRICT"| 
         PHU.name=="PORCUPINE"|
         PHU.name=="THUNDER BAY DISTRICT" |
         PHU.name=="NORTHWESTERN" |
         PHU.name=="SUDBURY AND DISTRICT"| 
         PHU.name=="TIMISKAMING" ) ~"NORTHERN", 
    ( PHU.name=="BRANT COUNTY"| 
        PHU.name=="CHATHAM-KENT"| 
        PHU.name=="WINDSOR-ESSEX COUNTY"|
        PHU.name=="GREY BRUCE" |
        PHU.name=="HALDIMAND-NORFOLK"|
        PHU.name=="HURON PERTH"| 
        PHU.name=="LAMBTON COUNTY"|
        PHU.name=="MIDDLESEX-LONDON"|
        PHU.name=="WATERLOO REGION"|
        PHU.name=="SOUTHWESTERN"|
        PHU.name=="WELLINGTON-DUFFERIN-GUELPH") ~ "WESTERN", 
    PHU.name=="TORONTO"| 
      PHU.name=="DURHAM REGION"| 
      PHU.name=="YORK REGION"|
      PHU.name=="HALIBURTON, KAWARTHA, PINE RIDGE" |
      PHU.name=="PEEL REGION"|
      PHU.name=="SIMCOE MUSKOKA DISTRICT"| 
      PHU.name=="CITY OF HAMILTON"|
      PHU.name=="PETERBOROUGH COUNTY-CITY"|
      PHU.name=="NIAGARA REGION"|
      PHU.name=="HALTON REGION"~ "CENTRAL")) %>% 
  bind_rows(ON.data)%>%
  arrange(-Percent_3doses) 





ggplot(PHU.data.g) +
  aes(x = reorder(PHU.name,Percent_3doses), weight = Percent_3doses, fill=Region) +
  geom_bar( color="black") +  scale_fill_manual(name = "PHU.name", values=c("#efefef","#827d73", "#3d2620", "#e89264","#474247"))  + 
  coord_flip() +labs(title="3rd dose % 50+", x="Public Health Unit",y= "Percentage with THREE doses", subtitle="by Victoria Silverman, 2022-01-07", caption= "Data from OnGov, vertical line at 33%")+
  theme_minimal()+  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+theme(legend.position="none", plot.caption=element_text(size=7, hjust=0.5, color="black")) +  geom_hline(yintercept= c(33),color='#871d20',size=1)









  

#Rt estimation

OttawaCase<- read.csv("/Users/Victoria/Documents/Victoria/Fun Stuff/COVID-19_Cases_and_Deaths_in_Ottawa_20211208.csv")
Ottawa.case.t<-as_tibble(PHUCase) %>% #convert it to a tibble
  select(Date, Daily_Cases_by_Reported_Date)%>%
  mutate_if(is.integer, ~replace(., is.na(.), 0)) %>%   
  rename(I = Daily_Cases_by_Reported_Date)  #rename columns

location <-rep("local", length=(nrow(Ottawa.case.t)))

incid <- incidence(Ottawa.case.t$I, groups = location)


res_parametric_si <- estimate_R(incid, 
                                method="parametric_si",
                                config = make_config(list(
                                  mean_si = 2.9, 
                                  std_si = 1.5)))


