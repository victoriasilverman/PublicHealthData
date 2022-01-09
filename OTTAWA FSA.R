library(tidyr)
library(tidyverse)
library(formattable)
library(ggplot2)
library(dplyr)

FSAData<- read.csv("/Users/Victoria/Desktop/ICES-COVID19-Vaccination-Data-by-FSA.csv")
FSAData$X85.<-as.numeric(FSAData$X85.)
FSAData$X80.84yrs<-as.numeric(FSAData$X80.84yrs)
FSAData$X75.79yrs<-as.numeric(FSAData$X75.79yrs)
FSAData$X70.75yrs<-as.numeric(FSAData$X70.75yrs)



FSAData.t<-as_tibble(FSAData) %>% #convert it to a tibble
  filter(Public.Health.Unit=="2251 Ottawa Public Health") %>% #filter to most recent date
  mutate(.data=., Total.pop=Total.pop*100, X85. = X85.*100, X80.84yrs= X80.84yrs*100,X75.79yrs= X75.79yrs*100,X70.75yrs= X70.75yrs*100)%>% #create a column converting weight to lbs
  rename("85+" = X85., "80-84yrs"= X80.84yrs,"75-79yrs"= X75.79yrs,"70-74yrs"= X70.75yrs) #rename columns
  
  
  ggplot(FSAData.t) +
  aes(x = reorder(FSA,Total.pop), weight = Total.pop) +
  geom_bar(color="black", fill='#A6836F') + 
  labs(title="3rd dose total population", x="FSA",y= "Percentage with THREE doses", subtitle="by Victoria Silverman, 2021-12-05", caption= "Data from ICES")+
  theme_minimal()+  coord_flip()+
  theme(plot.subtitle=element_text(size=7, hjust=0.5, face="italic", color="black"))+
  theme(legend.position="none", plot.caption=element_text(size=7, hjust=0.5, color="black"))
  
