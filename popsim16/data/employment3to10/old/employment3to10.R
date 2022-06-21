# deal with employment

setwd("J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data/employment3to10")

library(readxl)
library(dplyr)
library(tidyverse)

emp_town <- read_excel("2016townindEMPwagesbytownallown.xlsx")

convers <- read_excel("three2ten_conversion.xlsx")


# filter emp_town to just sectors 
emp2 <- emp_town %>% 
  filter(indcode %in% convers$NAICS_Sectors)

unique(emp2$naicstitle)

# join indcode to emp
together <- left_join(emp2, convers, by= c("indcode" = "NAICS_Sectors"))

# per town, calc sum of avg of 3 super sector
count3 <- together %>% 
  group_by(areaname, `Super-Sector`) %>%
  summarise(Sum3 = sum(avgemp)) %>%
  ungroup()

# per town, calc sum of avg of 10 sectors
count10 <- together %>% 
  group_by(areaname, `Emp Agent`, `Super-Sector`) %>%
  summarise(Sum10 = sum(avgemp)) %>%
  ungroup()

summa <- convers[c("Emp Agent","Super-Sector")] %>% group_by(`Emp Agent`, `Super-Sector`) %>% summarise(`Super-Sector` = first(`Super-Sector`))

# join together
count10s <- left_join(count10, summa, by=c("Emp Agent","Super-Sector"))

count310 <- left_join(count10s, count3, by=c("areaname", "Super-Sector"))

count310$perc <- round(count310$Sum10/count310$Sum3,2)

write.csv(count310, "count310.csv")



