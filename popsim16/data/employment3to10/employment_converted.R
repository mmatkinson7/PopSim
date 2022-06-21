

setwd("J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data/employment3to10")

library(dplyr)
library(readr)
library(tidyverse)


count310 <- read_csv("count310.csv", col_types = cols(...1 = col_skip()))
Statewide_2016 <- read_csv("J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data/Statewide_2016_RTP_Updated4Proj.csv")

Statewide_2016 <- Statewide_2016 %>% filter(TAZ_Type=="Internal_Centroid")

count310 <- count310 %>%
  mutate(NAME = case_when(
    NAME == "ROWE,MA" ~ "ROWE/MONORE/CHARLEMONT,MA",
    NAME == "MONROE,MA" ~ "ROWE/MONORE/CHARLEMONT,MA",
    NAME == "CHARLEMONT,MA" ~ "ROWE/MONORE/CHARLEMONT,MA",
    NAME == "MIDDLEFIELD,MA" ~ "MIDDLEFIELD/WORTHINGTON,MA",
    NAME == "WORTHINGTON,MA" ~ "MIDDLEFIELD/WORTHINGTON,MA",
    NAME == "SHARON,NH" ~ "SHARON/TEMPLE,NH",
    NAME == "TEMPLE,NH" ~ "SHARON/TEMPLE,NH",
    NAME == "EGREMONT,MA" ~ "EGREMONT/MOUNT WASHINGTON,MA",
    NAME == "MOUNT WASHINGTON,MA" ~ "EGREMONT/MOUNT WASHINGTON,MA",
    TRUE ~ NAME
  ))

expand <- left_join(Statewide_2016, count310, by=c("Town" = "NAME"))

expand<- expand %>% 
  mutate(Emp_Agent = case_when(`Super-Sector` == "BASIC" ~ perc * Bas_Emp,
                                `Super-Sector` == "RETAIL"~ perc * Ret_Emp,
                                `Super-Sector` == "SERVICE"~ perc * Srv_Emp))


summed <- expand %>% group_by(ID, `Emp Agent`) %>%
  summarise(sum10 = sum(Emp_Agent)) %>%
  ungroup()


summed <- summed %>%
  pivot_wider(names_from = `Emp Agent`, values_from = sum10) %>%
  select(-`NA`)

colnames(summed) <- c("ID", "Emp1", "Emp2","Emp3","Emp4","Emp5","Emp6","Emp7","Emp8","Emp9","Emp10")

S16 <- left_join(Statewide_2016, summed, by="ID")

S16$Emp1 <- round(S16$Emp1, 0)
S16$Emp2 <- round(S16$Emp2, 0)
S16$Emp3 <- round(S16$Emp3, 0)
S16$Emp4 <- round(S16$Emp4, 0)
S16$Emp5 <- round(S16$Emp5, 0)
S16$Emp6 <- round(S16$Emp6, 0)
S16$Emp7 <- round(S16$Emp7, 0)
S16$Emp8 <- round(S16$Emp8, 0)
S16$Emp9 <- round(S16$Emp9, 0)
S16$Emp10 <- round(S16$Emp10, 0)


write_csv(S16,"J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data/Statewide_2016_RTP_Updated4Proj_10Emp.csv")







