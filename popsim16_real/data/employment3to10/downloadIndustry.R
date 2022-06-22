
library(tidycensus)
library(tidyverse)
library(readxl)
library(stringr)

setwd("J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data/employment3to10")
convers <- read_excel("three2ten_conversion.xlsx")

#download

S2403 <- get_acs(
  variables = c("S2403_C01_001","S2403_C01_003","S2403_C01_004","S2403_C01_005","S2403_C01_006","S2403_C01_007",
                "S2403_C01_008","S2403_C01_010","S2403_C01_011","S2403_C01_012","S2403_C01_014",
                "S2403_C01_015","S2403_C01_017","S2403_C01_018","S2403_C01_019","S2403_C01_021",
                "S2403_C01_022","S2403_C01_024","S2403_C01_025","S2403_C01_026","S2403_C01_027"),
  geography = "County Subdivision",
  state = c("MA","RI","NH"),
  survey = "acs5",
  year = 2016
)

S2403 <- S2403 %>% select(-moe) %>% 
  filter(!grepl("County subdivisions not defined", NAME)) 


S2403 <- S2403 %>% mutate(NAME = str_replace_all(NAME, c("Massachusetts" = "MA", "Rhode Island" = "RI", "New Hampshire" = "NH")))

S2403 <- S2403 %>% mutate(NAME = str_replace_all(NAME, c(", (.*) County" = "")))

S2403 <- S2403 %>% mutate(NAME = str_replace_all(NAME, c(" town" = "", " city" = "", " Town" = "")))

S2403$NAME <- toupper(S2403$NAME)
S2403 <- S2403 %>% mutate(NAME = str_replace_all(NAME, c(", " = ",")))

S2403 <- S2403 %>% mutate(variable = str_replace_all(variable, c(
  "S2403_C01_001"="Civilian employed population 16 years and over",
  "S2403_C01_003"="Agriculture, forestry, fishing and hunting",
  "S2403_C01_004"="Mining, quarrying, and oil and gas extraction",
  "S2403_C01_005"="Construction",
  "S2403_C01_006"="Manufacturing",
  "S2403_C01_007"="Wholesale trade",
  "S2403_C01_008"="Retail trade",
  "S2403_C01_010"="Transportation and warehousing",
  "S2403_C01_011"="Utilities",
  "S2403_C01_012"="Information",
  "S2403_C01_014"="Finance and insurance",
  "S2403_C01_015"="Real estate and rental and leasing",
  "S2403_C01_017"="Professional, scientific, and technical services",
  "S2403_C01_018"="Management of companies and enterprises",
  "S2403_C01_019"="ADMINISTRATIVE AND SUPPORT AND WASTE MANAGEMENT AND REMEDIATION SERVICES",
  "S2403_C01_021"="Educational services",
  "S2403_C01_022"="Health care and social assistance",
  "S2403_C01_024"="Arts, entertainment, and recreation",
  "S2403_C01_025"="Accommodation and food services",
  "S2403_C01_026"="OTHER SERVICES (EXCEPT PUBLIC ADMINISTRATION)",
  "S2403_C01_027"="Government"

)))


S2403$variable = toupper(S2403$variable)
convers$Description = toupper(convers$Description)

S2403 <- left_join(S2403, convers, by=c("variable" = "Description"))

# per town, calc sum of avg of 3 super sector
count3 <- S2403 %>% 
  group_by(NAME, `Super-Sector`) %>%
  summarise(Sum3 = sum(estimate)) %>%
  ungroup()

# per town, calc sum of avg of 10 sectors
count10 <- S2403 %>% 
  group_by(NAME, `Emp Agent`, `Super-Sector`) %>%
  summarise(Sum10 = sum(estimate)) %>%
  ungroup()

summa <- convers[c("Emp Agent","Super-Sector")] %>% group_by(`Emp Agent`, `Super-Sector`) %>% summarise(`Super-Sector` = first(`Super-Sector`))

# join together
count10s <- left_join(count10, summa, by=c("Emp Agent","Super-Sector"))

count310 <- left_join(count10s, count3, by=c("NAME", "Super-Sector"))

count310$perc <- round(count310$Sum10/count310$Sum3,3)

write.csv(count310, "count310.csv")











