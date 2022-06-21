#download required PUMS data for PopSim

setwd("J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data/employment3to10")


library(readxl)
library(tidycensus)
library(tidyverse)


convers <- read_excel("three2ten_conversion.xlsx")
convers <- convers %>% select(NAICS_Sectors, `Emp Agent`)


# look at all variables needed
pums_vars <- pums_variables %>%
  filter( survey == "acs5", 
         var_code == "NAICSP", level == "person") %>%
  select(val_max, val_label)

pums_vars <- pums_vars %>% mutate(val_label = str_replace_all(val_label, c("-(.*)" = "")))
pums_vars$NAICS = substr(pums_vars$val_max,1,2)

pums_vars <- pums_vars %>% mutate(NAICS = case_when(
  NAICS == 31 ~ "31-33",
  NAICS == 32 ~ "31-33",
  NAICS == 33 ~ "31-33",
  NAICS == 44 ~ "44-45",
  NAICS == 45 ~ "44-45",
  NAICS == 48 ~ "48-49",
  NAICS == 49 ~ "48-49",
  TRUE ~ NAICS
))

#download just the variables you need for MA, RI, and NH
pums <- get_pums(
  variables = c("PUMA","SERIALNO","TYPE","ST","WGTP","PWGTP","NP","HINCP",
                "ADJINC","SPORDER","ESR","PINCP","AGEP","SCH","NAICSP"),
  state = c("MA","RI","NH"),
  survey = "acs5",
  year = 2019
)

#get just housing units, exclude group quarters
pums <- pums %>% filter(TYPE == 1)
pums <- pums %>% filter(WGTP > 0 & PWGTP > 0)

pums_vars <- pums_vars %>% select("NAICS","val_max")
pums_vars <- unique(pums_vars)

#join with naics variables
pums <- left_join(pums, pums_vars, by=c("NAICSP" = "val_max"))
pums <- left_join(pums, convers, by=c("NAICS" = "NAICS_Sectors"))

#make sure SERIALNO is just numbers
pums$SERIALNO <- gsub("HU","",pums$SERIALNO)
pums$SERIALNO_orig <- pums$SERIALNO
jt <- data.frame(SERIALNO_orig = unique(pums$SERIALNO_orig))
jt$SERIALNO <- rownames(jt)

pums$SERIALNO <- jt$SERIALNO[match(unlist(pums$SERIALNO_orig), jt$SERIALNO_orig)]

# Data dictionary for ESR
#ESR        1      
#Employment status recode
#b .N/A (less than 16 years old)
#1 .Civilian employed, at work
#2 .Civilian employed, with a job but not at work (this week)
#3 .Unemployed
#4 .Armed forces, at work
#5 .Armed forces, with a job but not at work
#6 .Not in labor force

# make column called EMP that is binary on whether person is employed or not
pums <- pums %>% mutate(EMP = if_else(ESR<3,1,0))
# #get workers per HH
pums <- pums %>% 
  group_by(SERIALNO) %>%
  mutate(HHEMP = sum(EMP)) %>%
  ungroup()

#Deal with SCH - School Enrollment (need to do by age)
pums <- pums %>% 
  mutate(SCH = case_when(
    SCH == 'b' ~ 0,
    SCH == 1 ~ 0,
    SCH > 1 ~ 1
))
pums <- pums %>% mutate(ENRLL = if_else(SCH > 0 & AGEP < 19,1,0))


#still need to deal with the adjustment factor for $
# adjusting to 2015 with ADJINC
# adjusting to 2010 with 0.913 (8.7% inflation 2010 to 2015)
pums <- pums %>% mutate(HHINCPADJ = round(as.numeric(HINCP) * as.numeric(ADJINC)* 0.913,0))
pums <- pums %>% mutate(PINCPADJ = round(as.numeric(PINCP) * as.numeric(ADJINC) * 0.913,0))

# HH categories for people per HH
pums <- pums %>% mutate(HHNP = if_else(NP<3,as.numeric(NP),4))

# HH categories for workers per HH
pums <- pums %>% mutate(HHEMPCAT = if_else(HHEMP<3,HHEMP,3))

# make employment columns
pums <- pums %>% mutate(
  Emp1 = if_else(`Emp Agent` != 1 | is.na(`Emp Agent`), 0, 1),
  Emp2 = if_else(`Emp Agent` != 2 | is.na(`Emp Agent`), 0, 1),
  Emp3 = if_else(`Emp Agent` != 3 | is.na(`Emp Agent`), 0, 1),
  Emp4 = if_else(`Emp Agent` != 4 | is.na(`Emp Agent`), 0, 1),
  Emp5 = if_else(`Emp Agent` != 5 | is.na(`Emp Agent`), 0, 1),
  Emp6 = if_else(`Emp Agent` != 6 | is.na(`Emp Agent`), 0, 1),
  Emp7 = if_else(`Emp Agent` != 7 | is.na(`Emp Agent`), 0, 1),
  Emp8 = if_else(`Emp Agent` != 8 | is.na(`Emp Agent`), 0, 1),
  Emp9 = if_else(`Emp Agent` != 9 | is.na(`Emp Agent`), 0, 1),
  Emp10 = if_else(`Emp Agent` != 10 | is.na(`Emp Agent`), 0, 1) 
)

#separate HH and P
hh <- pums %>% distinct(SERIALNO,TYPE, WGTP, NP, PUMA, ST, HHNP, HHEMP,HHEMPCAT,HHINCPADJ)

p <- pums %>% select(SERIALNO, PWGTP, SPORDER, ESR, EMP, PINCPADJ, PUMA, ST, ENRLL, AGEP, 
                     Emp1, Emp2, Emp3, Emp4, Emp5, Emp6, Emp7, Emp8, Emp9, Emp10)

p$PUMA <- paste0(p$ST,p$PUMA)
hh$PUMA <- paste0(hh$ST,hh$PUMA)

setwd("J:/Shared drives/TMD_TSA/Model/software/PopSim/popsim16/data")

write_csv(hh, "hh_pums_2019_5YR.csv")
write_csv(p, "person_pums_2019_5YR.csv")
write.csv(pums, "pums_2019_5YR_MARINH.csv")





