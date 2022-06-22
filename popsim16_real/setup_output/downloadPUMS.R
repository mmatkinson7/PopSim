#download required PUMS data for PopSim

setwd("J:\\Shared drives\\TMD_TSA\\Model\\software\\PopSim\\popsim")

library(tidycensus)
library(tidyverse)

# look at all variables needed
pums_vars <- pums_variables %>%
  filter(year == 2019, survey == "acs5")

#download just the variables you need for MA, RI, and NH
pums <- get_pums(
  variables = c("PUMA","SERIALNO","TYPE","ST","WGTP","PWGTP","NP","HINCP",
                "ADJINC","SPORDER","ESR","PINCP"),
  state = c("MA","RI","NH"),
  survey = "acs5",
  year = 2019
)

#get just housing units, exclude group quarters
pums <- pums %>% filter(TYPE == 1)
#just get PUMAs in the model region
#pums <- pums %>% filter((ST != 33 & PUMA != "00100") & (ST != 33 & PUMA != "00200"))
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


#still need to deal with the adjustment factor for $
# adjusting to 2015 with ADJINC
# adjusting to 2010 with 0.913 (8.7% inflation 2010 to 2015)
pums <- pums %>% mutate(HHINCPADJ = round(HINCP * as.numeric(ADJINC)* 0.913,0))
pums <- pums %>% mutate(PINCPADJ = round(PINCP * as.numeric(ADJINC) * 0.913,0))

# HH categories for people per HH
pums <- pums %>% mutate(HHNP = if_else(NP<3,NP,4))

# HH categories for workers per HH
pums <- pums %>% mutate(HHEMPCAT = if_else(HHEMP<3,HHEMP,3))

#separate HH and P
hh <- pums %>% distinct(SERIALNO,TYPE, WGTP, NP, PUMA, ST, HHNP, HHEMP,HHEMPCAT,HHINCPADJ)

p <- pums %>% select(SERIALNO, PWGTP, SPORDER, ESR, EMP, PINCPADJ, PUMA, ST)

p$PUMA <- paste0(p$ST,p$PUMA)
hh$PUMA <- paste0(hh$ST,hh$PUMA)

write_csv(hh, "hh_pums_2019_5YR.csv")
write_csv(p, "person_pums_2019_5YR.csv")
write.csv(pums, "pums_2019_5YR_MARINH.csv")



