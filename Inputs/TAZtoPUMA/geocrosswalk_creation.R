# create geo cross walk

setwd("J:/Shared drives/TMD_TSA/Model/software/PopSim/Inputs/TAZtoPUMA")

library(tidyverse)

tazpuma <- read_csv("TAZPUMA.csv")

tp <- tazpuma %>% group_by(id) %>% filter(AreaMi == max(AreaMi))

tp <- tp %>% select(id, GEOID) %>% rename("TAZ" = id, "PUMA" = GEOID)

tp$REGION <- 1

write_csv(tp,"geo_cross_walk.csv")
