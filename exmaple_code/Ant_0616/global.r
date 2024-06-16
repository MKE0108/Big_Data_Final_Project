library(dplyr)
library(ggplot2)
library(maps)
library(tibble)
library(tidyr)

data=read.csv("./athlete_events.csv",sep=",",header=T)
noc=read.csv("./noc_regions.csv",sep=",",header=T)

data_regions <- data %>% 
  left_join(noc,by="NOC") %>%
  filter(!is.na(region))

Year_sel=sort(unique(data_regions$Year)) # 0616更新