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



### for 運動強國 pickerInput ####
ALLSPORT=c("All",sort(na.omit(unique(data$Sport))))
SPORT_HTML=c()
for(s in ALLSPORT){
  if(file.exists(paste0("Sports_image/",gsub(" ","_",s),"_pictogram.png"))){
    SPORT_HTML <- c(SPORT_HTML, paste0("<img src='","http://127.0.0.1:5500/Sports_image/",gsub(" ","_",s),"_pictogram.png' width='30px'><div class='jhr'></div>", s))
  }else{
    SPORT_HTML <- c(SPORT_HTML, paste0("<img src='","http://127.0.0.1:5500/Sports_image/default.png' width='30px'><div class='jhr'></div>", s))
  }
}
