library(shiny)
library(plotly)
library(dplyr)
library(leaflet)

data = read.csv("athlete_events.csv", stringsAsFactors = FALSE)
ALLSPORT=c("All",sort(na.omit(unique(data$Sport))))
SPORT_HTML=c()
for(s in ALLSPORT){
  #s轉成小寫
  ss=tolower(s)
  if(file.exists(paste0("Sports_image/",gsub(" ","_",ss),"_pictogram.png"))){
    SPORT_HTML <- c(SPORT_HTML, paste0("<img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/",gsub(" ","_",ss),"_pictogram.png' width='30px'><div class='jhr'></div>", s))
  }else{
    SPORT_HTML <- c(SPORT_HTML, paste0("<img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/default.png' width='30px'><div class='jhr'></div>", s))
  }
}

