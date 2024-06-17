library(dplyr)
library(ggplot2)
library(maps)
library(tibble)
library(tidyr)
library(dplyr)
library(plotly)
library(RColorBrewer)
library(htmlwidgets)
library(magrittr)
data=read.csv("./athlete_events.csv",sep=",",header=T)
noc=read.csv("./noc_regions.csv",sep=",",header=T)

### for 地圖數據 ####
data_regions <- data %>% 
  left_join(noc,by="NOC") %>%
  filter(!is.na(region))
Map1_Year_sel=sort(unique(data_regions$Year))
if(!file.exists("NOC_summary_with_map.csv")){
  source("Gen_NOC_summary_with_map.R")
}

NOC_summary_with_map= read.csv("NOC_summary_with_map.csv", row.names = 1)
### for 運動強國 ####
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


if(!file.exists("Sports_rank/Tennis.csv")){
  source("Gen_Sports_Rank.R")
}
Sports=data%>%select(Sport)%>%distinct()%>%arrange(Sport)

SPORTS_RANK=c()
for(s in Sports$Sport){
  D=read.csv(paste0("./Sports_rank/",s,".csv"))
  SPORTS_RANK[[s]]=D
}
D=read.csv(paste0("./Sports_rank/","All",".csv"))
SPORTS_RANK[["All"]]=D


ALL_NOC=sort(na.omit(unique(data$NOC)))
NOC_HTML=c()
for(n in ALL_NOC){
  n=as.character(n)
  index=which(noc$NOC==n)
  
  if(length(index)==0){
    NOC_HTML <- c(NOC_HTML,paste0("<img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Country_image/default.png' width='30px'><div class='jhr'></div>", n,"( ? )"))
  }else{
    region_name=noc[index,2]
    if(is.na(region_name)){
      region_name="?" 
    }
    flag=0
    for(possible_noc in noc$NOC[which(noc$region==region_name)]){
      if(file.exists(paste0("Country_image/",possible_noc,".png"))){
        NOC_HTML <- c(NOC_HTML,paste0("<img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Country_image/",possible_noc,".png' width='30px'><div class='jhr'></div>", n,"(",region_name,")"))
        flag=1
        break
      }
    }

    if(flag==0){
      NOC_HTML <- c(NOC_HTML,paste0("<img src='","https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Country_image/default.png' width='30px'><div class='jhr'></div>", n,"(",region_name,")"))
    }

  }


}


### for 歷史數據 ####
#data <- read.csv("athlete_events.csv", header = TRUE, sep=',', na.strings = c("", "NA"))

womenInOlympics <- data %>%
  filter(Sex == 'F') %>%
  group_by(Year, Season) %>%
  summarize(Count = n()) %>%
  arrange(Year)

gold_medals <- data%>%
  filter(Medal=='Gold') 

notNullMedals <- gold_medals %>%
  filter(!is.na(Height) & !is.na(Weight))

createColorScaleAndNormalize <- function(data, count_col, color_palette = "YlOrRd", num_colors = 100) {
  # 创建颜色梯度
  color_palette <- colorRampPalette(brewer.pal(9, color_palette))(num_colors)
  colorscale <- lapply(seq(0, 1, length.out = num_colors), function(i) {
    list(i, color_palette[i * (num_colors - 1) + 1])
  })
  
  # norm
  max_count <- max(data[[count_col]], na.rm = TRUE)
  min_count <- min(data[[count_col]], na.rm = TRUE)
  normalized_col_name <- paste0(count_col, "Normalized")
  data[[normalized_col_name]] <- (data[[count_col]] - min_count) / (max_count - min_count)
  #print(data)
  list(data = data, colorscale = colorscale)
}
Sport_url=c()
for(s in ALLSPORT){
  #s轉成小寫
  ss=tolower(s)
  if(file.exists(paste0("Sports_image/",gsub(" ","_",ss),"_pictogram.png"))){
      Sport_url <- c(Sport_url, paste0("https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/",gsub(" ","_",ss),"_pictogram.png"))
  }else{
      Sport_url <- c(Sport_url,paste0("https://raw.githubusercontent.com/MKE0108/Big_Data_Final_Project/main/main_final_project/Sports_image/default.png"))
  }
}
names(Sport_url) <- ALLSPORT