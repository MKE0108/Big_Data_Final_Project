library(dplyr)
library(shinyWidgets)
library(shiny)
dataBase=read.csv("athlete_events.csv")
Country=read.csv("CountryCode.csv")
get_OldCode=function(Country,NOC){
    #Country=read.csv("CountryCode.csv")
    Code=Country$Code
    return(unlist(strsplit(Country[which(Country$Code==NOC),]$OldCode," ")))
}




#A.input selection && ingore NA
A.Season_sel=na.omit(unique(dataBase$Season))
A.NOC_sel=sort(na.omit(unique(dataBase$NOC)))

A.NOC_sel_path <- paste0("<img src='","http://127.0.0.1:5500/Country_image/", A.NOC_sel, ".png' onerror='this.onerror=null;this.src=\"http://127.0.0.1:5500/Country_image/default.png\"' width='30px'><div class='jhr'></div>", A.NOC_sel)

A.Sport_sel=na.omit(unique(dataBase$Sport))
A.Begin_Year_sel=sort(na.omit(unique(dataBase$Year)))
A.End_Year_sel=sort(na.omit(unique(dataBase$Year)))
A.Medal_sel=na.omit(unique(dataBase$Medal))



# allzips <- readRDS("data/superzip.rds")
# allzips$latitude <- jitter(allzips$latitude)
# allzips$longitude <- jitter(allzips$longitude)
# allzips$college <- allzips$college * 100
# allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
# row.names(allzips) <- allzips$zipcode

# cleantable <- allzips %>%
#   select(
#     City = city.x,
#     State = state.x,
#     Zipcode = zipcode,
#     Rank = rank,
#     Score = centile,
#     Superzip = superzip,
#     Population = adultpop,
#     College = college,
#     Income = income,
#     Lat = latitude,
#     Long = longitude
#   )
