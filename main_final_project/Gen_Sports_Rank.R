library(dtplyr)
data=read.csv("./athlete_events.csv",sep=",",header=T)
Sports=data%>%select(Sport)%>%distinct()%>%arrange(Sport)

for(s in Sports$Sport){
  D=data%>%filter(Sport==s)
  D=D%>%group_by(Year,NOC,Sport,Event,Medal)%>%filter(!is.na(Medal))%>%summarise(n=1)%>%group_by(NOC)%>%summarise(Gold=sum(Medal=="Gold"),Silver=sum(Medal=="Silver"),Bronze=sum(Medal=="Bronze"),Weight=sum(Medal=="Bronze")+1.5*sum(Medal=="Silver")+2*sum(Medal=="Gold"))%>%arrange(desc(Weight))
  D=D[1:4]
  write.csv(D,file=paste0("./Sports_rank/",s,".csv"),col.names=T,row.names=F)
}
D=data[!is.na(data),]
D=D%>%group_by(Year,NOC,Sport,Event,Medal)%>%filter(!is.na(Medal))%>%summarise(n=1)%>%group_by(NOC)%>%summarise(Gold=sum(Medal=="Gold"),Silver=sum(Medal=="Silver"),Bronze=sum(Medal=="Bronze"),Weight=sum(Medal=="Bronze")+1.5*sum(Medal=="Silver")+2*sum(Medal=="Gold"))%>%arrange(desc(Weight))
D=D[1:4]
write.csv(D,file=paste0("./Sports_rank/","All",".csv"),col.names=T,row.names=F)