```{R}
B=read.csv("athlete_events.csv")
```

## 拿到過去的國家代碼
```{R}
get_OldCode=function(Country,NOC){
    #Country=read.csv("CountryCode.csv")
    Code=Country$Code
    return(unlist(strsplit(Country[which(Country$Code==NOC),]$OldCode," ")))
}
Country=read.csv("CountryCode.csv")
Code=Country$Code
Target_NOC="TPE"
Old_NOC=get_OldCode(Country,Target_NOC)
#取得資訊
Target_INFO=B[which(B$NOC==Target_NOC | B$NOC==Old_NOC),]
```

## 不同的運動項目
```{R}
for (sp in unique(B$Sport)){
    print(sp)
    print('||||||||||||||||')
    print(unique(B[which(B$Sport==sp),]$Event))
}
```



