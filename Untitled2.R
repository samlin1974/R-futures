###############
#
#策略：小型台指期處理連續三天上漲 
# 2018/06/01 01:51:00   缺資料
###############
library(dplyr)
library(lubridate)
library(quantmod)
par(family = 'STHeiti')
#######
#處理資料
#download.file(url="https://mega.nz/#F!y8o0wC5I!tk83Y0x7qS8LLh8O801gcg/mtx_1min_hot.zip", destfile="mtx_1min_hot.zip.zip")
#unzip("mtx_1min_hot.zip", exdir="mtx")
#######
setwd("/Users/samlin/Downloads/futures/")
unzip("mtx_1min_hot.zip", exdir="mtx")
mtxData = read.table(file=file.path("//Users/samlin/Downloads/futures/mtx/mtx_1min_hot.txt"), header=FALSE, sep=",", stringsAsFactors=FALSE)
colnames(mtxData) <- c("prod","period","Tdate","Ttime","open", "high","low", "close", "volume") 
mtxData2<-NULL
mtxData <- mtxData %>%
  filter(mtxData$Tdate>="2018/06/01")

mtxData2$Tdate <- as.Date(mtxData2$Tdate) 
mtxData2$Ddate<-mtxData2$Tdate
mtxData2$flag<-"B"
for(ix in 1:nrow(mtxData2)){
  Ttime <- mtxData2$Ttime[ix] 
  #mtxData2$no[ix]<-ix
  if (Ttime<"08:45:00")
  {
    mtxData2$Ddate[ix]<-mtxData2$Tdate[ix]-1
  }else if ((Ttime>"08:45:00")&&(Ttime<="13:45:00"))
  {
    mtxData2$flag[ix]<-"A"
  }
}
##
  dt<-ymd_hms(paste0(mtxData2$Ddate," ","08:45:00"))
  dt1<-ymd_hms(paste0(mtxData2$Tdate," ",mtxData2$Ttime))
  # datetime
  mtxData2$datetime<-dt1
  #Kno 表示單位編號/day 計算k棒群組用 為補足四碼 加999
  mtxData2$Kno<-as.numeric(difftime(dt1,dt, units="mins"))+1000
##
  K<-3
  #dk k棒編號的商數 決定哪一支新k
  mtxData2$dk<-(mtxData2$Kno-1000+K-1)%/%K
  #dk1 k棒編號的餘數 決定該群第幾個
  mtxData2$dk1<-(mtxData2$Kno-1000+K-1)%%K
  #Uno 表示取唯一編號 
  mtxData2$Uno<-paste0(mtxData2$Ddate-ymd("19700101"),mtxData2$dk)
#####
 
  #mtxData2<-mtxData2[1:10,]%>%
  mtxData2<-mtxData2%>%
  mutate(
    #新K open
    Kopen=lag(open,K-1),
    #新K close
    Kclose=close
  )
mtxData3 <- NULL
  mtxData3<-group_by(mtxData2, Uno) %>%
    summarise(
      #新K high
      Khigh=max(high),
      #新K low
      Kmin=min(low),
      #新K volume
      Kvolume=sum(volume))
  
   
  mtxData4 <- NULL
  for(ix in 1:nrow(mtxData3)){
    dKno <- mtxData3$Uno[ix] #計算新k 用dk
    dd <- which(mtxData2$Uno==dKno & mtxData2$dk1==K-1)[1] #對應新k後的最後一筆資料
     
    mtxData4<- bind_rows(mtxData4, 
                      bind_cols(mtxData2[dd,1:4],
                                #mtxData2[dd,8:9],
                                mtxData2[dd,17:18],
                                mtxData3[ix,1:4],
                                mtxData2[dd,11:12])
    )
    }
   
  