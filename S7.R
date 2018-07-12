

library(dplyr)
library(lubridate)
#######
#處理資料
#download.file(url="https://mega.nz/#F!y8o0wC5I!tk83Y0x7qS8LLh8O801gcg/mtx_1min_hot.zip", destfile="mtx_1min_hot.zip.zip")
#unzip("mtx_1min_hot.zip", exdir="mtx")
#######
setwd("/Users/samlin/Downloads/futures/")
unzip("mtx_1min_hot.zip", exdir="mtx")
mtxData = read.table(file=file.path("//Users/samlin/Downloads/futures/mtx/mtx_1min_hot.txt"), header=FALSE, sep=",", stringsAsFactors=FALSE)

#stockData <- as_data_frame(stockData)
colnames(mtxData) <- c("prod","period","Tdate","Ttime","open", "high","low", "close", "volume") 
mtxData2<-NULL
mtxData2 <- mtxData %>%
  filter(mtxData$Tdate>"2018/06/21")

mtxData2$Tdate <- as.Date(mtxData2$Tdate) 
mtxData2$Ddate<-mtxData2$Tdate
mtxData2$flag<-"B"

for(ix in 1:nrow(mtxData2)){
  Ttime <- mtxData2$Ttime[ix] 
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
mtxData2$Kno<-as.numeric(difftime(dt1,dt, units="mins"))
##

#######



#######
#Rea
#######
# 欄位１：商品代碼，如：TX、TE、TF、MTX
# 欄位２：商品月份，格式：YYYYMM
# 欄位３：交易日期，格式YYYY/MM/DD
# 欄位４：交易時間，格式HH24:MI:SS
# 欄位５：開盤價格，格式99990.99
# 欄位６：最高價格，格式99990.99
# 欄位７：最低價格，格式99990.99
# 欄位８：收盤價格，格式99990.99
# 欄位９：交易數量
# 各欄位以逗號(,)隔開，檔案存成*.txt
# require(stringr)
# theFiles = dir("mtx/", pattern="\\.txt")
# for(a in theFiles){
#     temp = read.table(file=file.path("mtx", a), header=FALSE, sep=",", stringsAsFactors=FALSE)
# }

