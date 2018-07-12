###############
#策略：處理日期 
#月底進隔月出 按提供日期列表(本次列表不精準)
###############
library(lubridate)
library(quantmod)
library(dplyr)
par(family = 'STHeiti')
############### 
#處理資料
###############
wdpath=paste0(getwd(),"/Documents/stock/")
indateList<-read.csv(paste0(wdpath,"/indate.csv"))
outdateList<-read.csv(paste0(wdpath,"/outdate.csv"))
stockcodeList<-read.csv(paste0(wdpath,"/stockcode1.csv"))
resulttbl<-NULL
for (i in 1:nrow(stockcodeList)) {
  StockCode <-as.character(stockcodeList$stockcode[i])
  #StockCode <- "GOOGL"
  #StockCode <- "2330.TW"
stockData <- getSymbols(StockCode)
stockData <- get(stockData)

stockData <- as_data_frame(stockData)
colnames(stockData) <- c("open", "high", "low", "close", "volume","adjusted") 

indateList$date<-ymd(indateList$date)
outdateList$date<-ymd(outdateList$date)
stockData$date <- as.Date(row.names(stockData)) 
stockData <- stockData %>%
  select(c(date,open:volume)) %>%
# 將成交量為0的交易日刪除
  filter((volume>0)|(!is.na(volume)))  
#
stockData <- stockData %>%
  filter(stockData$date>"2010-01-01")

################
#處理進場邏輯 26開盤價
################
stockData <- stockData %>%
mutate(
# 第t-1天收盤價
  lagClose1=lag(close,1), 
# 第t-2天收盤價
  lagClose2=lag(close,2),
# 第t-1天開盤價
  lagOpen1=lag(open,1),
# 第t-2天開盤價
  lagOpen2=lag(open,2),
# 第t天的實體K棒長度
  kbarValue=abs(close-open-1), 
# 第t-1天的實體K棒長度
  lagKbarValue1=lag(kbarValue,1),
# 第t-2天的實體K棒長度
  lagKbarValue2=lag(kbarValue,2)) 

inSiteTable<-inner_join(indateList, stockData)%>%
  select(inDate=date, buyPrice=open)
################
#處理出場邏輯 06收盤價
################
  outSiteTable<-inner_join(outdateList, stockData)%>%
  select(outDate=date, sellPrice=close)
################
#處理交易明細
################
# 建立交易明細表
tradeDetailTable <- NULL   
for(ix in 1:nrow(inSiteTable)){
# 目前的進場日期
  inDate <- inSiteTable$inDate[ix] 
# 找尋進場日期往後最近的出場位置
  outSite <- which(outSiteTable$outDate>inDate)[1] 
# 防呆機制，如果進場日期在資料尾端，有可能發生資料不足找不到出場位置的狀況
    if(length(outSite)>0){                            
# 將該筆進場資訊與對應的出場資訊合併，並儲存至交易明細表內
      tradeDetailTable <- bind_rows(tradeDetailTable, bind_cols(inSiteTable[ix,],outSiteTable[outSite,]))
   }
}
tradeDetailTable <- tradeDetailTable %>%
  # 將無出場交易日刪除
  filter(!is.na(outDate)) 
################
#計算報酬tradeDetailTable
################
buyCostR <- 0.003   # 買入交易成本
sellCostR <- 0.002  # 賣出交易成本
tradeDetailTable <- tradeDetailTable %>%
  mutate(
# 計算報酬率
    ret=sellPrice*(1-sellCostR)/(buyPrice*(1+buyCostR))-1,
# 計算持有日數
    holdDays=as.numeric(outDate-inDate))
################
#6.計算結果resulttbl
################

# 平均報酬率
meanRet <- mean(tradeDetailTable$ret)
# 報酬率標準差
sdRet <- sd(tradeDetailTable$ret)
# 交易次數
tradeNums <- nrow(tradeDetailTable)
# 勝率
winRatio <- sum(as.numeric(tradeDetailTable$ret>0))/tradeNums
# 最大報酬率
maxRet <- max(tradeDetailTable$ret)
# 最小報酬率
minRet <- min(tradeDetailTable$ret)
# 平均持有日數
avgHoldDays <- mean(tradeDetailTable$holdDays)

data<-data.frame(StockCode,
                 meanRet=round(meanRet*100,2), #"平均報酬率
                 tradeNums=tradeNums, #交易次數
                 winRatio=round(winRatio*100,2), #勝率
                 sdRet=round(sdRet*100,2), #報酬率標準差
                 maxRet=round(maxRet*100,2), #最大報酬率
                 minRet=round(minRet*100,2), #最小報酬率           
                 avgHoldDays=round(avgHoldDays,2) #平均持有日數
)
  #處理寫結果
  
  if (nrow(tradeDetailTable)==0){
    resulttbl<-data
  }else{
    resulttbl <- rbind(resulttbl, data)
  }

} #End of forloop
## write your table to file
write.table(resulttbl, file=paste0(wdpath,"/resulttbl51.csv"),sep=",",row.names=FALSE)

