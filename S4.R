###############
#
#策略：處理連續三天上漲
#
###############
library(Quandl)
library(quantmod)
library(dplyr)
par(family = 'STHeiti')
############### 
#處理資料
###############
#StockCode <- "GOOGL"
StockCode <- "2330.TW"
#StockCode <- "^TWII"
stockData <- getSymbols(StockCode)
stockData <- get(stockData)

stockData <- as_data_frame(stockData)
colnames(stockData) <- c("open", "high", "low", "close", "volume","adjusted") 

stockData$date <- as.Date(row.names(stockData)) 
stockData <- stockData %>%
  select(c(date,open:volume)) %>%
# 將成交量為0的交易日刪除
  filter((volume>0)|(!is.na(volume)))  

stockData <- stockData %>%
  filter(stockData$date>"2010-01-01")

################
#處理進場邏輯
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

inSiteTable <- stockData %>%
  filter(
    # 第t天的收盤價 > 第t天的開盤價
    close>open,
    # 第t-1天的收盤價 > 第t-1天的開盤價
    lagClose1>lagOpen1,
    # 第t-2天的收盤價 > 第t-2天的開盤價
    lagClose2>lagOpen2,
    # 第t天的實體K棒長度為1%以上
    kbarValue>0.01,
    # 第t-1天的實體K棒長度為0.5%以下
    lagKbarValue1<0.005,
    # 第t-2天的實體K棒長度為1%以上
    lagKbarValue2>0.01) %>%
  
    # 紀錄連續三日上漲K棒組合發生日及當日收盤價，並重新命名欄位
    select(inDate=date, buyPrice=close)

################
#處理出場邏輯MA05
################
MA05outSiteTable <- stockData %>%
  mutate(
    # 計算第t日的5日移動平均線
    MA05=SMA(close, 5),
    # 計算第t-1日的5日移動平均線
    lagMA05=lag(MA05,1)) %>%
   
  filter(
    close<MA05,
    # t-1日的收盤價>t-1日的05日移動平均線
    lagClose1>MA05) %>%
  # 紀錄收盤價跌破20日移動平均線的發生日及當日收盤價，並重新命名欄位
  select(outDate=date, sellPrice=close)

################
#處理出場邏輯MA20
################
MA20outSiteTable <- stockData %>%
  mutate(
    # 計算第t日的20日移動平均線
    MA20=SMA(close, 20),
    # 計算第t-1日的20日移動平均線
    lagMA20=lag(MA20,1)) %>% 
  filter(
    # # t日的收盤價<t日的20日移動平均線
     close<MA20,
    # # t-1日的收盤價>t-1日的20日移動平均線
     lagClose1>MA20) %>%
    
  # 紀錄收盤價跌破20日移動平均線的發生日及當日收盤價，並重新命名欄位
  select(outDate=date, sellPrice=close) 
################
#處理交易明細
################
# 建立交易明細表MA05
MA05tradeDetailTable <- NULL   
for(ix in 1:nrow(inSiteTable)){
# 目前的進場日期
  inDate <- inSiteTable$inDate[ix] 
# 找尋進場日期往後最近的出場位置
  outSite <- which(MA05outSiteTable$outDate>inDate)[1] 
# 防呆機制，如果進場日期在資料尾端，有可能發生資料不足找不到出場位置的狀況
    if(length(outSite)>0){                            
# 將該筆進場資訊與對應的出場資訊合併，並儲存至交易明細表內
      MA05tradeDetailTable <- bind_rows(MA05tradeDetailTable, bind_cols(inSiteTable[ix,], MA05outSiteTable[outSite,]))
   }
}
# 建立交易明細表MA20
MA20tradeDetailTable <- NULL   
for(ix in 1:nrow(inSiteTable)){
  # 目前的進場日期
  inDate <- inSiteTable$inDate[ix] 
  # 找尋進場日期往後最近的出場位置
  outSite <- which(MA20outSiteTable$outDate>inDate)[1] 
  # 防呆機制，如果進場日期在資料尾端，有可能發生資料不足找不到出場位置的狀況
  if(length(outSite)>0){                            
    # 將該筆進場資訊與對應的出場資訊合併，並儲存至交易明細表內
    MA20tradeDetailTable <- bind_rows(MA20tradeDetailTable, bind_cols(inSiteTable[ix,], MA20outSiteTable[outSite,]))
  }
}
################
#計算報酬tradeDetailTable
################
buyCostR <- 0.002   # 買入交易成本
sellCostR <- 0.002  # 賣出交易成本
MA05tradeDetailTable <- MA05tradeDetailTable %>%
  mutate(
# 計算報酬率
    ret=sellPrice*(1-sellCostR)/(buyPrice*(1+buyCostR))-1,
# 計算持有日數
    holdDays=as.numeric(outDate-inDate))

MA20tradeDetailTable <- MA20tradeDetailTable %>%
  mutate(
    # 計算報酬率
    ret=sellPrice*(1-sellCostR)/(buyPrice*(1+buyCostR))-1,
    # 計算持有日數
    holdDays=as.numeric(outDate-inDate))

#########
#D1ShowR <- function(){
D1ShowR <- function(MA){
  tradeDetailTable <- NULL
  if(MA=="MA05"){
    tradeDetailTable<-MA05tradeDetailTable
  }
  if(MA=="MA20"){
    tradeDetailTable<-MA20tradeDetailTable
  }
  
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
##############
cat(paste0("*********",MA,"出場策略回測績效*********\n",
           
           "平均報酬率: ",round(meanRet*100,2)," %\n",
           
           "交易次數: ",tradeNums," 次\n",
           
           "勝率: ",round(winRatio*100,2)," %\n",
           
           "報酬率標準差: ",round(sdRet*100,2)," %\n",
           
           "最大報酬率: ",round(maxRet*100,2)," %\n",
           
           "最小報酬率: ",round(minRet*100,2)," %\n",
           
           "平均持有日數: ",round(avgHoldDays,2),"天\n\n"))

}
##############

stockData <- stockData %>%
  mutate(
    MA5=SMA(close,5),     # 5日移動平均線
    MA20=SMA(close,20),   # 20日移動平均線
    MA60=SMA(close,60))   # 60日移動平均線
# # 繪製的交易樣本(在交易明細表列的位置)
# plotSample <- 2

#PlotGraph <- function(plotSample){
  PlotGraph <- function(MA,plotSample){
      if(MA=="MA05"){
        tradeDetailTable<-MA05tradeDetailTable
      }
      if(MA=="MA20"){
        tradeDetailTable<-MA20tradeDetailTable
      }
        tradeDetailTable<-MA05tradeDetailTable
      # 繪製交易樣本的進出場日期
      inDate <- tradeDetailTable$inDate[plotSample]
      outDate <- tradeDetailTable$outDate[plotSample]
      # 繪圖起始日(進場日前35個交易日)，此處用ifelse避免繪製資料超出邊界
      matchSite <- which(stockData$date==inDate)-35
      plotStartDate <- stockData$date[ifelse(matchSite<1, 1, matchSite)]                           
      # 繪圖結束日(出場日後35個交易日)，此處用ifelse避免繪製資料超出邊界
      matchSite <- which(stockData$date==outDate)+35
      plotEndDate <- stockData$date[ifelse(matchSite>nrow(stockData), nrow(stockData), matchSite)]
      # 整理繪製的股價資料期間範圍及欄位資料
      plotData <- stockData[which((stockData$date>=plotStartDate)&(stockData$date<=plotEndDate)),]
      # 取出繪圖資料所需的欄位
      plotData <- plotData %>% select(date:volume, MA5:MA60)
      # 加入進場位置欄位資訊，用於繪圖時標註進場點位
      plotData$inSite <- rep(NA, nrow(plotData))
      plotData$inSite[which(plotData$date==inDate)] <- plotData$open[which(plotData$date==inDate)]*0.95
      # 加入出場位置欄位資訊，用於繪圖時標註出場點位
      plotData$outSite <- rep(NA, nrow(plotData))
      plotData$outSite[which(plotData$date==outDate)] <- plotData$close[which(plotData$date==outDate)]*1.05
      # 將plotData資料由tibble格式轉為xts格式，符合chart_Series繪圖格式要求
      plotData <- xts(plotData[,-1], order.by= plotData$date)
      #
      # 設定K棒顏色
      myTheme <- chart_theme()
      myTheme$col$dn.col <- c("chartreuse3")  # 跌的K棒顏色
      myTheme$col$up.col <- c("firebrick3")   # 漲的K棒顏色
      # 繪製各交易日的K棒圖形(主圖)
      pic <- chart_Series(x=plotData[,1:5], name=paste0(StockCode," 技術分析圖形"), theme=myTheme)
      # 加入成交量圖形
      pic <- add_Vo()
      # 加入5日移動平均線
      pic <- add_TA(x=plotData$MA5, on=1, type="l", col="blue", lwd=1.5)
      # 加入20日移動平均線
      pic <- add_TA(x=plotData$MA20, on=1, type="l", col="orange", lwd=1.5)
      # 加入60日移動平均線
      pic <- add_TA(x=plotData$MA60, on=1, type="l", col="green", lwd=1.5)
      # 標註進場位置
      pic <- add_TA(x=plotData$inSite, on=1, type="p", col="red", pch=2, cex=5, lwd=1.5)
      # 標註出場位置
      pic <- add_TA(x=plotData$outSite, on=1, type="p", col="green", pch=6, cex=5, lwd=1.5)
      return(pic)
  }
##
D1ShowR("MA05")
D1ShowR("MA20")
#PlotGraph(6)
PlotGraph("MA05",4)
PlotGraph("MA20",4)
