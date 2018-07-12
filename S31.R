###############
#
#策略：處理連續三天上漲
#驗證股票股性
###############
library(quantmod)
library(dplyr)
  ################ 
  #1.準備/處理資料
  ################
  wdpath=paste0(getwd(),"/Documents/stock")
  stockcodeList<-read.csv(paste0(wdpath,"/stockcode.csv"))
  tradeDetailTable <- NULL
  resulttbl<-NULL
  
  for (i in 1:nrow(stockcodeList)) {
    StockCode <-as.character(stockcodeList$stockcode[i])
    #StockCode<- "00625K.TW" 
    #StockCode<- "0059.TW"
    stockData <- getSymbols(StockCode)
    stockData <- get(stockData)
    
    stockData <- as_data_frame(stockData)
    colnames(stockData) <- c("open", "high", "low", "close", "volume","adjusted") 
    
    stockData$date <- as.Date(row.names(stockData)) 
    stockData$stockcode <- StockCode 
    stockData <- stockData %>%
      select(c(stockcode,date,open:volume)) %>%
    # 將成交量為0的交易日刪除
      filter((volume>0)|(!is.na(volume)))  
    
    stockData <- stockData %>%
      filter(stockData$date>"2010-01-01")
    
    if(nrow(stockData)>20){
  ################
  #2.處理進場邏輯
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
      select(stockcode,inDate=date, buyPrice=close)
  ################
  #3.處理出場邏輯
  ################
    if(nrow(inSiteTable)>0){
      
    outSiteTable <- stockData %>%
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
      select(stockcode,outDate=date, sellPrice=close)
    ################
    #4.處理交易明細
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
            tradeDetailTable <- bind_rows(tradeDetailTable, bind_cols(inSiteTable[ix,], outSiteTable[outSite,]))
         }
      }
    ################
    #5.計算報酬tradeDetailTable
    ################
      
      buyCostR <- 0.002   # 買入交易成本
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
      
      }#End if no enought stockdata
    }#End if no insitedata
} #End of forloop

## write your table to file
write.table(resulttbl, file=paste0(wdpath,"/resulttbl.csv"),sep=",",row.names=FALSE)

