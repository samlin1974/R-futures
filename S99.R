


library(Quandl)
mydata = Quandl("TAIFEX/TXN2018")
mydata1 = Quandl('TAIFEX/TXN2018', start_date='2018-06-15', end_date='2018-06-20')
mydata3 = Quandl('TAIFEX/MTXN2018', start_date='2018-06-15', end_date='2018-06-20')

colnames(stockData) <- c("open", "high", "low", "close", "volume","adjusted") 
