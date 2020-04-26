## Libraries
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(BatchGetSymbols) # Get list of SP500 stocks
library(tidyverse) 
library(lubridate) ## Date Manipulation Package
library(sparseIndexTracking) # Sparse Index Tracking
library(xts) # time seires

####### Getting Date Variables
year_ago_today_date <- NULL


if (((year(Sys.Date())) %% 4 == 0) & ((month(Sys.Date())) >= 3)) {
  year_ago_today_date <- Sys.Date() - days(366)
}

if ((year(Sys.Date()))%%4 != 0) {
  year_ago_today_date <- Sys.Date() - days(365)
}


######## 

## Getting SP500 ticker list
sp500<-GetSP500Stocks()[,c("Tickers")]
tick = c(sp500)

## Switching problem stocks IE. Berkshire and BF.B
brk_val <- match('BRK.B',tick)
bfb_val <- match('BF.B',tick)

tick[brk_val] <- "BRK-B"
tick[bfb_val] <- "BF-B"



########## geting daily price data

price_data <- tq_get(tick,
                     from = year_ago_today_date,
                     get = 'stock.prices')

######### getting daily returns and manipulating data frame

arith_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret')

arith_ret_xts <- arith_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()

################# Beta Calculation ##############


beta_data <- data.frame(matrix(ncol = length(tick), nrow = 1))
colnames(beta_data) <- tick
val = 1

for (i in tick) {
  
  tickers = c(i)
  wts = c(1)
  
  price_data <- tq_get(tickers,
                       from = year_ago_today_date,
                       get = 'stock.prices')
  
  ret_data <- price_data %>%
    group_by(symbol) %>%
    tq_transmute(select = close,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "ret")
  
  port_ret <- ret_data %>%
    tq_portfolio(assets_col = symbol,
                 returns_col = ret,
                 weights = wts,
                 col_rename = 'port_ret',
                 geometric = FALSE)
  
  
  bench_price <- tq_get('SPY',
                        from = year_ago_today_date,
                        get = 'stock.prices')
  
  ###### Calculating benchmark returns  ######
  
  bench_ret <- bench_price %>%
    tq_transmute(select = close,
                 mutate_fun = periodReturn,
                 period = "daily",
                 col_rename = "bench_ret")
  
  comb_ret <- left_join(port_ret,bench_ret, by = 'date')
  
  
  model <- lm(comb_ret$port_ret ~ comb_ret$bench_ret)
  
  model_alpha <- model$coefficients[1]
  
  model_beta <- model$coefficients[2]
  
  beta_data[1,val] <- model_beta
  
  
  val <- val + 1
  print(val)
  
}

####### Filtering stocks based on Beta ######

stock_filter <- c()


for (i in 1:ncol(beta_data)) {
  if ((beta_data[1,i] <= 1.05) & (beta_data[1,i] >= .85 ))  {
    stock_filter <- c(stock_filter, tick[i])
  }
  
}



