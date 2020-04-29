## Libraries
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(BatchGetSymbols) # Get list of SP500 stocks
library(tidyverse) 
library(lubridate) ## Date Manipulation Package
library(sparseIndexTracking) # Sparse Index Tracking
library(xts) # time seires

########## Getting Year to Date Variables #########
year_ago_today_date <- NULL


if (((year(Sys.Date())) %% 4 == 0) & ((month(Sys.Date())) >= 3)) {
  year_ago_today_date <- Sys.Date() - days(366)
}

if ((year(Sys.Date()))%%4 != 0) {
  year_ago_today_date <- Sys.Date() - days(365)
}



###### Getting SP500 ticker list   ########
sp500<-GetSP500Stocks()[,c("Tickers")]
tick = c(sp500)

########## Switching problem stocks IE. Berkshire and BF.B   ######
brk_val <- match('BRK.B',tick)
bfb_val <- match('BF.B',tick)

tick[brk_val] <- "BRK-B"
tick[bfb_val] <- "BF-B"



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
  
######### Calculating benchmark returns  ###########
  
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

########### Filtering stocks based on Beta ##########

stock_filter <- c()


for (i in 1:ncol(beta_data)) {
  if ((beta_data[1,i] <= 1.05) & (beta_data[1,i] >= .85 ))  {
    stock_filter <- c(stock_filter, tick[i])
  }
  
}




########### geting daily price data from beta filtered stocks   ###
 
price_data <- tq_get(stock_filter,
                     from = year_ago_today_date,
                     get = 'stock.prices')

############ getting daily returns and manipulating data frame  ####

arith_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret')

arith_ret_xts <- arith_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()


##### SP500 Arith Data Frame

sp500_price_data <- tq_get('SPY',
                     from = year_ago_today_date,
                     get = 'stock.prices')

arith_ret_sp500 <- sp500_price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret')

arith_ret_sp500_xts <- arith_ret_sp500 %>%
  spread(symbol, value = ret) %>%
  tk_xts()




###### Sparse Index Optimization 


constituent_train <- arith_ret_xts[1:126]
constituent_test <- arith_ret_xts[127:252]
sp500_train <- arith_ret_sp500_xts[1:126]
sp500_test <- arith_ret_sp500_xts[127:252]



# ETE tracking error
w_ete <- spIndexTrack(constituent_train, sp500_train, lambda = 1e-7, u = 0.5, measure = 'ete')
cat('Number of assets used:', sum(w_ete > 1e-6))


# DR tracking error 
w_dr <- spIndexTrack(constituent_train, sp500_train, lambda = 2e-8, u = 0.5, measure = 'dr')
cat('Number of assets used:', sum(w_dr > 1e-6))


# HETE tracking error
w_hete <- spIndexTrack(constituent_train, sp500_train, lambda = 8e-8, u = 0.5, measure = 'hete', hub = 0.05)
cat('Number of assets used:', sum(w_hete > 1e-6))


# HDR tracking error
w_hdr <- spIndexTrack(constituent_train, sp500_train, lambda = 2e-8, u = 0.5, measure = 'hdr', hub = 0.05)
cat('Number of assets used:', sum(w_hdr > 1e-6))

### removing null weights from list

updated_w_hdr_list <- c()
updated_w_hete_list <- c()
updated_w_ete_list <- c()
updated_w_dr_list <- c()

for (i in 1:length(w_dr)) {
  if (w_hdr[i] != 0) {
    updated_w_hdr_list <- c(updated_w_hdr_list, w_hdr[i])
  }
 if (w_dr[i] != 0) {
     updated_w_dr_list <- c(updated_w_dr_list, w_dr[i])
 }
  if (w_hete[i] != 0) {
    updated_w_hete_list <- c(updated_w_hete_list, w_hete[i])
  }
  if (w_ete[i] != 0) {
    updated_w_ete_list <- c(updated_w_ete_list, w_ete[i])
  }
}


print(updated_w_hete_list)
print(updated_w_hdr_list)
print(updated_w_ete_list)
print(updated_w_dr_list)

optimize_tickers <- names(updated_w_dr_list)

m1 <- matrix(updated_w_dr_list, ncol=length(updated_w_dr_list), byrow=TRUE)
d1 <- as.data.frame(m1, stringsAsFactors=FALSE)
colnames(d1) <- optimize_tickers <- names(updated_w_dr_list)

optimize_tickers <- c(names(updated_w_dr_list))

print(optimize_tickers)

print(d1)

v1 <- nrow(arith_ret_xts)
arith_ret_xts[v1,'ABT']


