## Libraries
library(tidyquant) # To download the data
library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(BatchGetSymbols) # Get list of SP500 stocks
library(tidyverse)
library(lubridate) ## Date Manipulation Package
library(sparseIndexTracking) # Sparse Index Tracking
library(xts) # time seires
library(PerformanceAnalytics)

total_port_return <- c()

#################################################### Date Variables ########################################################################
year_ago_today_date <- NULL
final_date <- Sys.Date()

if (((year(Sys.Date())) %% 4 == 0) & ((month(Sys.Date())) >= 3)) {
  year_ago_today_date <- Sys.Date() - days(366)
}

if ((year(Sys.Date()))%%4 != 0) {
  year_ago_today_date <- Sys.Date() - days(365)
}


#####################################################   Getting SP500 ticker list   ###############################################
sp500<-GetSP500Stocks()[,c("Tickers")]
tick = c(sp500)

##################################################### Switching problem stocks IE. Berkshire and BF.B   ######
brk_val <- match('BRK.B',tick)
bfb_val <- match('BF.B',tick)

tick[brk_val] <- "BRK-B"
tick[bfb_val] <- "BF-B"


# STARTING TRACKING ERROR FUNCTION HERE

tracking_error_function <- function(start_date, end_date) {
  
  while (start_date <= end_date) {
    w_dr_filtered_names <- NULL
    if (weekdays(start_date) == 'Sunday' || weekdays(start_date) == 'Saturday') {
      start_date <- start_date + 1
      
    } else {
      
      beta_data <- data.frame(matrix(ncol = length(tick), nrow = 1))  # Creating an empty data frame
      colnames(beta_data) <- tick
      val = 1
      
      for (i in tick) {
        wts = c(1)
        
        price_data <- tq_get(i,
                             from = start_date -183,
                             to = start_date,
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
        
        ################# SP500 Benchmark returns  ###########
        
        bench_price <- tq_get('SPY',
                              from = start_date - 183,
                              to = start_date ,
                              get = 'stock.prices')
        
        bench_ret <- bench_price %>%
          tq_transmute(select = close,
                       mutate_fun = periodReturn,
                       period = "daily",
                       col_rename = "bench_ret")
        ############### Beta Calc
        
        comb_ret <- left_join(port_ret,bench_ret, by = 'date')
        model <- lm(comb_ret$port_ret ~ comb_ret$bench_ret)
        model_beta <- model$coefficients[2]
        beta_data[1,val] <- model_beta
        val <- val + 1
        print(val)
      }
      ############ Filtering stocks based on Beta ##########
      stock_filter <- c()
      for (i in 1:ncol(beta_data)) {
        if ((beta_data[1,i] <= 1.10) & (beta_data[1,i] >= .9 ))  {
          stock_filter <- c(stock_filter, tick[i])
        }
      }
      
      print(stock_filter)
      ########### geting daily price data from beta filtered stocks   #####
      constit_price_data <- NULL
      constit_price_data <- tq_get(stock_filter,
                                   from = start_date -183,
                                   to = start_date,
                                   get = 'stock.prices')
      
      
      consit_ret <- NULL
      constit_ret <- constit_price_data %>%
        group_by(symbol) %>%
        tq_transmute(select = close,
                     mutate_fun = periodReturn,
                     period = 'daily',
                     col_rename = 'ret')
      
      constit_ret_xts <- NULL
      constit_ret_xts <- constit_ret %>%
        spread(symbol, value = ret) %>%
        tk_xts()
    
      ############# SP500 Data Frame
      
      sp500_price_data <- tq_get('SPY',
                                 from = start_date - 183,
                                 to = start_date,
                                 get = 'stock.prices')
      
      
      sp500_ret <- sp500_price_data %>%
        group_by(symbol) %>%
        tq_transmute(select = close,
                     mutate_fun = periodReturn,
                     period = 'daily',
                     col_rename = 'ret')
      
      sp500_ret_xts <- sp500_ret %>%
        spread(symbol, value = ret) %>%
        tk_xts()
      
      ####### dr optimization ######
      w_dr <- spIndexTrack(constit_ret_xts, sp500_ret_xts, lambda = 2e-5, u = 0.5, measure = 'dr')
      cat('Number of assets used:', sum(w_dr > 1e-6))
      w_dr_filtered <- c()
      for (i in 1:length(w_dr)) {
        if (w_dr[i] != 0) {
          w_dr_filtered <- c(w_dr_filtered, w_dr[i])
        }
      }
      w_dr_filtered_names<- c()
      sum_as_vector <- c()
      sum_of_return <- c()
      w_dr_filtered_names <- c(names(w_dr_filtered))
      
      if (weekdays(start_date) == 'Monday') {
        a_price_data <- tq_get(w_dr_filtered_names,
                               from = start_date -3,
                               to = start_date +1,
                               get = 'stock.prices')
        
      } else {
        a_price_data <- tq_get(w_dr_filtered_names,
                               from = start_date -1,
                               to = start_date +1,
                               get = 'stock.prices')
      }
      
      a_ret <- a_price_data %>%
        group_by(symbol) %>%
        tq_transmute(select = close,
                     mutate_fun = periodReturn,
                     period = 'daily',
                     col_rename = 'ret')
      
      a_ret_xts <- a_ret %>%
        spread(symbol, value = ret) %>%
        tk_xts()
      
      for (val in w_dr_filtered_names){
        ret_var <- a_ret_xts[2,val]
        weighted_return <- ret_var * w_dr_filtered[val]
        print(w_dr_filtered[val])
        sum_as_vector <- c(sum_as_vector,weighted_return)
      }
      sum_of_return <- c(sum(sum_as_vector))
      
      print(sum_of_return)
      total_port_return <<- c(total_port_return, sum_of_return)
      
    }
    start_date <- start_date + 1
  }
  
  print(total_port_return)
  
}
x <- as.Date("2020-04-06")
y <- as.Date("2020-04-09")
tracking_error_function(x,y)





