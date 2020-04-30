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


################################## Function Inits ######################################################################################################  

dr_optimization_function <- function(constit_ret_xts, sp500_ret_xts) {
w_dr <- spIndexTrack(constit_ret_xts, sp500_ret_xts, lambda = 2e-8, u = 0.5, measure = 'dr')
cat('Number of assets used:', sum(w_dr > 1e-6)) # prints number of assets used
w_dr_filtered <- c()

## Filters out zero weights ##
for (i in 1:length(w_dr)) {
  if (w_dr[i] != 0) {
    w_dr_filtered <- c(w_dr_filtered, w_dr[i])
    }
}
w_dr_filtered_names<-c(names(w_dr_filtered))
summative_list <- list(w_dr_filtered, w_dr_filtered_names)

return(summative_list)
}


hdr_optimization_function <- function(constit_ret_xts, sp500_ret_xts) {
  w_hdr <- spIndexTrack(constit_ret_xts, sp500_ret_xts, lambda = 2e-8, u = 8, measure = 'hdr', hub = 0.05)
  cat('Number of assets used:', sum(w_hdr > 1e-6)) # prints number of assets used
  w_hdr_filtered <- c()
  
  ## Filters out zero weights ##
  for (i in 1:length(w_hdr)) {
    if (w_hdr[i] != 0) {
      w_hdr_filtered <- c(w_hdr_filtered, w_hdr[i])
    }
  }
  w_hdr_filtered_names<-c(names(w_hdr_filtered))
  summative_list <- list(w_hdr_filtered, w_hdr_filtered_names)
  
  return(summative_list)
}

hete_optimization_function <- function(constit_ret_xts, sp500_ret_xts) {
  w_hete <- spIndexTrack(constit_ret_xts, sp500_ret_xts, lambda = 2e-8, u = 0.5, measure = 'hete', hub = .05)
  cat('Number of assets used:', sum(w_hete > 1e-6)) # prints number of assets used
  w_hete_filtered <- c()
  
  ## Filters out zero weights ##
  for (i in 1:length(w_hete)) {
    if (w_hete[i] != 0) {
      w_hete_filtered <- c(w_hete_filtered, w_hete[i])
    }
  }
  w_hete_filtered_names<-c(names(w_hete_filtered))
  summative_list <- list(w_hete_filtered, w_hete_filtered_names)
  
  return(summative_list)
}

ete_optimization_function <- function(constit_ret_xts, sp500_ret_xts) {
  w_ete <- spIndexTrack(constit_ret_xts, sp500_ret_xts, lambda = 2e-8, u = 0.5, measure = 'ete')
  cat('Number of assets used:', sum(w_ete > 1e-6)) # prints number of assets used
  w_ete_filtered <- c()
  
  ## Filters out zero weights ##
  for (i in 1:length(w_ete)) {
    if (w_ete[i] != 0) {
      w_ete_filtered <- c(w_ete_filtered, w_ete[i])
    }
  }
  w_ete_filtered_names<-c(names(w_ete_filtered))
  summative_list <- list(w_ete_filtered, w_ete_filtered_names)
  
  return(summative_list)
}

get_optimized_assetdata <- function(filtered_ticker_vector, start_date) {

if (weekdays(start_date) == 'Monday') {
  a_price_data <- tq_get(filtered_ticker_vector,
                         from = start_date -3,
                         to = start_date +1,
                         get = 'stock.prices')
  
} else {
  a_price_data <- tq_get(filtered_ticker_vector,
                         from = start_date -1,
                         to = start_date +1,
                         get = 'stock.prices')
  
}

a_ret <- a_price_data %>%      # Portfolio Data
  group_by(symbol) %>%
  tq_transmute(select = close,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret')

a_ret_xts <- a_ret %>%
  spread(symbol, value = ret) %>%
  tk_xts()
return(a_ret_xts)
}

daily_port_return_function <- function(filtered_weights_vector, filtered_names_vector, xts_object)  {
sum_as_vector <- c()
for (val in filtered_names_vector){
  ret_var <- xts_object[2,val]
  weighted_return <- ret_var * filtered_weights_vector[val]
  sum_as_vector <- c(sum_as_vector,weighted_return)
}
return(sum_as_vector)
}


retreving_vector_names <- function(lst1) {
  vec_names <- c()
  for (i in lst1[2]) {
    vec_names <- c(vec_names, i)
  }
  return(vec_names)
}

retreving_vec_weights <- function(lst1) {
  vec_one <- c()
  for(i in lst1[1]) {
    vec_one <-c(vec_one, i)
  }
  return(vec_one)
}

p_analytics_error_func <- function(port_xts, sp500_xts) {
  port_track_error <- TrackingError(port_xts, sp500_xts, scale = 252)
  return(port_track_error)
}

ex_ret_func <- function(daily_return_xts, sp500_daily_xts) {
  exret <- sum(daily_return_xts[,1]) - sum(sp500_daily_xts[,1])
  return(exret)
}

##########################################################  End of Function Inits   ###################################################################################

#####################################################   Getting SP500 ticker list   ###############################################
sp500<-GetSP500Stocks()[,c("Tickers")]
tick = c(sp500)

##################################################### Switching problem stocks IE. Berkshire and BF.B   ###########################################
brk_val <- match('BRK.B',tick)
bfb_val <- match('BF.B',tick)

tick[brk_val] <- "BRK-B"
tick[bfb_val] <- "BF-B"


# STARTING TRACKING ERROR FUNCTION HERE

tracking_error_function <- function(start_date, end_date) {
  
  ############# GETTING SP500 XTS FOR TRACKING ERROR FUNCTION ##########################
  
  if (weekdays(start_date) == 'Monday') {
    sp2 <- tq_get('SPY',
                  from = start_date-3,
                  to = end_date +1,
                  get = 'stock.prices')
    
  } else {
    sp2 <- tq_get('SPY',
                  from = start_date-1,
                  to = end_date +1,
                  get = 'stock.prices')
  }
  
  sp2_ret <- sp2 %>%
    group_by(symbol) %>%
    tq_transmute(select = close,
                 mutate_fun = periodReturn,
                 period = 'daily',
                 col_rename = 'ret')
  
  sp2_ret_xts <- sp2_ret %>%
    spread(symbol, value = ret) %>%
    tk_xts()
  
  main_sp500_xts <<- sp2_ret_xts[2:nrow(sp2_ret_xts)]
  
  #### Variable Inits ####
  date_vector <- c()
  total_dr_port_return <- c()
  total_hete_port_return <- c()
  total_ete_port_return <- c()
  total_hdr_port_return <- c()
  
  while (start_date <= end_date) {
    
    other_date <- c(as.Date(start_date))
    date_vector <- c(other_date, date_vector)  
    ## FILTERING OUT WEEKDAYS VS WEEKENDS 
    
    if (weekdays(start_date) == 'Sunday' || weekdays(start_date) == 'Saturday') {
      start_date <- start_date + 1
      
    } else {
############################################### BETA FILTER STARTS HERE #######################################################################################3
      beta_data <- data.frame(matrix(ncol = length(tick), nrow = 1))  
      colnames(beta_data) <- tick
      val = 1
      
      for (i in tick) {
        wts = c(1)
        price_data <- tq_get(i,
                             from = start_date -60,
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
        
        ################# SP500 Benchmark returns  #############
        
        bench_price <- tq_get('SPY',
                              from = start_date - 60,
                              to = start_date ,
                              get = 'stock.prices')
        
        bench_ret <- bench_price %>%
          tq_transmute(select = close,
                       mutate_fun = periodReturn,
                       period = "daily",
                       col_rename = "bench_ret")
        ############### Beta Calc ##########################
        
        comb_ret <- left_join(port_ret,bench_ret, by = 'date')
        model <- lm(comb_ret$port_ret ~ comb_ret$bench_ret)
        model_beta <- model$coefficients[2]
        beta_data[1,val] <- model_beta
        val <- val + 1
        print(val)
      }
###################### Filtering stocks based on Beta ###################################################################
      stock_filter <- c()
      for (i in 1:ncol(beta_data)) {
        if ((beta_data[1,i] <= 1.10) & (beta_data[1,i] >= .9 ))  {
          stock_filter <- c(stock_filter, tick[i])
        }
      }
      
##################### geting daily price data from beta filtered stocks   ###############################################
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
      
####################### SP500 Data Frame for Portfolio Optimization Function #########################################################
      
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
      
      #####################  Tracking Error Optimizations  ###########
      dr_func_return <- dr_optimization_function(constit_ret_xts, sp500_ret_xts)
      hete_func_return <- hete_optimization_function(constit_ret_xts, sp500_ret_xts)
      ete_func_return <- ete_optimization_function(constit_ret_xts, sp500_ret_xts)
      hdr_func_return <- hdr_optimization_function(constit_ret_xts, sp500_ret_xts)
    
      ################## Getting Tick Names As A Vector #################
      dr_tick_names <- retreving_vector_names(dr_func_return)
      hete_tick_names <- retreving_vector_names(hete_func_return)
      ete_tick_names <- retreving_vector_names(ete_func_return)
      hdr_tick_names <- retreving_vector_names(hdr_func_return)
      
      ################# Getting Weights As A Vector #####################
      dr_weights <- retreving_vec_weights(dr_func_return)
      hete_weights<- retreving_vec_weights(hete_func_return)
      ete_weights<- retreving_vec_weights(ete_func_return)
      hdr_weights <- retreving_vec_weights(hdr_func_return)
    
      #################### Getting Data for Perfromance Analytics tracking error function  ###############
      dr_ret_xts <- get_optimized_assetdata(dr_tick_names, start_date)
      hete_ret_xts <- get_optimized_assetdata(hete_tick_names, start_date)
      ete_ret_xts <- get_optimized_assetdata(ete_tick_names, start_date)
      hdr_ret_xts <- get_optimized_assetdata(hdr_tick_names, start_date)
      
      #######################################  LOOPING THROUGH TO CALCULATE PORTFOLIO RETURN   #######################################################  

      dr_daily_port_return <- daily_port_return_function(dr_weights, dr_tick_names, dr_ret_xts)
      hete_daily_port_return <- daily_port_return_function(hete_weights, hete_tick_names, hete_ret_xts)
      ete_daily_port_return <- daily_port_return_function(ete_weights, ete_tick_names, ete_ret_xts)
      hdr_daily_port_return <- daily_port_return_function(hdr_weights, hdr_tick_names, hdr_ret_xts)
      #
      
      ################### Summing the total_return of the portfolio ###########################
      dr_sum_of_return <- c(sum(dr_daily_port_return))
      hete_sum_of_return <- c(sum(hete_daily_port_return))
      ete_sum_of_return <- c(sum(ete_daily_port_return))
      hdr_sum_of_return <-c(sum(hdr_daily_port_return))      
      
      ################### Appending total daily portfolio return to a vector ######################################
      total_dr_port_return <- c(total_dr_port_return, dr_sum_of_return)
      total_hdr_port_return <- c(total_hdr_port_return, hdr_sum_of_return)
      total_ete_port_return <- c(total_ete_port_return, ete_sum_of_return)
      total_hete_port_return <- c(total_hete_port_return, hete_sum_of_return)
    }
    ########### END OF WHILE LOOP ################################################################## 
    start_date <- start_date + 1    
  }
  date_vec_2 <- rev.default(date_vector)   ## reversing vector of dates for XTS Compatability
  
  ######################### Creating XTS of portfolio returns for calculation of P.A Tracking Error ##########################
  dr_xts <<- xts(total_dr_port_return, date_vec_2)
  hdr_xts <<- xts(total_hdr_port_return, date_vec_2)
  ete_xts <<- xts(total_ete_port_return, date_vec_2)
  hete_xts <<- xts(total_hete_port_return, date_vec_2)
  
  ###################### Getting the tracking error from P.A ############################################  
  terror_of_hdr <- p_analytics_error_func(hdr_xts, main_sp500_xts)
  terror_of_dr  <-  p_analytics_error_func(dr_xts, main_sp500_xts)
  terror_of_ete <- p_analytics_error_func(ete_xts, main_sp500_xts)
  terror_of_hete <-  p_analytics_error_func(hete_xts, main_sp500_xts)
  
  ###################### Printing the tracking error #########################################
  
  cat("HDR Tracking Error: " , terror_of_hdr, "\n")
  cat("DR Tracking Error: ", terror_of_dr, "\n")
  cat("ETE Tracking Error: ", terror_of_ete, "\n")
  cat("HETE Tracking Error: ", terror_of_hete, "\n")
  
}
x <- as.Date("2020-04-06")
y <- as.Date("2020-04-10")
tracking_error_function(x,y)


dr_ex_return <- ex_ret_func(dr_xts, main_sp500_xts)
ete_ex_return <- ex_ret_func(ete_xts, main_sp500_xts)
hete_ex_return <- ex_ret_func(hete_xts, main_sp500_xts)
hdr_ex_return <- ex_ret_func(hdr_xts, main_sp500_xts)

cat("The DR excess return from ", as.character(start_date), " to " , as.character(end_date), " is: " , dr_ex_return, '\n')
cat("The ETE excess return from ", as.character(start_date), " to " , as.character(end_date), " is: " , ete_ex_return, '\n')
cat("The HETE excess return from ", as.character(start_date), " to " , as.character(end_date), " is: " , hete_ex_return, '\n')
cat("The HDR excess return from ",as.character(start_date), " to " , as.character(end_date), " is: " , hdr_ex_return, '\n')





