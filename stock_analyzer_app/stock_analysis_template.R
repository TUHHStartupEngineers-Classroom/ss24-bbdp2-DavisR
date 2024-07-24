# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - DATA ANALYSIS -----

# APPLICATION DESCRIPTION ----
# - The user will select 1 stock from the SP 500 stock index
# - The functionality is designed to pull the past 180 days of stock data by default
# - We will implement 2 moving averages - short (fast) and long (slow)
# - We will produce a timeseries visualization
# - We will produce automated commentary based on the moving averages

# LIBRARIES ----
library(tidyverse)
library(fs)
library(glue)

library(rvest)
library(quantmod)

library(plotly)

# 1.0 GET STOCK LIST ----
get_stock_list <- function(stock_index = "DAX") {
  
  # Control for upper and lower case
  index_lower <- str_to_lower(stock_index)
  # Control if user input is valid
  index_valid <- c("dax", "sp500", "dow", "nasdaq")
  if (!index_lower %in% index_valid) {stop(paste0("x must be a character string in the form of a valid exchange.",
                                                  " The following are valid options:\n",
                                                  stringr::str_c(str_to_upper(index_valid), collapse = ", ")))
  }
  
  # Control for different currencies and different column namings in wiki
  vars <- switch(index_lower,
                 dax    = list(wiki     = "DAX", 
                               columns  = c("Ticker", "Company")),
                 sp500  = list(wiki     = "List_of_S%26P_500_companies", 
                               columns  = c("Symbol", "Security")),
                 dow    = list(wiki     = "Dow_Jones_Industrial_Average",
                               columns  = c("Symbol", "Company")),
                 nasdaq = list(wiki     = "NASDAQ-100",
                               columns  = c("Ticker", "Company"))
  )
  print("vars")
  print(vars)
  print("vars$columns")
  print(vars$columns)
  
  # Extract stock list depending on user input
  read_html(glue("https://en.wikipedia.org/wiki/{vars$wiki}")) %>% 
    
    # Extract table from wiki
    html_nodes(css = "#constituents") %>% 
    html_table() %>% 
    dplyr::first() %>% 
    as_tibble(.name_repair = "minimal") %>% 
    # Select desired columns (different for each article)
    dplyr::select(vars$columns) %>% 
    # Make naming identical
    set_names(c("symbol", "company")) %>% 
    
    # Clean (just relevant for DOW)
    mutate(symbol = str_remove(symbol, "NYSE\\:[[:space:]]")) %>% 
    
    # Sort
    arrange(symbol) %>%
    # Create the label for the dropdown list (Symbol + company name)
    mutate(label = str_c(symbol, company, sep = ", ")) %>%
    dplyr::select(label)
  
}

stock_list_tbl <- get_stock_list()
# get_stock_list("DOW")
# get_stock_list("SP500")

stock_list_tbl

user_input <- "AAPL, Apple Inc."

# Define the function
get_symbol_from_user_input <- function(user_input) {
  user_input %>%
    str_split(",") %>%
    pluck(1, 1) %>%
    str_trim()
}

# Example function calls
t1 <- "ADS.DE, Adidas" %>% get_symbol_from_user_input()
## [1] "ADS.DE"

t2 <- "AAPL, Apple Inc." %>% get_symbol_from_user_input()
## [1] "AAPL"

t1
t2



# Define the currency_format function
currency_format <- function(currency) {
  if (currency == "USD") {
    x <- scales::dollar_format(largest_with_cents = 10)
  } else if (currency == "EUR") {
    x <- scales::dollar_format(prefix = "", suffix = " €",
                               big.mark = ".", decimal.mark = ",",
                               largest_with_cents = 10)
  } else {
    x <- scales::dollar_format()
  }
  return(x)
}




from   <- today() - days(180) 
to     <- today() # or something int this format "2021-01-07"

# Retrieve market data
get_stock_data <- function(stock_symbol, 
                           from = today() - days(180), 
                           to   = today(), 
                           mavg_short = 20, mavg_long = 50) {
  stock_symbol %>%
    quantmod::getSymbols(
      src = "yahoo", 
      from = from, 
      to = to, 
      auto.assign = FALSE
    ) %>%
    timetk::tk_tbl(preserve_index = TRUE, silent = TRUE) %>%
    mutate(currency = case_when(
      str_detect(names(.) %>% last(), "USD") ~ "USD",
      TRUE ~ "Unknown"
    )) %>%
    set_names(c("date", "open", "high", "low", "close", "volume", "adjusted", "currency")) %>%
    drop_na() %>%
    mutate(date = lubridate::ymd(date)) %>%
    mutate(mavg_short = rollmean(adjusted, mavg_short, fill = NA, align = "right")) %>%
    mutate(mavg_long = rollmean(adjusted, mavg_long, fill = NA, align = "right")) %>%
    select(date, adjusted, mavg_short, mavg_long, currency)
}


stock_data <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 8)
stock_data




g <- stock_data %>%
  # convert to long format
  pivot_longer(cols = c("adjusted", "mavg_short", "mavg_long"),
               names_to = "legend",
               values_to = "value",
               names_ptypes = list(legend = factor())) %>%
  
  # ggplot
  ggplot(aes(x = date, y = value, color = legend, group = legend)) +
  geom_line(aes(linetype = legend)) +
  
  # Add theme and colors
  theme_minimal() +
  scale_color_manual(values = c("adjusted" = "black", "mavg_short" = "red", "mavg_long" = "green")) +
  labs(y = "Adjusted Share Price", x = "", title = "Stock Price with Moving Averages") +
  theme(legend.title = element_blank())

ggplotly(g)


# Define the function to plot stock data
plot_stock_data <- function(stock_data) {
  currency <- stock_data %>% pull(currency) %>% first()
  
  g <- stock_data %>%
    # convert to long format
    pivot_longer(cols = c("adjusted", "mavg_short", "mavg_long"),
                 names_to = "legend",
                 values_to = "value",
                 names_ptypes = list(legend = factor())) %>%
    
    # ggplot
    ggplot(aes(x = date, y = value, color = legend, group = legend)) +
    geom_line(aes(linetype = legend)) +
    
    # Add theme and colors
    theme_minimal() +
    scale_color_manual(values = c("adjusted" = "black", "mavg_short" = "red", "mavg_long" = "green")) +
    labs(y = "Adjusted Share Price", x = "", title = "Stock Price with Moving Averages") +
    theme(legend.title = element_blank()) +
    scale_y_continuous(labels = currency_format(currency))
  
  ggplotly(g)
}


# stock_data <- get_stock_data("AAPL", from = "2020-06-01", to = "2021-01-12", mavg_short = 5, mavg_long = 8)
# plot_stock_data(stock_data)

"ADS.DE" %>% 
  get_stock_data() %>%
  plot_stock_data()

warning_signal <- stock_data %>%
  tail(1) %>% # Get last value
  mutate(mavg_warning_flag = mavg_short < mavg_long) %>% # insert the logical expression
  pull(mavg_warning_flag)



generate_commentary <- function(stock_data, user_input) {
  # Generate the warning signal
  warning_signal <- stock_data %>%
    tail(1) %>% # Get last value
    mutate(mavg_warning_flag = mavg_short < mavg_long) %>% # Logical expression
    pull(mavg_warning_flag)
  
  n_short <- stock_data %>% pull(mavg_short) %>% is.na() %>% sum() + 1
  n_long <- stock_data %>% pull(mavg_long) %>% is.na() %>% sum() + 1
  
  if (warning_signal) {
    str_glue("In reviewing the stock prices of {user_input} has {n_long} data points for the long moving average. The short-term moving average is below the long-term moving average, indicating a negative trend.")
  } else {
    str_glue("In reviewing the stock prices of {user_input} has {n_short} data points for the short moving average. The short-term moving average is above the long-term moving average, indicating a positive trend.")
  }
}

generate_commentary(stock_data, user_input = user_input)

"ADS.DE, Adidas" %>% 
  get_symbol_from_user_input() %>%
  get_stock_data(from = from, to = to ) %>%
  # plot_stock_data() %>%
  generate_commentary(user_input = "ADS.DE, Adidas")


fs::dir_create("00_scripts") #create folder

# write functions to an R file
dump(
  list = c("get_stock_list", "get_symbol_from_user_input", "get_stock_data", "plot_stock_data", "currency_format", "generate_commentary"),
  file = "00_scripts/stock_analysis_functions.R", 
  append = FALSE) # Override existing 

# 2.0 EXTRACT SYMBOL BASED ON USER INPUT ----



# 3.0 GET STOCK DATA ----



# 4.0 PLOT STOCK DATA ----



# 5.0 GENERATE COMMENTARY ----



# 6.0 TEST WORKFLOW ----



# 7.0 SAVE SCRIPTS ----

