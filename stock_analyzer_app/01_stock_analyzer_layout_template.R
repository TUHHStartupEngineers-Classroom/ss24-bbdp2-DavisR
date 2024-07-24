# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot and commentary


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)
library(quantmod)
library(rvest)
library(glue)

source(file = "00_scripts/stock_analysis_functions.R")
stock_list_tbl <- get_stock_list("DOW")

# UI ----
ui <- fluidPage(title = "Stock Analyzer")



# RUN APP ----
ui <- fluidPage(
  title = "Stock Analyzer",

# 1.0 HEADER ----
div(
  h1("Stock Analyzer"),
  p("This is my second shiny project")
),

    # 2.0 APPLICATION UI -----
    
    div(
      column(width = 4,
             # Stock selection input
             pickerInput(
               inputId = "index_selection", 
               label = "Stock Index", # Adding a label
               choices = c("DAX", "DOW", "NASDAQ", "SP500"), # Add your indices here
               multiple = FALSE, 
               options = pickerOptions(
                 actionsBox = FALSE,
                 liveSearch = TRUE,
                 size = 10
               )
            ),
             # Dynamic stock selection input based on index
             uiOutput("indices"),
            # Date range input
            dateRangeInput(inputId = "date_range",
                           label = "Date Range",
                           start = today() - 180, # Default to last 180 days
                           end = today()),
             # Analyze button below the pickerInput
             actionButton(
               inputId = "analyze",
               label = "Analyze",
               icon = icon("download")
             ),
            hr(), # Horizontal rule
            # Moving average sliders
            sliderInput(inputId = "short_mavg", 
                        label = "Short Moving Average", 
                        min = 5, max = 40, value = 20),
            sliderInput(inputId = "long_mavg", 
                        label = "Long Moving Average", 
                        min = 50, max = 120, value = 50),
             # # Selected symbol output
             # div(
             #   h4("Selected Symbol:"),
             #   textOutput(outputId = "selected_symbol")
             # )
            
      ),
      column(width = 8, 
             # # Stock data output
             # div(
             #   h4(textOutput(outputId = "stock_data_header")),
             #   verbatimTextOutput(outputId = "stock_data")
             # ),
             div(
               h4(textOutput(outputId = "plot_header")),
               plotlyOutput(outputId = "stock_plot")
             )
      )
    ),

    # 3.0 ANALYST COMMENTARY ----
    fluidRow(
      column(width = 12,
             div(
               h3("Analyst Commentary"),
               textOutput(outputId = "commentary")
             )
      )
    )
)
# SERVER ----
server <- function(input, output, session) {
  
  output$indices <- renderUI({
    req(stock_list_tbl())
    choices = stock_list_tbl() %>% purrr::pluck("label")
    pickerInput(
      inputId = "stock_selection", 
      label = "Stock List (Pick One to Analyze)", # Adding a label
      choices = choices, 
      multiple = FALSE, 
      options = pickerOptions(
        actionsBox = FALSE,
        liveSearch = TRUE,
        size = 10
      )
    )
  })
  
  stock_list_tbl <- reactive({
    req(input$index_selection)
    get_stock_list(input$index_selection)
  })
  
  stock_data <- reactive({
    req(stock_symbol())
    req(input$date_range) # Ensure date range is available
    stock_symbol() %>% 
      get_stock_data(from = input$date_range[1], 
                     to   = input$date_range[2],
                     mavg_short = input$short_mavg, 
                     mavg_long  = input$long_mavg)
  })
  
  output$stock_data <- renderPrint({
    req(stock_data())
    stock_data()
  })
  
  # Reactive expression to fetch the selected stock symbol
  stock_symbol <- eventReactive(input$stock_selection, {
    req(input$stock_selection)
    get_symbol_from_user_input(input$stock_selection)
  }, ignoreNULL = FALSE)
  
  # Render the selected stock symbol
  output$selected_symbol <- renderText({
    req(stock_symbol())
    stock_symbol()
  })
  
  # Render the plotly plot
  output$stock_plot <- renderPlotly({ 
    req(stock_data())
    plot_stock_data(stock_data())
  })
  
  # Render the commentary
  output$commentary <- renderText({
    req(stock_data())
    req(input$stock_selection)
    
    input$stock_selection %>%
      get_symbol_from_user_input() %>%
      get_stock_data() %>%
      generate_commentary(user_input = input$stock_selection)
  })
  
  # Render the dynamic stock data header
  output$stock_data_header <- renderText({
    paste(input$stock_selection)
  })
}

# RUN APP ----
shinyApp(ui = ui, server = server)