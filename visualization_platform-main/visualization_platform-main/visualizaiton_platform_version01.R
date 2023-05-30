library(shiny)
library(dplyr)
#install.packages('shinydashboard')
library(shinydashboard)
library(gridExtra)
library(grid)
library(ggplot2)
library(flexdashboard)
library(shiny)
#install.packages("devtools")
#devtools::install_github("dreamRs/shinyWidgets")
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(plotly)
library(parsnip)
library(timetk)
library(xgboost)
library(ggthemes)
# install.packages('highcharter')
library(highcharter)
# install.packages('fmsb')
library(fmsb)
# install.packages("Rcpp")
library(Rcpp)
# devtools::document(pkg = paste0("own_package/", 
#                            "PredictFutureOrder"))
# devtools::check(paste0("own_package/", 
#                 "PredictFutureOrder"))  #check whether there's problem
# devtools::install(pkg = paste0("own_package/", 
#                              "PredictFutureOrder"), 
#                 reload = TRUE)

library(PredictFutureOrder)
useShinyjs(rmd = TRUE)
#setwd('C:\\Users\\yangxinchen\\Desktop\\yxcgit\\visualization_platform')
#setwd('D:/R-programing/visualization_platform')
#getwd()
# Load functions
source('function/xgboost_forcast_coustomer.R')
########################Data processing for page 1 ##########################
user_behavior <- read.csv("data/user_behavior.csv")
user_activity_daily <- function(data){
  activity <- data %>%
    group_by(date) %>%
    summarize(UV = n_distinct(user_id), PV = n())
  return (activity)
}
Daily <- user_activity_daily(user_behavior)
Daily$date <- as.Date.factor(Daily$date)

average_pv1 <- function(data){
  average1 <-
    data %>%
    group_by(date) %>%
    summarize(UV = n_distinct(user_id), PV = n()) %>%
    mutate(PVmeandaily = PV/UV)
  return(average1)
}
average_pv2 <- function(data){
  average2 <-
    data %>%
    group_by(hour) %>%
    summarize(UV = n_distinct(user_id), PV = n()) %>%
    mutate(PVmeanhourly = PV/UV)
  return(average2)
}
Average1 <- average_pv1(user_behavior)
Average1$date <- as.Date.factor(Average1$date)
Average1$Average_date <- Average1$date
Average2 <- average_pv2(user_behavior)
Average2$Average_hour <- Average2$hour

min_date1 <- min(Daily$date[!is.na(Daily$date)])
max_date2 <- max(Daily$date[!is.na(Daily$date)])
min_date_average <- min(Average1$Average_date[!is.na(Average1$Average_date)])
max_date_average <- max(Average1$Average_date[!is.na(Average1$Average_date)])

########################Data processing for page 2 ##########################
orders <- read.csv("data/orders.csv")
user_order <- function(data){
  user <-  data %>%
    group_by(date) %>%
    summarise(distinct_users = n_distinct(user_id),
              total_products = n(),
              total_price = sum(product_price))
  return(user)
}
User <- user_order(orders)
User$date <- as.Date.factor(User$date)
User$date1 <- User$date
min_date3 <- min(User$date1[!is.na(User$date)])
max_date4 <- max(User$date1[!is.na(User$date)])


########################Data processing for page 3 ##########################
user_behavior_data <- read_csv('data/user_behavior.csv') 
setClass("UserBehavior", slots = list(
  behavior_id = "numeric",
  user_id = "numeric",
  product_id = "numeric",
  categoryId = "numeric",
  behavior_type = "character",
  create_time = "numeric",
  date = "Date",
  year = "numeric",
  month = "numeric",
  day = "numeric",
  hour = "numeric"
))
pv_data <- user_behavior_data[user_behavior_data$behavior_type=='pv',]
pv_data_object <- new("UserBehavior",
                      behavior_id = pv_data$behavior_id,
                      user_id = pv_data$user_id,
                      product_id = pv_data$product_id,
                      categoryId = pv_data$categoryId,
                      behavior_type = pv_data$behavior_type,
                      create_time = pv_data$create_time,
                      date = pv_data$date,
                      year = pv_data$year,
                      month = pv_data$month,
                      day = pv_data$day,
                      hour = pv_data$hour
)
cf_data <- user_behavior_data[user_behavior_data$behavior_type=='cart'|user_behavior_data$behavior_type=='fav',]
cf_data_object <- new("UserBehavior",
                      behavior_id = cf_data$behavior_id,
                      user_id = cf_data$user_id,
                      product_id = cf_data$product_id,
                      categoryId = cf_data$categoryId,
                      behavior_type = cf_data$behavior_type,
                      create_time = cf_data$create_time,
                      date = cf_data$date,
                      year = cf_data$year,
                      month = cf_data$month,
                      day = cf_data$day,
                      hour = cf_data$hour
)
buy_data <- user_behavior_data[user_behavior_data$behavior_type=='buy',]
buy_data_object <- new("UserBehavior",
                      behavior_id = buy_data$behavior_id,
                      user_id = buy_data$user_id,
                      product_id = buy_data$product_id,
                      categoryId = buy_data$categoryId,
                      behavior_type = buy_data$behavior_type,
                      create_time = buy_data$create_time,
                      date = buy_data$date,
                      year = buy_data$year,
                      month = buy_data$month,
                      day = buy_data$day,
                      hour = buy_data$hour
)
merged_data <- rbind(pv_data, cf_data, buy_data)


########################Data processing for page 4 ##########################
order_data <- read_csv('data/orders.csv') 

new_order_data <- order_data %>%
  group_by(user_id) %>%
  summarize(min_day = min(18 - day),
            order_count = n(),
            total_price = sum(product_price))

# Create data frame
k_mean_data <- data.frame(user_id = new_order_data$user_id, recency = new_order_data$min_day, frequency = new_order_data$order_count, monetary = new_order_data$total_price)
# cppFunction('
#   // Function to create k_mean_data data frame
#   DataFrame createKMeanData(DataFrame new_order_data) {
#     // Extract columns from new_order_data
#     IntegerVector user_id = new_order_data["user_id"];
#     NumericVector recency = new_order_data["min_day"];
#     NumericVector frequency = new_order_data["order_count"];
#     NumericVector monetary = new_order_data["total_price"];
#     
#     // Create k_mean_data data frame
#     DataFrame k_mean_data = DataFrame::create(
#       _["user_id"] = user_id,
#       _["recency"] = recency,
#       _["frequency"] = frequency,
#       _["monetary"] = monetary
#     );
#     
#     // Return the k_mean_data data frame
#     return k_mean_data;
#   }
# ')
# 
# # Use the C++ function to create k_mean_data data frame
# k_mean_data <- createKMeanData(new_order_data)





########################Data processing for page 5 and page 6 ##########################
# Load data
#setwd('D:/R-programing/visualization_platform')
sales_data_raw <- read_csv('data/orders02.csv',show_col_types = FALSE) 

# Select relevant data
processed_data_tbl <- sales_data_raw %>% 
  select(ORDERDATE, ORDERNUMBER, ORDERLINENUMBER, COUNTRY, SALES, PRODUCTLINE, DEALSIZE, STATUS, CUSTOMERNAME)

# Preprocessing
processed_data_tbl <- processed_data_tbl %>%
  mutate(ORDERDATE = mdy_hm(ORDERDATE),
         ORDERDATE = as_datetime(ORDERDATE))

# Manual edits
processed_data_tbl$COUNTRY[processed_data_tbl$COUNTRY=="UK"] <- "United Kingdom"
processed_data_tbl$COUNTRY[processed_data_tbl$COUNTRY=="USA"] <- "United States"

########################Data processing for page 6 ##########################
vip_num=row_number(processed_data_tbl)[1:800]
lost_num=row_number(processed_data_tbl)[801:1200]
processed_data_tbl$classification='Retain_Customers'
processed_data_tbl$classification[vip_num]='Important_Vip'
processed_data_tbl$classification[lost_num]='Lost_Customers'

############################## ui #########################
ui <- dashboardPage(
  dashboardHeader(title = "Customer Dashboard"),
  dashboardSidebar(
    sidebarMenu( width = 160,
      id = "sidebar",
      menuItem("Customer Activity Analysis-HJ", tabName = "page1"),
      menuItem("Customer Order Analysis-HJ", tabName = "page2"),
      menuItem("Conversion Rate-YXC", tabName = "page3"),
      menuItem("Consumer Segmentation-YXC", tabName = "page4"),
      menuItem("Future Order Forecasting-WT", tabName = "page5"),
      menuItem("Customer Forecasting-WT", tabName = "page6")
    )
  ),
  dashboardBody(
    tabItems(
      # Page 1
      tabItem(
        tabName = "page1",
        sidebarLayout(
          sidebarPanel(
            shinyWidgets::airDatepickerInput(
              inputId = "date",
              label = h4("Date Interval"),
              value = c(min_date1,
                        max_date2),
              separator = " to ",
              range = TRUE,
              minDate  = min_date1,
              maxDate = max_date2,
              dateFormat = "yyyy-MM-dd",
              autoClose = TRUE,
              clearButton = TRUE,
              width = "100%",
              addon = "none"
            ),
            sliderInput("hour", HTML("Hour Interval:"),
                        min = 0, max = 24, value = c(0, 24), step = 1),
            sliderInput("Average_date", HTML("Average_date Interval:"),
                        min = min_date_average, 
                        max = max_date_average, 
                        value = c(min_date_average, max_date_average)),
            sliderInput("Average_hour", HTML("Average_hour Interval:"),
                        min = 0, max = 24, value = c(0, 24), step = 1)
            # Add sidebar content for page 1 here
            
          ),
          mainPanel(
            h2("Page 1"),
            plotOutput("plot_user_activity_daily"),
            plotOutput("plot_user_activity_hour"),
            plotOutput("plot_average1"),
            plotOutput("plot_average2"),
            # Add main panel content for page 1 here
            textOutput("page1_output")
          )
        )
      ),
      # Page 2
      tabItem(
        tabName = "page2",
        sidebarLayout(
          sidebarPanel(
            sliderInput("date1", HTML("Date Interval:"),
                        min = min_date3, 
                        max = max_date4, 
                        value = c(min_date3, max_date4))
            # Add sidebar content for page 2 here
            
          ),
          mainPanel(
            h2("Page 2"),
            plotOutput("plot_user_order"),
            plotOutput("plot_hot_product"),
            # Add main panel content for page 2 here
            textOutput("page2_output")
          )
        )
      ),
      # Page 3
      tabItem(
        tabName = "page3",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page3_filter", "Page 3 Filter", choices = c("all", "individual"), selected = "all")
          ),
          mainPanel(
            h2("Conversion rate"),
            # # Add main panel content for page 1 here
            # highchartOutput("funnel_chart")
            # Conditional rendering of the funnel chart based on the selected option
            conditionalPanel(
              condition = "input.page3_filter == 'all'",
              highchartOutput("funnel_chart")
            ),
            conditionalPanel(
              condition = "input.page3_filter == 'individual'",
              highchartOutput("funnel_chart02")
            )
          )
        )
      ),
      ## tab 4
      tabItem(
        tabName = "page4",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            selectInput("page4_filter", "Graph type", choices = c("radar", "chart"), selected = "radar")
          ),
          mainPanel(
            h2("Rader-Buyer Group"),
            # # Add main panel content for page 1 here
            conditionalPanel(
              condition = "input.page4_filter == 'radar'",
              plotlyOutput("segmentation_plot")
            ),
            conditionalPanel(
              condition = "input.page4_filter == 'chart'",
              plotlyOutput("column_chart")
            )
          )
        )
      ),
      # Page 5

      tabItem(
        tabName = "page5",
        sidebarLayout(
          sidebarPanel(
            # Add sidebar content for page 1 here
            shinyWidgets::airDatepickerInput(
              inputId = "date_range",
              label = h4("Date"),
              value = c(min(processed_data_tbl$ORDERDATE),
                        max(processed_data_tbl$ORDERDATE)),
              separator = " to ",
              range = TRUE,
              minDate  = min(processed_data_tbl$ORDERDATE),
              maxDate = max(processed_data_tbl$ORDERDATE),
              dateFormat = "mm-dd-yyyy",
              autoClose = TRUE,
              clearButton = TRUE,
              width = "100%",
              addon = "none"
          ),
          shinyWidgets::pickerInput(
            inputId  = "picker_country",
            label    = h4("Country"),
            choices  = sort(unique(processed_data_tbl$COUNTRY)),
            selected = unique(processed_data_tbl$COUNTRY),
            multiple = TRUE, # Allow multiple options
            options = list(
              `actions-box` = TRUE,  # Note back ticks
              size = 10,
              `selected-text-format` = "count > 3"
          )),
          shinyWidgets::radioGroupButtons(
            inputId   = "time_unit", # Create name 
            #label     = "Time Unit", # What is shown
            choices   = c("Day"="day","Week"="week","Month"="month","Qtr"="quarter","Year"="year"), # The options shown
            selected  = "month", # Default selection
            status    = "info", # Set color
            justified = TRUE
            ),
          h4("Forecast"),
          shinyWidgets::switchInput(inputId = "forecast_mode",
                                    handleWidth = 100,
                                    labelWidth = 100,
                                    inline = TRUE,
                                    value = FALSE,
                                    onStatus = "info",
                                    onLabel = "On",
                                    offLabel = "Off",
                                    width = "200px"),
          
          conditionalPanel(condition = "input.forecast_mode == 1",
                           numericInput(inputId = "n_future",
                                        label = "Forecast Horizon",
                                        value = 12,
                                        min = 1  # At least 1 period in the future
                           )),
          # APPLY BUTTONS -----
          actionButton(inputId = "apply", 
                       label   = "Apply Forcast", 
                       icon    = icon("play"),
                       width   = '50%')
          ),
          mainPanel(
            h2("Forcasting Future Order"),
  
            # Add main panel content for page 1 here
            plotlyOutput(outputId = "page5_output"),
            
            h2("Acutal and Forcast Order Info"),
            
            DT::dataTableOutput(outputId="page5_output02")
            
          )
        )
      ),
      # Page 6
      tabItem(
        tabName = "page6",
        sidebarLayout(
          sidebarPanel(
            shinyWidgets::airDatepickerInput(
              inputId = "date_range02",
              label = h4("Date"),
              value = c(min(processed_data_tbl$ORDERDATE),
                        max(processed_data_tbl$ORDERDATE)),
              separator = " to ",
              range = TRUE,
              minDate  = min(processed_data_tbl$ORDERDATE),
              maxDate = max(processed_data_tbl$ORDERDATE),
              dateFormat = "mm-dd-yyyy",
              autoClose = TRUE,
              clearButton = TRUE,
              width = "100%",
              addon = "none"
            ),
            shinyWidgets::pickerInput(
              inputId  = "picker_customer",
              label    = h4("Customer (at least 1 customers)"),
              choices  = unique(processed_data_tbl$CUSTOMERNAME) %>% sort(),
              selected = unique(processed_data_tbl$CUSTOMERNAME) %>% sort(),
              multiple = TRUE, # Allow multiple options
              options  = list(
                `actions-box` = TRUE,  # Note back ticks
                size = 10,
                `selected-text-format` = "count > 1"
              )),
            shinyWidgets::radioGroupButtons(
              inputId   = "time_unit02", # Create name 
              #label     = "Time Unit", # What is shown
              choices   = c("Day"="day","Week"="week","Month"="month","Qtr"="quarter","Year"="year"), # The options shown
              selected  = "month", # Default selection
              status    = "info", # Set color
              justified = TRUE
            ),
            h4("Forecast"),
            shinyWidgets::switchInput(inputId = "forecast_mode02",
                                      handleWidth = 100,
                                      labelWidth = 100,
                                      inline = TRUE,
                                      value = FALSE,
                                      onStatus = "info",
                                      onLabel = "On",
                                      offLabel = "Off",
                                      width = "200px"),
            
            conditionalPanel(condition = "input.forecast_mode02 == 1",
                             numericInput(inputId = "n_future02",
                                          label = "Forecast Horizon",
                                          value = 12,
                                          min = 1  # At least 1 period in the future
                             )),
            # APPLY BUTTONS -----
            actionButton(inputId = "apply02", 
                         label   = "Apply Forcast", 
                         icon    = icon("play"),
                         width   = '50%')
          ),
          mainPanel(
            h2("Forcasting Customer"),
            
            # Add main panel content for page 1 here
            plotlyOutput(outputId = "page6_output"),
            
            h2("Acutal and Forcast Customer Info"),
            
            DT::dataTableOutput(outputId="page6_output02")
            
          )
        )
      )
    )
  )
)

######################### server #########################

server <- function(input, output) {
  # Server logic goes here
  
  output$page1_output <- renderText({
    paste("Selected option in Page 1:", input$page1_filter)
  })
  
  output$page2_output <- renderText({
    paste("Selected option in Page 2:", input$page2_filter)
  })
  
 
  ######## Reactive event for page1 #########################
  data_filtered1 <- reactive({Daily %>%
      filter(
        between(date, input$date[1], input$date[2]))
  })
  plot_user_activity_daily <- function(data){
    A <- data_filtered1()
    if(colnames(A)[2] == "UV"){
      par(mfrow = c(1,2)) 
      barplot(A$UV,
              main = "Daily Activity Chart-UV",
              names.arg = A$date,
              cex.names = 0.65,
              ylab = "Number",
              col = "orange",
              las = 2
      )
      legend("topright", "UV", cex = 0.65, fill = "orange")
    } else {
      "Please run user_activity_daily to generate UV"
    }
    if(colnames(A)[3] == "PV"){
      barplot(A$PV,
              main = "Daily Activity Chart-PV",
              names.arg = A$date,
              cex.names = 0.65,
              ylab = "Number",
              col = "orange",
              las = 2
      )
      legend("topright", "PV", cex = 0.65, fill = "orange")
    } else {
      "Please run user_activity_daily to generate PV"
    }
  }
  output$plot_user_activity_daily <- renderPlot(plot_user_activity_daily(data_filtered1()))
  
  user_activity_hour <- function(data){
    hour <- data %>%
      group_by(hour) %>%
      summarize(UV = n_distinct(user_id), PV = n())
    return (hour)
  }
  Hourly <- user_activity_hour(user_behavior)
  data_filtered2 <- reactive({Hourly %>%
      filter(
        between(hour, input$hour[1], input$hour[2]))
  })
  plot_user_activity_hour <- function(data){
    B <- data_filtered2()
    if(colnames(B)[2] == "UV"){
      par(mfrow = c(1,2)) 
      barplot(B$UV,
              main = "Hourly Activity Chart-UV",
              names.arg = B$hour,
              cex.names = 0.65,
              xlab = "Date",
              ylab = "Number",
              col = "green",
              las = 1
      )
      legend("topright", "UV", cex = 0.65, fill = "green")
    } else {
      "Please run user_activity_hour to generate UV"
    } 
    if(colnames(B)[3] == "PV"){
      barplot(B$PV,
              main = "Hourly Activity Chart-PV",
              names.arg = B$hour,
              cex.names = 0.65,
              xlab = "Date",
              ylab = "Number",
              col = "green",
              las = 1
      )
      legend("topright", "PV", cex = 0.65, fill = "green")
    } else {
      "Please run user_activity_hour to generate PV"
    }
  }
  output$plot_user_activity_hour <- renderPlot(plot_user_activity_hour(data_filtered2()))
  
  data_filtered3 <- reactive({Average1 %>%
      filter(
        between(Average_date, input$Average_date[1], input$Average_date[2]))
  })
  
  plot_average1 <- function(data){
    C <- data_filtered3()
    barplot(C$PVmeandaily,
            main = "Daily PV Per Capita",
            names.arg = C$date,
            cex.names = 0.65,
            ylab = "Number",
            col = "blue",
            las = 2
    )
    
  }
  output$plot_average1 <- renderPlot(plot_average1(date_filtered3()))
  
  data_filtered4 <- reactive({Average2 %>%
      filter(
        between(Average_hour, input$Average_hour[1], input$Average_hour[2]))
  })
  plot_average2 <- function(data){
    D <- data_filtered4()
    barplot(D$PVmeanhourly,
            main = "Hourly PV Per Capita",
            names.arg = D$hour,
            cex.names = 0.65,
            ylab = "Number",
            col = "blue",
            las = 1
    )
    
  }
  output$plot_average2 <- renderPlot(plot_average2(date_filtered4()))
  
  ######## Reactive event for page2 #########################
  data_filtered5 <- reactive({User %>%
      filter(
        between(date, input$date1[1], input$date1[2]))
  })
  
  plot_user_order <- function(data){
    D <- data_filtered5()
    if(colnames(D)[2] == "distinct_users"){
      par(mfrow = c(1,3))
      barplot(D$distinct_users,
              main = "Order-Buyers",
              names.arg = D$date,
              cex.names = 0.8,
              ylab = "Number",
              col = "orange",
              las = 2
      )
      legend("topright", "Buyers", cex = 0.8, fill = "orange")
    } else {
      "Please run user_order to generate distinct_users"
    }
    if(colnames(D)[3]== "total_products"){
      barplot(D$total_products,
              main = "Order-Orders",
              names.arg = D$date,
              cex.names = 0.8,
              col = "green",
              las = 2
      )
      legend("topright", "Orders", cex = 0.8, fill = "green")
    } else {
      "Please run user_order to generate total_products"
    }
    if(colnames(D)[4]== "total_price"){
      barplot(D$total_price,
              main = "Order-GMV",
              names.arg = D$date,
              cex.names = 0.8,
              col = "blue",
              las = 2
      )
      legend("topright", "GMV", cex = 0.8, fill = "blue")
    } else {
      "Please run user_order to generate total_price"
    }
  }
  output$plot_user_order <- renderPlot(plot_user_order(data_filtered5()))
  
  hot_product <- function(data){
    product <- data %>% 
      group_by(product_name) %>% 
      summarise(count_order = n(),
                sum_product_price = sum(product_price)) 
    
    return(product)
  }
  
  plot_hot_product <- function(data){
    E <- hot_product(data)
    if(colnames(E)[2]== "count_order"){
      par(mfrow = c(1,2))
      barplot(E$count_order,
              main = "Hot product-Sales Volume",
              names.arg = E$product_name,
              cex.names = 0.65,
              ylab = "Sales Money",
              col = "red",
              las = 2
      )
      legend("topleft", "Sales Volume", cex = 0.8, fill = "red")
    } else {
      "Please run hot_product to generate count_order"
    }
    if(colnames(E)[3]== "sum_product_price"){
      barplot(E$sum_product_price,
              main = "Hot product-Sales Money",
              names.arg = E$product_name,
              cex.names = 0.65,
              col = "orange",
              las = 2
      )
      legend("topleft", "Sales Money", cex = 0.8, fill = "orange")
    } else {
      "Please run hot_product to generate sum_product_price"
    }
  }
  output$plot_hot_product <- renderPlot(plot_hot_product(orders))
  
  
  ######## Reactive event for page3 #########################
  output$funnel_chart <- renderHighchart({
    
    req(input$page3_filter)  # Ensure the input is available
    if (input$page3_filter == "all") {
      
    # pv_sum  <- nrow(pv_data)
    pv_sum  <- length(pv_data_object@behavior_id)
    cf_sum  <- length(cf_data_object@behavior_id)
    buy_sum  <- length(buy_data_object@behavior_id)
    merge_sum <- nrow(merged_data)
    
    sum
    # Calculating conversion rates
    convert_rate_action <- c(100, cf_sum[[1]] / pv_sum[[1]] * 100, buy_sum[[1]] / pv_sum[[1]] * 100)
    x_data <- c("Website visit", "Add to cart & Add to favorite", "Buy product")
    
    # Creating data for funnel chart
    data <- data.frame(name = x_data, y = convert_rate_action)
    
    # Creating funnel chart
    hc <- highchart() %>%
      hc_chart(type = "funnel") %>%
      hc_title(text = "conversion_rate_action",
               subtitle = "Browse --> Purchase&Favorite --> Purchase") %>%
      hc_add_series(data = data,
                    type = "funnel",
                    name = "",
                    dataLabels = list(enabled = TRUE, inside = TRUE),
                    tooltip = list(pointFormat = "{point.name}: {point.y}%"),
                    borderColor = "#fff",
                    borderWidth = 1)
    
    hc
    }
  })


  output$funnel_chart02 <- renderHighchart({
    # pv_sum01<-distinct(pv_data, user_id)
    req(input$page3_filter)  # Ensure the input is available
    
    if (input$page3_filter == "individual") {
    
    pv_sum_distinct  <- nrow(distinct(pv_data,user_id))
    cf_sum_distinct  <- nrow(distinct(cf_data,user_id))
    buy_sum_distinct  <- nrow(distinct(buy_data,user_id))

    sum
    # Calculating conversion rates
    conversion_rate_individual <- c(100, cf_sum_distinct[[1]] / pv_sum_distinct[[1]] * 100, buy_sum_distinct[[1]] / pv_sum_distinct[[1]] * 100)
    x_data <- c("Website visit", "Add to cart & Add to favorite", "Buy product")
    
    # Creating data for funnel chart
    data <- data.frame(name = x_data, y = conversion_rate_individual)
    
    # Creating funnel chart
    hc <- highchart() %>%
      hc_chart(type = "funnel") %>%
      hc_title(text = "conversion_rate_individual",
               subtitle = "Browse --> Purchase&Favorite --> Purchase") %>%
      hc_add_series(data = data,
                    type = "funnel",
                    name = "",
                    dataLabels = list(enabled = TRUE, inside = TRUE),
                    tooltip = list(pointFormat = "{point.name}: {point.y}%"),
                    borderColor = "#fff",
                    borderWidth = 1)
    hc
    }
  })
  
  ######## Reactive event for page4 #########################
  
  output$segmentation_plot <- renderPlotly({
    req(input$page4_filter)  # Ensure the input is available
    
    if (input$page4_filter == "radar") {
    scaled_data <- k_mean_data %>%
      select(-user_id) %>%
      scale()
    
    # Perform k-means clustering
    k <- 3  # Number of clusters
    kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
    
    # Get cluster assignments and cluster centers
    clusters <- kmeans_result$cluster
    centroids <- kmeans_result$centers
    
    # Add cluster assignments to the dataframe
    k_mean_data$group <- clusters
    
    # Restore cluster center coordinates
    centroids_restored <- centroids * attr(scaled_data, "scaled:scale") + attr(scaled_data, "scaled:center")
    
    # Plot the radar chart
    group1 <- as.list(centroids_restored[1, ])
    group2 <- as.list(centroids_restored[2, ])
    group3 <- as.list(centroids_restored[3, ])
    
    radar <- plot_ly(type = 'scatterpolar', mode = 'lines')
    
    radar <- add_trace(
      radar,
      r = c(group1$recency, group1$frequency, group1$monetary),
      theta = c('recency', 'frequency', 'monetary'),
      fill = 'toself',
      name = 'group1',
      line = list(color = '#f9713c')
    )

    radar <- add_trace(
      radar,
      r = c(group2$recency, group2$frequency, group2$monetary),
      theta = c('recency', 'frequency', 'monetary'),
      fill = 'toself',
      name = 'group2',
      line = list(color = '#b3e4a1')
    )

    radar <- add_trace(
      radar,
      r = c(group3$recency, group3$frequency, group3$monetary),
      theta = c('recency', 'frequency', 'monetary'),
      fill = 'toself',
      name = 'group3',
      line = list(color = '#5CACEE')
    )

    radar <- layout(
      radar,
      title = 'Radar-Buyer Group',
      showlegend = TRUE
    )

    radar
    }
  })
 
  output$column_chart <- renderPlotly({
    req(input$page4_filter)  # Ensure the input is available
    
    if (input$page4_filter == "chart") {
    
    scaled_data <- k_mean_data %>%
      select(-user_id) %>%
      scale()
    # Perform k-means clustering
    k <- 3  # Number of clusters
    kmeans_result <- kmeans(scaled_data, centers = k, nstart = 10)
    
    # Get cluster assignments and cluster centers
    clusters <- kmeans_result$cluster
    centroids <- kmeans_result$centers
    
    # Add cluster assignments to the dataframe
    k_mean_data$group <- clusters
    # Group the data by the 'group' column and calculate the count in each group
    group_counts <- k_mean_data %>% 
      group_by(group) %>% 
      summarize(count = n())
    
    # Create the column chart using ggplot2
    column_chart <- ggplot(group_counts, aes(x = group, y = count)) +
      geom_col(fill = "#5CACEE", width = 0.5) +  # Set the fill color and width of the columns
      labs(x = "Group", y = "Count") +  # Set the labels for the x and y axes
      ggtitle("Group Counts")  # Set the chart title
    
    # Return the column chart
    column_chart
    }
  })
  
  ######## Reactive event for page5 #########################
  #1. Reactive Event: waits until a button (Apply) is clicked to run reactive code 
  processed_data_filtered_tbl <- eventReactive(
    eventExpr = input$apply, 
    
    valueExpr = {
      
      processed_data_tbl %>%
        
        # Date Range filter
        filter(ORDERDATE %>% between(left = as_datetime(input$date_range[1]),
                                     right = as_datetime(input$date_range[2]))) %>%
        
        
        # Picker filter: Country
        filter(COUNTRY %in% input$picker_country)
    },
    ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
  )
  # 2.If time button is pressed, then act like Apply button is pressed
  observeEvent(eventExpr = input$time_unit, {
    
    if (input$forecast_mode) {
      
      delay(300, click(id = "apply")) # Add delay and simulate clicking actions in JavaScript
      
    } })
  
  # 3.If forecast switch is pressed, then act like Apply button is pressed...but only once
  observeEvent(eventExpr = input$forecast_mode, {
    
    delay(300, click(id = "apply")) # Add delay and simulate clicking actions in JavaScript
    
  }, once = TRUE) 



  # Reactive expression based on input
  time_plot_tbl <- reactive({
    
    processed_data_filtered_tbl() %>%
      aggregate_time_series(time_unit = input$time_unit)
    
  })
  
  # Reactive event for forecast switch
  time_plot_predictions_tbl <- eventReactive(eventExpr = input$apply, {
    
    if (input$forecast_mode) {
      time_plot_tbl() %>%
        generate_forecast(length_out  = input$n_future, seed = 123)
    }
    
  })

  #############################################################

  
  output$page5_output <- plotly::renderPlotly({
    
    if (input$forecast_mode) {
      p <- time_plot_predictions_tbl() %>%
        plot_forecast()
    } else {
      p <- time_plot_tbl() %>%
        plot_time_series()
    }
    
    p %>%
      layout(margin = list(b = 150))
    
  })
  opts <- list(
    
    pageLength = 10,
    searchHighlight = TRUE
  )
  
  output$page5_output02 <- DT::renderDataTable({
    
    if (input$forecast_mode) {
      time_plot_predictions_tbl()  %>%dplyr::select(key,date,total_sales)
    }
    
    else {time_plot_tbl() %>%dplyr::select(date,total_sales)}
    
  },options = opts)

######## Reactive event for page6 #########################
#1. Reactive Event: waits until a button (Apply) is clicked to run reactive code 
### Tip: For performance reasons, find repetitive code and put here
  processed_customer_filtered <- eventReactive(
    eventExpr = input$apply02, 
    
    valueExpr = {
      
      processed_data_tbl %>%
        
        # Date Range filter
        filter(ORDERDATE %>% between(left = as_datetime(input$date_range02[1]),
                                     right = as_datetime(input$date_range02[2]))) %>%
        
        
        # Picker filter: Country
        filter(CUSTOMERNAME  %in% input$picker_customer)
    },
    ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
  )
  
  # 2.If time button is pressed, then act like Apply button is pressed
  observeEvent(eventExpr = input$time_unit02, {
    
    if (input$forecast_mode02) {
      
      delay(300, click(id = "apply02")) # Add delay and simulate clicking actions in JavaScript
      
    } })
  
  # 3.If forecast switch is pressed, then act like Apply button is pressed...but only once
  observeEvent(eventExpr = input$forecast_mode02, {
    
    delay(300, click(id = "apply02")) # Add delay and simulate clicking actions in JavaScript
    
  }, once = TRUE) 
  #######################################
  #######################################################
  # Reactive expression based on input
  customer_plot_tbl <- reactive({
    
    processed_customer_filtered() %>%
      classification_time_series(time_unit = input$time_unit02)
    
  })
  
  # Reactive event for forecast switch
  customer_plot_predictions_tbl <- eventReactive(eventExpr = input$apply02, {
    
    if (input$forecast_mode02) {
      customer_plot_tbl() %>%
        customer_forecast(n_future = input$n_future02, seed = 123)
    }
    
  })
  #############################################################
  
  
  output$page6_output <- plotly::renderPlotly({
    
    if (input$forecast_mode02) {
      p02 <- customer_plot_predictions_tbl() %>%
        plot_customer_forecast ()
    } else {
      p02 <- customer_plot_tbl() %>%
        plot_classification_series()
    }
    
    p02 %>%
      layout(margin = list(b = 150))
    
  })
  
  output$page6_output02 <- DT::renderDataTable({
    
    if (input$forecast_mode02) {
      customer_plot_predictions_tbl ()  %>%dplyr::select(key,date,classification ,total_num)
    }
    
    else {customer_plot_tbl() %>%dplyr::select(date,classification,total_num)}
    
  },options = opts)
}

shinyApp(ui, server)





