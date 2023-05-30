classification_time_series <-
  function(data, time_unit = "month") {
    
    output_tbl <- data %>%
      
      mutate(date = floor_date(ORDERDATE, unit = time_unit)) %>%
      
      group_by(date,classification) %>%
      summarize(total_num =n()) %>%
      ungroup() 
    
    return(output_tbl)
    
  }


plot_classification_series <-
  function(data) {
    
    g <- data %>%
      
      ggplot(aes(x = date, y = total_num, color = classification)) +
      
      geom_line() +

      geom_smooth(method = "loess", span = 0.1) +
      labs(title = "Present Customer Classification",
           x = "Date",
           y = "Total Number") +
      theme_minimal()
    
    ggplotly(g)
  }


customer_forecast <-
  function(data, n_future = 12, seed = NULL) {
    
    train_tbl <- data %>% 
      tk_augment_timeseries_signature()
    
    future_data_tbl <- data %>%
      tk_index() %>%unique()%>%
      tk_make_future_timeseries(n_future = n_future, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
      tk_get_timeseries_signature() 
    
    future_data_tbl<- future_data_tbl[rep(row.names(future_data_tbl), each = 3), ]
    
    categories <- c("Retain_Customers", "Lost_Customers", "Important_Vip")
    num_rows <- nrow(future_data_tbl)
    repeated_categories <- rep(categories, length.out = num_rows)
    future_data_tbl$classification <- repeated_categories
    
    
    # Isolate and pull scale 
    time_scale <-  data %>%
      tk_index() %>%unique()%>%
      tk_get_timeseries_summary() %>%
      pull(scale)
    
    # Linear Regression for "year", XGBoost for other time units
    if (time_scale == "year") {
      
      model <- linear_reg(mode = "regression") %>%
        set_engine(engine = "lm") %>%
        fit.model_spec(total_num ~ ., data = train_tbl %>% select(total_num,classification ,index.num))
      
      
    } else {
      seed <- seed
      set.seed(seed)
      model <- boost_tree(
        mode = "regression", 
        mtry = 20, 
        trees = 500, 
        min_n = 3, 
        tree_depth = 8, 
        learn_rate = 0.01, 
        loss_reduction = 0.01) %>%
        set_engine(engine = "xgboost") %>%
        fit.model_spec(total_num ~ ., data = train_tbl %>% select(-date,  -diff))
      
    }
 
    prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
      bind_cols(future_data_tbl) %>%
      select(.pred, index,classification) %>%
      rename(total_num = .pred, 
             date        = index) %>%
      add_column(key = "Prediction")
    
    output_tbl <- data %>%
      add_column(key = "Actual") %>%
      bind_rows(prediction_tbl) 
    
    output_tbl
    
    return(output_tbl)
  }


plot_customer_forecast <-
  function(data) {
    
    # Yearly - LM Smoother (Loess)
    
    time_scale <- data %>% 
      tk_index() %>%unique()%>%
      tk_get_timeseries_summary() %>%
      pull(scale)
    
    # If only one yearly prediction -> points
    
    n_predictions <- data %>%
      filter(key == "Prediction") %>%
      nrow()
    
    g <- data %>%
      ggplot(aes(x=date, y=total_num, color = interaction(key,classification))) +
      
      geom_line()  +
      labs(title = "Present Customer Classification",
           x = "Date",
           y = "Total Number") +
      theme_minimal()
    
    # Yearly - LM Smoother
    if (time_scale == "year") {
      g <- g + geom_smooth(method = "lm")
    } else {
      g <- g + geom_smooth(method = "loess", span = 0.2)
    }
    
    # Only 1 Prediction
    if (n_predictions == 1) {
      g <- g + geom_point( size = 1)
    } else {
      g <- g + geom_point(size = 0.01)
    }
    
    ggplotly(g)
    
  }
