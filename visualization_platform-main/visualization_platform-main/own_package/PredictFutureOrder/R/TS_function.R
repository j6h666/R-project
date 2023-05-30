#' A function that aggregates time series data
#'
#'This function aggregates the given data based on the specified time unit.
#'time_unit value includes "day", "week", "month", "quarter", "year".
#'Then it calculates the total sales for each time unit and generates the time series info.
#' @author Ting Wei
#' @param data A tibble format data, including the following columns:
#'   - ORDERDATE: A column in POSIXct format, e.g., "2020-02-24", representing the order dates.
#'   - SALES: A column recording the sales price.
#' @param time_unit A character string specifying the time unit to aggregate the data (default: "month").
#' @param ORDERDATE A column in POSIXct format, e.g., "2020-02-24", representing the order dates.
#' @param SALES A column recording the sales price.
#' @return A tibble with the aggregated data, including the date and total sales.
#' @examples
#' # Example data
#' data <- tibble::tribble(
#'   ~ORDERDATE, ~SALES,
#'   "2020-02-24", 2871,
#'   "2020-05-07", 2766,
#'   "2020-08-30", 3884,
#'   # More rows...
#' )
#' data$ORDERDATE=as.POSIXct(data$ORDERDATE)
#' aggregated_data <- aggregate_time_series(data,
#'                                          time_unit = "month")
#' @import dplyr
#' @import lubridate
#' @import stringr
#' @import scales
#' @export


#aggregate_time_series(processed_data_tbl)
aggregate_time_series <-
  function(data, ORDERDATE,SALES, time_unit = "month") {

    output_tbl <- data %>%

      dplyr::mutate(date = floor_date(ORDERDATE, unit = time_unit)) %>%
      group_by(date) %>%
      summarize(total_sales = sum(SALES)) %>%
      ungroup() %>%

      dplyr::mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}"))

    return(output_tbl)

  }


#' A function that plots the time series data
#'
#' This function plots the time series data, showing the total sales over time.
#'
#' @param data A data frame containing the time series data.
#' @param total_sales recording the total_sales basing on time unit
#' @param date A column in POSIXct format, e.g., "2020-02-24", representing the order dates.
#' @return A plot of the time series data.
#' @import ggplot2
#' @import ggthemes
#' @importFrom plotly ggplotly
#' @export

plot_time_series <-
function(data,total_sales,date) {

  g <- data %>%

    ggplot(aes(date, total_sales)) +

    geom_line(color = "#01579B") +
    geom_point( color = "#01579B", size = 0.1) +
    geom_smooth(method = "loess", span = 0.2) +

    theme_excel_new() +
    scale_color_gdocs() +
    expand_limits(y = 0) +
    scale_y_continuous(labels = scales::dollar_format()) +
    labs(x = "", y = "") +
    theme(axis.text=element_text(size=10.5))

  ggplotly(g)
}
#plot_time_series(a)

#' A function that generates forecasts for the time series data
#'
#' This function generates forecasts for the future time periods based on the given data.
#'
#' @param data A data frame containing the time series data.
#' @param total_sales recording the total_sales basing on time unit
#' @param length_out An integer specifying the number of future periods to forecast (default: 12 and min:1).
#' @param seed An optional seed for reproducibility (default: NULL).
#' @return A data frame with the actual and predicted values for the time series data.
#' @import dplyr
#' @import parsnip
#' @import tidyverse
#' @import stringr
#' @import scales
#' @import tibble
#' @import timetk
#' @importFrom  stats predict
#' @export

#b=generate_forecast(a,length_out = 12, total_sales = a$total_sales, seed=1234)

generate_forecast=function(data, total_sales,length_out = 12, seed = NULL) {

  train_tbl <- data %>%
    tk_augment_timeseries_signature()

  future_data_tbl <- data %>%
    tk_index() %>%
    tk_make_future_timeseries(length_out = length_out, inspect_weekdays = TRUE, inspect_months = TRUE) %>%
    tk_get_timeseries_signature()

  # Isolate and pull scale
  time_scale <-  data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)

  # Linear Regression for "year", XGBoost for other time units
  if (time_scale == "year") {

    model <- linear_reg(mode = "regression") %>%
      set_engine(engine = "lm") %>%
      fit.model_spec(total_sales ~ ., data = train_tbl %>% select(total_sales, index.num))


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
      fit.model_spec(total_sales ~ ., data = train_tbl %>% select(-date, -label_text, -diff))

  }

  prediction_tbl <- predict(model, new_data = future_data_tbl) %>%
    bind_cols(future_data_tbl) %>%
    select(.pred, index) %>%
    rename(total_sales = .pred,
           date        = index) %>%
    mutate(label_text = str_glue("Date: {date}
                                 Revenue: {scales::dollar(total_sales)}")) %>%
    add_column(key = "Prediction")

  output_tbl <- data %>%
    add_column(key = "Actual") %>%
    bind_rows(prediction_tbl)

  output_tbl

  return(output_tbl)
}

#' A function that plots the forecasts for the time series data
#'
#' This function plots the actual and predicted values for the time series data.
#'
#' @param data A data frame containing the time series data and forecasts.
#' @param key record the actual and predict value, format as "Revenue: $10,032,629 Actual" or "Revenue: $10,032,629 Predictl"
#' @param total_sales recording the total_sales basing on time unit
#' @return A plot of the actual and predicted values.
#' @import ggplot2
#' @import ggthemes
#' @importFrom plotly ggplotly
#' @export
#plot_forecast(b)
plot_forecast <-
function(data, key,total_sales) {

  # Yearly - LM Smoother (Loess)

  time_scale <- data %>%
    tk_index() %>%
    tk_get_timeseries_summary() %>%
    pull(scale)

  # If only one yearly prediction -> points

  n_predictions <- data %>%
    filter(key == "Prediction") %>%
    nrow()

  g <- data %>%
    ggplot(aes(date, total_sales, color = key)) +

    geom_line() +

    theme_excel_new() +
    scale_color_gdocs() +
    scale_y_continuous(labels = scales::dollar_format()) +
    expand_limits(y=0) +
    labs(x = "", y = "") +
    theme(axis.text=element_text(size=10.5))

  # Yearly - LM Smoother
  if (time_scale == "year") {
    g <- g + geom_smooth(method = "lm")
  } else {
    g <- g + geom_smooth(method = "loess", span = 0.2)
  }

  # Only 1 Prediction
  if (n_predictions == 1) {
    g <- g + geom_point(aes(text = date), size = 1)
  } else {
    g <- g + geom_point(aes(text = date), size = 0.01)
  }

  ggplotly(g)

}


