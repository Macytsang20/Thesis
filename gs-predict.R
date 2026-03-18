install.packages(c("readxl", "forecast", "ggplot2", "dplyr"))

library(readxl)
library(forecast)
library(ggplot2)
library(dplyr)



data <- read_excel("/Users/m/Desktop/Final project/General Surgery/ngs3.xlsx", sheet = 'Sheet1')
data$Date <- as.Date(data$Date)
data <- data %>% arrange(Date)


history_window <- 730  # 2 years
forecast_horizon <- 10
start_index <- 1
end_index <- history_window


all_forecasts <- data.frame(
  Date = as.Date(character()),
  Actual = numeric(),
  Forecast = numeric(),
  stringsAsFactors = FALSE
)



while((end_index + forecast_horizon - 1) <= nrow(data)) {
  
  
  train_series <- ts(data$`Bed Occupancy`[start_index:end_index], frequency = 7)
  
  # ETS 
  model <- ets(train_series, model = "AAA", damped = TRUE)
  forecast_result <- forecast(model, h = forecast_horizon)
  
  
  actual_dates <- data$Date[(end_index+1):(end_index+forecast_horizon)]
  actual_values <- data$`Bed Occupancy`[(end_index+1):(end_index+forecast_horizon)]
  
  
  temp_df <- data.frame(
    Date = actual_dates,
    Actual = actual_values,
    Forecast = as.numeric(forecast_result$mean)
  )
  
  all_forecasts <- bind_rows(all_forecasts, temp_df)
  
  cat("Forecasted up to:", max(actual_dates), "\n")
  
  
  start_index <- start_index + 10
  end_index <- end_index + 10
}


print(head(all_forecasts, 20))
cat("Forecasted up to:", max(actual_dates), "\n")


ggplot(all_forecasts, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 0.7) +
  geom_line(aes(y = Forecast), color = "red", size = 0.5) +
  labs(title = "General Surgery ETS(AAdA) Forecast vs Actual",
       subtitle = "Training data: 2017 - 2018",
        y = "Bed Occupancy") +
  theme_minimal()




ggplot() +
  geom_line(data = data, aes(x = Date, y = `Bed Occupancy`), color = "black", size = 0.7) +  
  geom_line(data = all_forecasts, aes(x = Date, y = Actual), color = "black", size = 0.7) +     
  geom_line(data = all_forecasts, aes(x = Date, y = Forecast), color = "red", size = 0.5) +
  labs(title = "General Surgery ETS(AAdA) Forecast vs Actual(Full data)",
       subtitle = "Date: 1/2019 - 3/2020",
       y = "Bed Occupancy") +
  theme_minimal()





#install.packages("writexl")
library(writexl)
write_xlsx(all_forecasts, "ets_gs_forecasts.xlsx")

getwd()




















data <- read_excel("/Users/m/Desktop/Final project/General Surgery/ngs3.xlsx", sheet = 'Sheet1')
data$Date <- as.Date(data$Date)
data <- data %>% arrange(Date)


history_window <- 365  # 2 years
forecast_horizon <- 10
start_index <- 1
end_index <- history_window


all_forecasts <- data.frame(
  Date = as.Date(character()),
  Actual = numeric(),
  Forecast = numeric(),
  stringsAsFactors = FALSE
)



while((end_index + forecast_horizon - 1) <= nrow(data)) {
  
  
  train_series <- ts(data$`Bed Occupancy`[start_index:end_index], frequency = 7)
  
  # ETS 
  model <- ets(train_series, model = "AAA", damped = TRUE)
  forecast_result <- forecast(model, h = forecast_horizon)
  
  
  actual_dates <- data$Date[(end_index+1):(end_index+forecast_horizon)]
  actual_values <- data$`Bed Occupancy`[(end_index+1):(end_index+forecast_horizon)]
  
  
  temp_df <- data.frame(
    Date = actual_dates,
    Actual = actual_values,
    Forecast = as.numeric(forecast_result$mean)
  )
  
  all_forecasts <- bind_rows(all_forecasts, temp_df)
  
  cat("Forecasted up to:", max(actual_dates), "\n")
  
  
  start_index <- start_index + 10
  end_index <- end_index + 10
}


print(head(all_forecasts, 20))
cat("Forecasted up to:", max(actual_dates), "\n")


ggplot(all_forecasts, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 0.7) +
  geom_line(aes(y = Forecast), color = "red", size = 0.5) +
  labs(title = "General Surgery ETS(AAdA) Forecast vs Actual",
       subtitle = "Date: 1/2018 - 3/2020",
       y = "Bed Occupancy") +
  theme_minimal()




library(dplyr)
library(ggplot2)

plot_data <- all_forecasts %>%
  filter(Date >= as.Date("2019-01-01") & Date <= as.Date("2020-02-29"))

ggplot(plot_data, aes(x = Date)) +
  geom_line(aes(y = Actual), color = "black", size = 0.7) +
  geom_line(aes(y = Forecast), color = "red", size = 0.5) +
  labs(
    title = "General Surgery ETS(AAdA) Forecast vs Actual",
    subtitle = "Training data: 2017 ",
    y = "Bed Occupancy"
  ) +
  theme_minimal()












ggplot() +
  geom_line(data = data, aes(x = Date, y = `Bed Occupancy`), color = "black", size = 0.7) +  
  geom_line(data = all_forecasts, aes(x = Date, y = Actual), color = "black", size = 0.7) +     
  geom_line(data = all_forecasts, aes(x = Date, y = Forecast), color = "red", size = 0.5) +
  labs(title = "General Surgery ETS(AAdA) Forecast vs Actual (Full data)",
       subtitle = "Date: 1/2018 - 3/2020",
       y = "Bed Occupancy") +
  theme_minimal()


