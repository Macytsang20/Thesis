#install.packages("forecast")  # Only need to run once
library(forecast)

# Load required libraries
library(readxl)
library(dplyr)
library(tsibble)
library(fable)    # 👈 for na.approx
library(ggplot2)


data2 <- read_excel("/Users/m/Desktop/Final project/Cardiology/nc3.xlsx", sheet='Sheet1')


# Convert to time series
# Assuming daily data with no missing days
ts_data <- ts(data2$`Bed Occupancy`, frequency = 7)  # weekly seasonality

# Fit ETS model automatically
fit <- ets(ts_data)

# Print model summary
summary(fit)

# Forecast next 30 periods
fcast <- forecast(fit, h = 30)

# Plot forecast
autoplot(fcast) + ggtitle("ETS Forecast of Bed Occupancy")







data_nc9 <- read_excel("/Users/m/Desktop/Final project/Cardiology/nc3.xlsx", sheet='Sheet9')


data_nc <- data_nc9 %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE)) 


fit1 <- data_nc %>%
  model(ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("A")))



acf(residuals(fit1))
acf(residuals(fit1), lag.max = 80)


# Forecast 5 months ahead
fc1 <- fit1 %>%
  forecast(h = 30)


# Plot
fc1 %>%
  autoplot(data_nc) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit1)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - ANA") +
  guides(colour = "none")




fc2 <- fit1 %>%
  forecast(h = 10)

fc2 %>%
  autoplot(data_nc) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit1)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - ANA") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-01-18"), NA))







fit2 <- data_nc %>%
  model(ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("N")))

report(fit2)

acf(residuals(fit2))
acf(residuals(fit2), lag.max = 80)

# Step 3: Show model diagnostics
report(fit1)


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - ANA")


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - ANA")+
  scale_x_date(limits = c(as.Date("2018-02-01"), as.Date("2018-03-01"))) ###


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - ANA")+
  scale_x_date(limits = c(as.Date("2018-01-01"), as.Date("2018-12-31"))) ###









aug <- augment(fit1) 
e <- aug$.innov       


ggplot(aug, aes(x = .innov)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6) +
  geom_density(linewidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(e, na.rm = TRUE), sd = sd(e, na.rm = TRUE)),
                col = "red", linewidth = 1) +
  labs(title = "Residuals Histogram with Normal Curve in ANA (C)",
       x = "Residuals", y = "Density")















# Convert to tsibble
data_nc1 <- data_nc9 %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE)) 


# Fit ETS model: Additive error, No trend, No seasonality (ETS(A,N,N))
fit1 <- data_nc1 %>%
  model(ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("N")))

# Forecast 5 months ahead
fc1 <- fit1 %>%
  forecast(h = 30)


# Plot
fc1 %>%
  autoplot(data_nc1) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit1)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - ANN") +
  guides(colour = "none")




fc2 <- fit1 %>%
  forecast(h = 10)

fc2 %>%
  autoplot(data_nc1) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit1)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - ANN") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-01-18"), NA))



# Step 3: Show model diagnostics
report(fit1)


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - ANN")


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - ANN")+
  scale_x_date(limits = c(as.Date("2018-02-01"), as.Date("2018-03-01"))) ###


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - ANN")+
  scale_x_date(limits = c(as.Date("2018-01-01"), as.Date("2018-12-31"))) ###




aug <- augment(fit1) 
e <- aug$.innov       


ggplot(aug, aes(x = .innov)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.6) +
  geom_density(linewidth = 1) +
  stat_function(fun = dnorm,
                args = list(mean = mean(e, na.rm = TRUE), sd = sd(e, na.rm = TRUE)),
                col = "red", linewidth = 1) +
  labs(title = "Residuals Histogram with Normal Curve in ANN (C)",
       x = "Residuals", y = "Density")








# Step 1: Read and clean the Excel data
data_nc2 <- data_nc9 %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE))



# Step 2: (Optional) Summarise like Trips — here we just fit the full series
fit21 <- data_nc2  %>%
  model(ETS(`Bed Occupancy` ~ error("M") + trend("A") + season("A")))


# Forecast 5 months ahead
fc21 <- fit21 %>%
  forecast(h = 30)


# Plot
fc21 %>%
  autoplot(data_nc2) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit21)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - MAA") +
  guides(colour = "none")



fc22 <- fit21 %>%
  forecast(h = 10)

fc22 %>%
  autoplot(data_nc2) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit21)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - MAA") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-01-18"), NA))


# Step 3: Show model diagnostics
report(fit21)


components(fit21) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - MAA")



components(fit21) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - MAA")+
  scale_x_date(limits = c(as.Date("2018-02-01"), as.Date("2018-03-01"))) ###


components(fit21) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - MAA")+
  scale_x_date(limits = c(as.Date("2018-01-01"), as.Date("2018-12-31"))) ###











# Step 1: Read and clean the Excel data
data_nc3 <- data_nc9 %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE))

# Step 2: Fit the ETS model (Damped)
fit31 <- data_nc3 %>%
  model(
    Damped = ETS(`Bed Occupancy` ~ error("A") + trend("Ad") + season("M"))
  )


# Forecast 5 months ahead
fc31 <- fit31 %>%
  forecast(h = 30)

# Plot
fc31 %>%
  autoplot(data_nc3) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit31)) +
  labs(y = "Bed Occupancy", title = "Cardiology Hospital Bed Occupancy Forecast - AAdM") +
  guides(colour = "none")




fc32 <- fit31 %>%
  forecast(h = 10)

fc32 %>%
  autoplot(data_nc3) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit31)) +
  labs(y = "Bed Occupancy", title = "Cardiology Bed Occupancy Forecast - AAdM") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-01-18"), NA))




# Step 3: Show model diagnostics
report(fit31)


components(fit31) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - AAdM")


components(fit31) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - AAdM")+
  scale_x_date(limits = c(as.Date("2018-02-01"), as.Date("2018-03-01"))) ###


components(fit31) %>%
  autoplot() +
  labs(title = "ETS components: Cardiology - AAdM")+
  scale_x_date(limits = c(as.Date("2018-01-01"), as.Date("2018-12-31"))) ###











library(feasts)

# Read the data
data_nc9 <- read_excel("/Users/m/Desktop/Final project/Cardiology/nc3.xlsx", sheet = "Sheet1") %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps()  # Ensure regular spacing


data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CANN = ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("N"))
  ) %>%
  glance()



data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CANN = ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("A"))
  ) %>%
  glance()




data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CMAA = ETS(`Bed Occupancy` ~ error("M") + trend("A") + season("A"))
  ) %>%
  glance()


data_nc9 %>%
  stretch_tsibble(.init = 2) %>%
  model(
    CAAdM = ETS(`Bed Occupancy` ~ error("A") + trend("Ad") + season("M"))
  ) %>%
  glance()








# Rolling forecast accuracy
accuracy_results <- data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CANN = ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("N"))
     ) %>%
  forecast(h = 10) %>%
  accuracy(data_nc9)

# Print results
print(accuracy_results)





accuracy_results <- data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CANN = ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("A"))
  ) %>%
  forecast(h = 10) %>%
  accuracy(data_nc9)

# Print results
print(accuracy_results)







accuracy_results <- data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CMAA = ETS(`Bed Occupancy` ~ error("M") + trend("A") + season("A"))
  ) %>%
  forecast(h = 10) %>%
  accuracy(data_nc9)

# Print results
print(accuracy_results)




accuracy_results <- data_nc9 %>%
  stretch_tsibble(.init = 5) %>%
  model(
    CAAdM = ETS(`Bed Occupancy` ~ error("A") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = 10) %>%
  accuracy(data_nc9)

# Print results
print(accuracy_results)
