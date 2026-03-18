install.packages("readxl")
install.packages("ggplot2")

library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)

gs_cor1 <- read_excel("/Users/m/Desktop/Final project/General Surgery/ets_gs_forecasts.xlsx", sheet = 'Sheet1')

data_long <- pivot_longer(gs_cor1, cols = c("Actual", "Forecast"),
                          names_to = "Type", values_to = "Value")

# Total number of rows
total_n <- nrow(gs_cor1)   

# Plot relative frequency histogram
ggplot(data_long, aes(x = Value, fill = Type)) +
  geom_histogram(aes(y = after_stat(count / total_n * 100)),  # ✅ percent of total (e.g. 421 × 2 = 842)
                 bins = 30, position = "identity", alpha = 0.5) +
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +  # show true %
  scale_fill_manual(values = c("Actual" = "blue", "Forecast" = "red")) +
  labs(
    title = "General Surgery Distribution of Actual vs Forecast",
    x = "Bed Occupancy", y = "Percentage of Total Observations"
  ) +
  theme_minimal()









# Custom color setup (you've already done this)
data_long <- data_long %>%
  mutate(FillColor = case_when(
    Type == "Actual" ~ "red",
    Type == "Forecast" ~ "blue"
  ))

# Plot faceted histogram with percentage Y-axis
ggplot(data_long, aes(x = Value, fill = FillColor)) +
  geom_histogram(
    aes(y = after_stat(count / total_n * 100)),  # 🔥 Convert to percentage of total
    bins = 30, color = "black", alpha = 0.7
  ) +
  facet_wrap(~ Type, ncol = 2) +
  scale_fill_identity() +  # use FillColor column values directly
  scale_y_continuous(labels = function(x) paste0(round(x, 1), "%")) +
  labs(
    title = "General Surgery Distribution of Actual and Forecast",
    x = "Bed Occupancy", y = "Percentage of Total Observations"
  ) +
  theme_minimal()





















#Actual vs Forecast
ggplot(gs_cor1, aes(x = Actual, y = Forecast)) +
  geom_point() +                            
  geom_abline(slope = 1, intercept = 0, color = "red", size=1) +  # 45-degree line
  labs(title = "General Surgery comparison between Actual and Forecast",
       x = "Actual",
       y = "Forecast") +
  theme_minimal()







df <- subset(gs_cor1, !is.na(Actual) & !is.na(Forecast))
y  <- df$Actual
yhat <- df$Forecast

SSR <- sum((1.*y - yhat)^2)             
SST <- sum((1.*y - mean(y))^2)         

R2_manual <- 1. - SSR / SST
R2_manual

print(SSR)
print(SST)






model <- lm(Actual ~ Forecast, data = gs_cor1)
summary(model)


plot(gs_cor1$Forecast, gs_cor1$Actual,
     main = "AAdA ETS(GS) Linear Regression of Actual vs Forecast",
     xlab = "Forecast", ylab = "Actual",
     pch = 19, col = "black")

abline(model, col = "red", lwd = 2)


# R²
r2 <- summary(model)$r.squared
cat("R-squared:", r2, "\n")

adj_r2 <- summary(model)$adj.r.squared
cat("Adjusted R-squared:", adj_r2, "\n")










#6 months
gs_cor2 <- read_excel("/Users/m/Desktop/Final project/General Surgery/ets_gs_forecasts.xlsx", sheet = 'Sheet2')


#Actual vs Forecast
ggplot(gs_cor2, aes(x = Actual, y = Forecast)) +
  geom_point() +                            
  geom_abline(slope = 1, intercept = 0, color = "red", size=1) +  # 45-degree line
  labs(title = "General Surgery Correlation between Actual and Forecast - 6 months",
       x = "Actual",
       y = "Forecast") +
  theme_minimal()








#1 year
gs_cor3 <- read_excel("/Users/m/Desktop/Final project/General Surgery/ets_gs_forecasts.xlsx", sheet = 'Sheet3')


#Actual vs Forecast
ggplot(gs_cor3, aes(x = Actual, y = Forecast)) +
  geom_point() +                            
  geom_abline(slope = 1, intercept = 0, color = "red", size=1) +  # 45-degree line
  labs(title = "General Surgery Correlation between Actual and Forecast - 1 year",
       x = "Actual",
       y = "Forecast") +
  theme_minimal()




df <- subset(gs_cor3, !is.na(Actual) & !is.na(Forecast))
y  <- df$Actual
yhat <- df$Forecast

SSR <- sum((y - yhat)^2)             
SST <- sum((y - mean(y))^2)         

R2_manual <- 1 - SSR / SST
R2_manual

