#install.packages("readxl")
library(readxl)

data_ngs <- read_excel("/Users/m/Desktop/Final project/General Surgery/ngs3.xlsx", sheet='Sheet1')


library(fpp3)

ggplot(data_ngs, aes(x = `Date`, y = `Bed Occupancy` )) +
  geom_line(color = "black") +
  labs(
    title = "General Surgery Bed Occupancy Over Time",
    x = "Time",
    y = "Bed Occupancy"
  ) +
  theme_minimal()






library(ggplot2)
data_ngs6 <- read_excel("/Users/m/Desktop/Final project/General Surgery/ngs3.xlsx", sheet='Sheet6')

# Rename columns for easier use
colnames(data_ngs6) <- c("Month", "Bed_Occupancy")

# Ensure Month is an ordered factor (Jan to Dec)
data_ngs6$Month <- factor(data_ngs6$Month, levels = month.abb)

# Plot: Calendar order preserved
ggplot(data_ngs6, aes(x = Month, y = Bed_Occupancy)) +
  geom_col(fill = "#0072B2") +
  labs(title = "General Surgery Monthly Bed Occupancy",
       x = "Month", y = "Bed Occupancy") +
  theme_minimal()




# Load necessary libraries
library(readxl)
library(ggplot2)
library(dplyr)

# Read the Excel file (adjust path as needed)
data_ngs7 <- read_excel("ngs3.xlsx", sheet = "Sheet7")

# Optional: Ensure month order is preserved
data_ngs7$Date <- factor(data_ngs7$Date, 
                        levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

# Create the boxplot
ggplot(data_ngs7, aes(x = Date, y = `Bed Occupancy`)) +
  geom_boxplot(fill = "#0072B2", color = "black") +
  labs(title = "General Surgery Monthly Bed Occupancy Distribution",
       x = "Month", y = "Bed Occupancy") +
  theme_minimal()








data_ngs_week <- read_excel("/Users/m/Desktop/Final project/General Surgery/ngs3.xlsx", sheet='Sheet5')

# Rename columns for easier use
colnames(data_ngs_week) <- c("Weekday", "Bed_Occupancy")

weekday_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
data_ngs_week$Weekday <- factor(data_ngs_week$Weekday, levels = weekday_order)

# Plot: Calendar order preserved
ggplot(data_ngs_week, aes(x = Weekday, y = Bed_Occupancy)) +
  geom_col(fill = "#0072B2") +
  labs(title = "General Surgery Weekly Bed Occupancy",
       x = "Day of the Week", y = "Bed Occupancy") +
  theme_minimal()




# Read the Excel file (adjust path as needed)
data_ngs3 <- read_excel("ngs3.xlsx", sheet = "Sheet3")

# Optional: Ensure month order is preserved
data_ngs3$Date <- factor(data_ngs3$Date, 
                         levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Create the boxplot
ggplot(data_ngs3, aes(x = Date, y = `Bed Occupancy`)) +
  geom_boxplot(fill = "#0072B2", color = "black") +
  labs(title = "General Surgery Weekly Bed Occupancy Distribution",
       x = "Day of the Week", y = "Bed Occupancy") +
  theme_minimal()











# Read the Excel file (adjust path as needed)
data_ngs7 <- read_excel("ngs3.xlsx", sheet = "Sheet7")
# Rename columns just in case
colnames(data_ngs7) <- c("Month", "Bed_Occupancy")

# Assign season manually based on Month
data_ngs7 <- data_ngs7 %>%
  mutate(
    Season = case_when(
      Month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      Month %in% c("Mar", "Apr", "May") ~ "Spring",
      Month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      Month %in% c("Sep", "Oct", "Nov") ~ "Autumn",
      TRUE ~ NA_character_
    )
  )

# Create a density plot (spectrum-like visualization)
ggplot(data_ngs7, aes(x = Bed_Occupancy, fill = Season)) +
  geom_density(alpha = 0.6) +
  labs(title = "Gerenal Surgery Density Spectrum of Bed Occupancy by Season",
       x = "Bed Occupancy", y = "Density") +
  theme_minimal()






data_ngs7 <- data_ngs7 %>%
  mutate(
    Season = case_when(
      Month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      Month %in% c("Mar", "Apr", "May") ~ "Spring",
      Month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      Month %in% c("Sep", "Oct", "Nov") ~ "Autumn",
      TRUE ~ NA_character_
    ),
    Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
  )



ggplot(data_ngs7, aes(x = Season, y = Bed_Occupancy, fill = Season)) +
  geom_boxplot(color = "black") +
  labs(title = "General Surgery Bed Occupancy by Season",
       x = "Season", y = "Bed Occupancy") +
  theme_minimal()
  








# Load required libraries
library(readxl)
library(dplyr)
library(tsibble)
library(fable)
library(ggplot2)


# Convert to tsibble
data_ngs1 <- data_ngs %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE)) 


# Fit ETS model: Additive error, No trend, No seasonality (ETS(A,N,N))
fit1 <- data_ngs1 %>%
  model(ETS(`Bed Occupancy` ~ error("A") + trend("N") + season("N")))

# Forecast 5 months ahead
fc1 <- fit1 %>%
  forecast(h = 30)


# Plot
fc1 %>%
  autoplot(data_ngs1) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit1)) +
  labs(y = "% Occupancy", title = "General Surgery Bed Occupancy Forecast - ANN") +
  guides(colour = "none")



fc2 <- fit1 %>%
  forecast(h = 10)

fc2 %>%
  autoplot(data_ngs1) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit1)) +
  labs(y = "% Occupancy", title = "General Surgery Bed Occupancy Forecast - ANN") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-02-01"), NA))





# Step 3: Show model diagnostics
report(fit1)


components(fit1) %>%
  autoplot() +
  labs(title = "ETS components: General Surgery - ANN")














# Step 1: Read and clean the Excel data
data_ngs2 <- data_ngs %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE))



# Step 2: (Optional) Summarise like Trips — here we just fit the full series
fit21 <- data_ngs2  %>%
  model(ETS(`Bed Occupancy` ~ error("M") + trend("A") + season("A")))


# Forecast 5 months ahead
fc21 <- fit21 %>%
  forecast(h = 30)


# Plot
fc21 %>%
  autoplot(data_ngs2) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit21)) +
  labs(y = "% Occupancy", title = "General Surgery Bed Occupancy Forecast - MAA") +
  guides(colour = "none")



fc22 <- fit21 %>%
  forecast(h = 10)

fc22 %>%
  autoplot(data_ngs2) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit21)) +
  labs(y = "% Occupancy", title = "General Surgery Bed Occupancy Forecast - MAA") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-02-01"), NA))


# Step 3: Show model diagnostics
report(fit21)


components(fit21) %>%
  autoplot() +
  labs(title = "ETS components: General Surgery - MAA")





















# Step 1: Read and clean the Excel data
data_ngs3 <- data_ngs %>%
  mutate(Date = as.Date(Date)) %>%
  as_tsibble(index = Date) %>%
  fill_gaps() %>%
  mutate(`Bed Occupancy` = zoo::na.approx(`Bed Occupancy`, na.rm = FALSE))

# Step 2: Fit the ETS model (Damped)
fit31 <- data_ngs3 %>%
  model(
    Damped = ETS(`Bed Occupancy` ~ error("A") + trend("Ad") + season("M"))
  )


# Forecast 5 months ahead
fc31 <- fit31 %>%
  forecast(h = 30)


# Plot
fc31 %>%
  autoplot(data_ngs3) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit31)) +
  labs(y = "% Occupancy", title = "General Surgery Hospital Bed Occupancy Forecast - AAdM") +
  guides(colour = "none")



fc32 <- fit31 %>%
  forecast(h = 10)

fc32 %>%
  autoplot(data_ngs3) +
  geom_line(aes(y = .fitted), col = "#D55E00", data = augment(fit31)) +
  labs(y = "% Occupancy", title = "General Surgery Bed Occupancy Forecast - AAdM") +
  guides(colour = "none") +
  scale_x_date(limits = c(as.Date("2020-02-01"), NA))



# Step 3: Show model diagnostics
report(fit31)


components(fit31) %>%
  autoplot() +
  labs(title = "ETS components: General Surgery - AAdM")






# Extract info with glance
info1 <- glance(fit1)
info21 <- glance(fit21)
info31 <- glance(fit31)

# Build comparison table
model_comparison <- bind_rows(
  info1 %>% mutate(Model = "GS-ANN"),
  info21 %>% mutate(Model = "GS-MAA"),
  info31 %>% mutate(Model = "GS-AAdM")
) %>%
  select(Model, AIC, BIC, AICc)

print(model_comparison)

