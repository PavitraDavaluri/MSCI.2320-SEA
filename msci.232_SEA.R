# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# Read the CSV data 
data <- read_csv("country_wise_latest.csv")

# Clean and prepare numeric columns
data$Confirmed <- as.numeric(data$Confirmed)
data$Deaths <- as.numeric(data$Deaths)
data$Recovered <- as.numeric(data$Recovered)
data$Active <- as.numeric(data$Active)

# Summarize totals by WHO Region
region_summary <- data %>%
  group_by(`WHO Region`) %>%
  summarise(
    Confirmed = sum(Confirmed, na.rm = TRUE),
    Deaths = sum(Deaths, na.rm = TRUE),
    Recovered = sum(Recovered, na.rm = TRUE),
    Active = sum(Active, na.rm = TRUE)
  )

# Bar plot: Confirmed cases by WHO Region
ggplot(region_summary, aes(x = `WHO Region`, y = Confirmed)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Confirmed COVID-19 Cases by WHO Region",
       x = "WHO Region",
       y = "Total Confirmed Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("region_confirmed_cases.png", dpi = 300)

# Stacked bar plot: Confirmed, Deaths, and Recovered by WHO Region
region_long <- region_summary %>%
  select(`WHO Region`, Confirmed, Deaths, Recovered) %>%
  tidyr::pivot_longer(-`WHO Region`, names_to = "Type", values_to = "Count")

ggplot(region_long, aes(x = `WHO Region`, y = Count, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(title = "COVID-19 Cases by WHO Region",
       x = "WHO Region",
       y = "Total Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("region_confirmed_deaths_recovered.png", dpi = 300)

# Export a sample of the data and summary as CSV
write_csv(head(data), "sample_data.csv")
write_csv(region_summary, "region_summary.csv")


