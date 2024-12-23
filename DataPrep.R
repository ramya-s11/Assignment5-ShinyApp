library(dplyr)
library(ggplot2)


data <- read.csv("data/DIG.csv", stringsAsFactors = TRUE)


head(data)


summary(data)


missing_summary <- sapply(data, function(x) sum(is.na(x)))
missing_summary <- missing_summary[missing_summary > 0]
print("Missing Value Summary:")
print(missing_summary)


data <- data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))


missing_summary_post <- sapply(data, function(x) sum(is.na(x)))
print("Missing Value Summary After Imputation:")
print(missing_summary_post)


summary_stats <- summary(data)


write.csv(summary_stats, "data/summary_stats.csv", row.names = TRUE)


ggplot(data, aes(x = BMI)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "white") +
  theme_minimal() +
  labs(
    title = "Histogram of BMI",
    x = "BMI",
    y = "Frequency"
  )


write.csv(data, "data/DIG_cleaned.csv", row.names = FALSE)

