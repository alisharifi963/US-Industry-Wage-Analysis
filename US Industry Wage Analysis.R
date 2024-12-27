# Analysis of US Industry Wages
# Using Bureau of Labor Statistics (BLS) sample data

# Load required packages
library(tidyverse)
library(scales)
library(viridis)

# Create sample dataset (based on BLS data structure)
industries_wages <- data.frame(
  industry = c(
    "Information Technology", "Healthcare", "Manufacturing",
    "Financial Services", "Construction", "Retail Trade",
    "Professional Services", "Education", "Hospitality",
    "Transportation"
  ),
  avg_hourly_wage = c(52.45, 34.80, 31.25, 45.70, 33.15, 22.50, 
                      44.60, 29.85, 19.20, 28.90),
  employment = c(5800000, 16500000, 12300000, 8900000, 7600000,
                 15200000, 10100000, 13700000, 14200000, 6100000)
)

# Basic analysis functions
calculate_stats <- function(data) {
  stats <- list(
    mean_wage = mean(data$avg_hourly_wage),
    median_wage = median(data$avg_hourly_wage),
    wage_sd = sd(data$avg_hourly_wage),
    total_employment = sum(data$employment)
  )
  return(stats)
}

# Create visualizations
plot_wage_comparison <- function(data) {
  # Bar plot of average hourly wages
  ggplot(data, aes(x = reorder(industry, avg_hourly_wage), 
                   y = avg_hourly_wage)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    theme_minimal() +
    labs(
      title = "Average Hourly Wages by Industry",
      x = "Industry",
      y = "Average Hourly Wage ($)"
    ) +
    scale_y_continuous(labels = dollar_format()) +
    theme(
      plot.title = element_text(face = "bold"),
      axis.text = element_text(size = 10)
    )
}

plot_wage_vs_employment <- function(data) {
  # Scatter plot of wages vs employment
  ggplot(data, aes(x = employment/1000000, y = avg_hourly_wage)) +
    geom_point(aes(size = avg_hourly_wage), color = "steelblue", alpha = 0.6) +
    geom_text(aes(label = industry), 
              vjust = -0.5, 
              size = 3) +
    theme_minimal() +
    labs(
      title = "Wage vs Employment by Industry",
      x = "Total Employment (Millions)",
      y = "Average Hourly Wage ($)",
      size = "Hourly Wage"
    ) +
    scale_y_continuous(labels = dollar_format())
}

# Perform analysis
industry_stats <- calculate_stats(industries_wages)

# Print summary statistics
cat("Industry Wage Analysis Summary:\n")
cat("Mean Wage:", dollar(industry_stats$mean_wage), "\n")
cat("Median Wage:", dollar(industry_stats$median_wage), "\n")
cat("Wage Standard Deviation:", dollar(industry_stats$wage_sd), "\n")
cat("Total Employment:", format(industry_stats$total_employment, big.mark = ","), "\n")

# Create plots
p1 <- plot_wage_comparison(industries_wages)
print(p1)

p2 <- plot_wage_vs_employment(industries_wages)
print(p2)