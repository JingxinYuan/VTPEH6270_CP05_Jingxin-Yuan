# Packages
library(tidyverse)
library(knitr)

# Part 1: Load data
# Set working directory
setwd("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Data/Original data")

# Load file
data_ckd = read.csv("IHME-GBD_CKD_20260130.csv")

#Check if it contains NA
n_na <- sum(is.na(data_ckd))
cat("The amount of NA is",n_na,end='\n')

#Check available years
sort(unique(data_ckd$year))

#Check of Abnormal Value
#Define IQR measure to detect abnormal value
detect_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR_value <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  return(x < lower_bound | x > upper_bound)
}

#Detected column
numeric_cols <- c("val", "upper", "lower")

#Detect
outliers_df <- data_ckd %>%
  mutate(across(all_of(numeric_cols), detect_outliers))

outlier_summary <- colSums(outliers_df[numeric_cols], na.rm = TRUE)
print(outlier_summary)

#Replace abnormal value with median value
median_vals <- apply(data_ckd[numeric_cols], 2, median, na.rm = TRUE)

data_ckd[numeric_cols] <- lapply(numeric_cols, function(col) {
  data_ckd[[col]] <- ifelse(outliers_df[[col]], median_vals[col], data_ckd[[col]])
})

# filter data
data_ckd_old <- data_ckd %>%
  filter(
    location_name == "China",
    age_name == "60+ years",
    measure_name == "Deaths"
  ) %>%
  select(year, sex_name, val)

# Examine data structure
str(data_ckd_old)

# Create variable description table
var_table <- tibble(
  Variable_Name = c("year", "sex_name", "val"),
  Variable_Type = c("Discrete", "Categorical", "Continuous"),
  R_Class = c(class(data_ckd_old$year),
              class(data_ckd_old$sex_name),
              class(data_ckd_old$val)),
  Description = c(
    "Observation year",
    "Gender (Male or Female)",
    "CKD mortality rate per 100,000 population"
  )
)
var_table


# Part 2
# Statistical Analyses
# Split data
male_data <- data_ckd_old %>% filter(sex_name == "Male") %>% pull(val)
female_data <- data_ckd_old %>% filter(sex_name == "Female") %>% pull(val)

# Shapiro test
shapiro.test(male_data)
shapiro.test(female_data)

# QQ plots
qqnorm(male_data, main = "QQ Plot for Male")
qqline(male_data)
qqnorm(female_data, main = "QQ Plot for Female")
qqline(female_data)

# t-test
t_test_result <- t.test(val ~ sex_name, data = data_ckd_old)
t_test_result


# Part 3: Results
# Visualization
p3 <- ggplot(data_ckd_old, aes(x = year, y = val, color = sex_name)) +
  geom_line(size = 1.2) +
  labs(
    title = "CKD Mortality Rate in China (Age 60+ Years)",
    x = "Year",
    y = "Mortality Rate per 100,000",
    color = "Gender"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "top"
  )
ggsave("C:/Users/乌乌没有仙人/Desktop/6270/HW5/VTPEH6270_Jingxin-Yuan/Output/Figures/Gender trend on mortality of CKD of 60+ years people in China.png",plot = p3)

# Show plot
p3

