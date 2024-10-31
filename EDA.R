#Welcome to the Exploratory Data Analysis R Script!


#### Property Price Analysis ####

library(dplyr)
library(ggplot2)
setwd("C:\\Users\\glauc\\Desktop\\Glasgow\\ADA\\Individual Assignment 1")
dataset <- read.csv("MIDDLESBROUGH.csv")
cat("The dataset contains", nrow(dataset), "observations and", ncol(dataset), "variables.\n")
cat("Dataset columns:\n")
print(names(dataset))

missing_price <- sum(is.na(dataset$price))
cat("Number of missing values in 'price':", missing_price, "\n")
#Distribution of 'price'
ggplot(dataset, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Property Prices", x = "Price", y = "Frequency") +
  theme_minimal()
#We notice that the distribution of prices is skewed.


# We will now select some variables for further exploratory analysis

#### EDA ####

#1 Analyze 'OLDNEW': Newly built properties often have a price premium due to 
#                    modern amenities.
cat("Unique values in 'OLDNEW':\n")
print(unique(dataset$oldnew))
ggplot(dataset, aes(x = oldnew, y = price, fill = oldnew)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(title = "Price Comparison: New vs Old Properties", 
       x = "Property Age", y = "Price (£)") +
  theme_minimal()
old_count <- sum(dataset$oldnew == "N")
new_count <- sum(dataset$oldnew == "Y")
cat("Number of old properties:", old_count, "\n")
cat("Number of new properties:", new_count, "\n")
cat("'OLDNEW' excluded as an explanatory variable due to class imbalance.\n")


#2 PROPERTYTYPE: Different property types have distinct price ranges
cat("Unique property types:\n")
print(unique(dataset$propertytype))
dataset$propertytype <- as.factor(dataset$propertytype)
property_type_counts <- dataset %>%
  count(propertytype) %>%
  rename("Property Type" = propertytype, "Count" = n)
print(property_type_counts)
ggplot(dataset, aes(x = propertytype)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Property Types", x = "Property Type", y = "Count") +
  theme_minimal()
ggplot(dataset, aes(x = propertytype, y = price, fill = propertytype)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(title = "Price Distribution by Property Type", 
       x = "Property Type", 
       y = "Price (£)") +
  theme_minimal()
summary_stats <- dataset %>%
  group_by(propertytype) %>%
  summarize(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    count = n()
  )
print(summary_stats)
cat("Apparently this variable has a significant impact on Prices.")
cat("We want to include it in the model and test for its significance.")


#3 TFAREA: Total floor area, which is directly related to price, 
#         as larger properties tend to be more expensive
cat("Total floor area statistics:\n")
summary(dataset$tfarea)
ggplot(dataset, aes(x = tfarea)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Total Floor Area (TFAREA)", 
       x = "Total Floor Area (m²)", 
       y = "Count") +
  theme_minimal()
# Relationship Between TFAREA and Price
ggplot(dataset, aes(x = tfarea, y = price)) +
  geom_point(alpha = 0.5, color = "darkblue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Linear regression with confidence intervals
  scale_y_continuous(labels = scales::comma) +
  xlim(0,400) +
  labs(title = "Relationship Between Total Floor Area and Price", 
       x = "Total Floor Area (m²)", 
       y = "Price (£)") +
  theme_minimal()
correlation <- cor(dataset$tfarea, dataset$price, use = "complete.obs")
cat("Correlation between Total Floor Area and Price:", correlation, "\n")
cat("tfarea is likely to have a significant impact on the prices.")
cat("We want to include it in the model,")


#4 NUMBERROOMS: Number of rooms may affect the price, similar to total floor area
cat("Range of 'NUMBERROOMS':", range(dataset$numberrooms, na.rm = TRUE), "\n")
ggplot(dataset, aes(x = numberrooms)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Number of Rooms", 
       x = "Number of Rooms", 
       y = "Count") +
  theme_minimal()
correlation_rooms_price <- cor(dataset$numberrooms, dataset$price, use = "complete.obs")
cat("Correlation between Number of Rooms and Price:", correlation_rooms_price, "\n")
ggplot(dataset, aes(x = factor(numberrooms), y = price, fill = factor(numberrooms))) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::comma) +  # Format y-axis with commas
  labs(title = "Price Distribution by Number of Rooms", 
       x = "Number of Rooms", 
       y = "Price (£)") +
  theme_minimal()
# Is NUMBERROOMS a proxy for tfarea?
# We evaluate the linear impact of NUMBERROOMS on price, adjusting for TFAREA
model <- lm(price ~ numberrooms + tfarea, data = dataset)
cat("Linear model summary for Price ~ Number of Rooms + Total Floor Area:\n")
print(summary(model))
par(mfrow = c(2, 2))
plot(model)
# 'NUMBERROOMS' shows a strong linear significance even when accounting for 
# the total floor area. It seems an important variable to include in the model.
# Note: there may be some potential assumptions violations. However, we don't
# want to fit a linear regression model. Here we are just having a look at the
# level of significance of the variable when accounting for tfarea to have a 
# rough idea of its impact.

#5 CURRENT ENERGY RATING: Higher energy efficiency may lead to higher property prices.
cat("Unique 'CURRENT_ENERGY_RATING' values:\n")
print(unique(dataset$CURRENT_ENERGY_RATING))
dataset$CURRENT_ENERGY_RATING <- factor(dataset$CURRENT_ENERGY_RATING)
cat("\nSummary of 'CURRENT_ENERGY_RATING':\n")
print(summary(dataset$CURRENT_ENERGY_RATING))
ggplot(dataset, aes(x = CURRENT_ENERGY_RATING)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Current Energy Rating", 
       x = "Current Energy Rating", 
       y = "Count") +
  theme_minimal()
ggplot(dataset, aes(x = CURRENT_ENERGY_RATING, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price Distribution by Current Energy Rating", 
       x = "Current Energy Rating", 
       y = "Price (£)") +
  theme_minimal()
price_summary <- dataset %>%
  group_by(CURRENT_ENERGY_RATING) %>%
  summarize(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    count = n()
  )
cat("\nPrice Summary by Current Energy Rating:\n")
print(price_summary)
table(dataset$CURRENT_ENERGY_RATING)
# Since there are some levels with a tiny amount of observations, we try
# to group some levels together.
dataset <- dataset %>%
  mutate(ENERGY_RATING_GROUP = case_when(
    CURRENT_ENERGY_RATING %in% c("A", "B", "C") ~ "A, B, C",
    CURRENT_ENERGY_RATING == "D" ~ "D",
    CURRENT_ENERGY_RATING %in% c("E", "F", "G") ~ "E, F, G"
  ))
dataset$ENERGY_RATING_GROUP <- factor(dataset$ENERGY_RATING_GROUP, levels = c("A, B, C", "D", "E, F, G"))
cat("Summary of 'ENERGY_RATING_GROUP':\n")
print(summary(dataset$ENERGY_RATING_GROUP))
ggplot(dataset, aes(x = ENERGY_RATING_GROUP)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Energy Rating Groups", 
       x = "Energy Rating Group", 
       y = "Count") +
  theme_minimal()
ggplot(dataset, aes(x = ENERGY_RATING_GROUP, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price Distribution by Energy Rating Group", 
       x = "Energy Rating Group", 
       y = "Price (£)") +
  theme_minimal()
price_summary_group <- dataset %>%
  group_by(ENERGY_RATING_GROUP) %>%
  summarize(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    count = n()
  )
cat("\nPrice Summary by Energy Rating Group:\n")
print(price_summary_group)
#Even grouping values to account accounting for class imbalance does not provide
#visible insights on price.


#6 ENERGY_CONSUMPTION_CURRENT: Higher energy consumption may lead to lower property prices
cat("Summary of 'ENERGY_CONSUMPTION_CURRENT':\n")
print(summary(dataset$ENERGY_CONSUMPTION_CURRENT))
correlation <- cor(dataset$ENERGY_CONSUMPTION_CURRENT, dataset$price, use = "complete.obs")
cat("\nCorrelation between 'price' and 'ENERGY_CONSUMPTION_CURRENT':", correlation, "\n")
ggplot(dataset, aes(x = ENERGY_CONSUMPTION_CURRENT)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Current Energy Consumption", 
       x = "Energy Consumption Current", 
       y = "Count") +
  theme_minimal()
ggplot(dataset, aes(x = ENERGY_CONSUMPTION_CURRENT, y = price)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  xlim(50, 700) +
  labs(title = "Price vs. Current Energy Consumption", 
       x = "Energy Consumption Current", 
       y = "Price (£)") +
  theme_minimal()
# The correlation analysis and visualizations suggest that 
# energy consumption could significantly influence property prices.
# We will include this variable in the model.


#7 ENERGY_CONSUMPTION_POTENTIAL: Higher energy consumption may lead to lower property prices
cat("Summary of 'ENERGY_CONSUMPTION_POTENTIAL':\n")
print(summary(dataset$ENERGY_CONSUMPTION_POTENTIAL))
correlation <- cor(dataset$ENERGY_CONSUMPTION_CURRENT, dataset$price, use = "complete.obs")
cat("Correlation between price and 'ENERGY_CONSUMPTION_POTENTIAL:", correlation)
# Distribution of ENERGY_CONSUMPTION_POTENTIAL
ggplot(dataset, aes(x = ENERGY_CONSUMPTION_POTENTIAL)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Potential Energy Consumption", 
       x = "Energy Consumption Potential", 
       y = "Count") +
  theme_minimal()
ggplot(dataset, aes(x = ENERGY_CONSUMPTION_POTENTIAL, y = price)) +
  geom_point(alpha = 0.5, color = "darkgreen") +
  geom_smooth(method = "lm", formula = y ~ x, color = "blue", se = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  xlim(0, 600) +
  labs(title = "Price vs. Potential Energy Consumption", 
       x = "Energy Consumption Potential", 
       y = "Price (£)") +
  theme_minimal()
#No visible impact on prices.


#8 MAINHEAT_ENV_EFF: This variable represents the environmental efficiency 
#                    rating, ranging from "Very Poor" to "Very Good". Since 
#                    energy efficiency can impact the overall desirability
#                    and operational cost of a property, it is likely to 
#                    influence its market price. 
dataset$MAINHEAT_ENV_EFF <- factor(dataset$MAINHEAT_ENV_EFF, levels = c("Very Poor", "Poor", "Average", "Good", "Very Good"), ordered = TRUE)
cat("Summary of 'MAINHEAT_ENV_EFF':\n")
print(summary(dataset$MAINHEAT_ENV_EFF))
correlation <- cor(dataset$MAINHEAT_ENV_EFF, dataset$price, use = "complete.obs")
cat("Correlation between price and 'MAINHEAT_ENV_EFF':", correlation)
# Distribution of MAINHEAT_ENV_EFF
ggplot(dataset, aes(x = MAINHEAT_ENV_EFF)) +
  geom_bar(fill = "lightblue", color = "black") +
  labs(title = "Distribution of Main Heating Environmental Efficiency", 
       x = "Main Heating Environmental Efficiency", 
       y = "Count") +
  theme_minimal()
table(dataset$MAINHEAT_ENV_EFF) 
#Note: some class imbalance. However, even the smaller class has 135 observation.
# Price Distribution by MAINHEAT_ENV_EFF
ggplot(dataset, aes(x = MAINHEAT_ENV_EFF, y = price)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Price Distribution by Main Heating Environmental Efficiency", 
       x = "Main Heating Environmental Efficiency", 
       y = "Price (£)") +
  theme_minimal()
# Summary Statistics by MAINHEAT_ENV_EFF
price_summary_heat_eff <- dataset %>%
  group_by(MAINHEAT_ENV_EFF) %>%
  summarize(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    count = n()
  )
cat("\nPrice Summary by Main Heating Environmental Efficiency:\n")
print(price_summary_heat_eff)
# Conclusion: `MAINHEAT_ENV_EFF` may be a valuable predictor for price due to its potential correlation with property value. 
# Properties with more efficient heating systems might fetch higher prices as they offer lower running costs and greater environmental appeal. 


#9-13 DISTRICT / TOWNCITY / LOCALITY / COUNTY / POSTCODE
table(dataset$district) #No variation within the dataset.
table(dataset$towncity) #Highly imbalanced distribution with limited categories
sum(is.na(dataset$locality)) #Excessive missing values leading to potential bias.
table(dataset$county) #No variation within the dataset.
# Analyze 'POSTCODE': A proxy for neighborhood quality, accessibility, and local amenities
cat("First few postcodes:\n")
head(dataset$postcode)
unique_postcodes <- length(unique(dataset$postcode))
cat("Number of unique postcodes:", unique_postcodes, "\n")
cat("'POSTCODE' cannot be used like it is as an explanatory variable due to excessive unique values.\n\n")
dataset$postcode_group <- substr(dataset$postcode, 1, 3)
postcode_group_summary <- dataset %>%
  group_by(postcode_group) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
print(postcode_group_summary)
ggplot(postcode_group_summary, aes(x = postcode_group, y = Count)) +
  geom_bar(stat = "identity",
           fill = "lightblue",
           color = "black") +
  labs(title = "Distribution of Postcode Groups",
       x = "Postcode Group (First 3 Digits)",
       y = "Count") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1) 
  )
cat("There is class imbalance. TS6 and TS9 categories have just a few observations.")
cat("However, grouping postcodes is a very risky strategy. TS6 and TS9 are not adjacent.")
cat("Moreover, those levels cannot be grouped alone. A third level should be grouped with them to reach a fair amount of observations.")
cat("However, this would mean grouping those levels in a meaningless level 'Other'")
cat("I simply prefer dropping the levels TS6 and TS9 to mantain interpretability.")
dataset <- dataset %>% 
  filter(!(postcode_group %in% c("TS6", "TS9")))
print(unique(dataset$postcode_group))
#Comparison between postcode groups and price
dataset$postcode_group <- as.factor(dataset$postcode_group)
ggplot(dataset, aes(x = postcode_group, y = price)) +
  geom_boxplot(fill = "darkblue", alpha = 0.5) +
  labs(title = "Price Distribution by Postcode Group",
       x = "Postcode Group (First 3 Digits)",
       y = "Price (£)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# The postcode seems to have an impact on the price. We want to include this
# variable in the model.


#Thanks! Go to the 'Modelling' file to continue!

