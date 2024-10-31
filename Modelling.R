#### MODEL CHOICE ####

library(mgcv)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

setwd("C:\\Users\\glauc\\Desktop\\Glasgow\\ADA\\Individual Assignment 1")
dataset <- read.csv("MIDDLESBROUGH.csv")

#First, we list the variables we chose during the EDA.
variables <- c("price", "propertytype", "tfarea", "numberrooms", "ENERGY_CONSUMPTION_CURRENT", "MAINHEAT_ENV_EFF", "postcode_group")
#Among them, postcode_group was created according to the first 3 digits of the postcode variable.
dataset$postcode_group <- substr(dataset$postcode, 1, 3)
#After careful consideration, the levels TS6 and TS9 were removed to account for class imbalance.
#Please refer to the file "EDA" for a more detailed explanation.
dataset <- dataset %>% 
  filter(!(postcode_group %in% c("TS6", "TS9")))
# We then filter the dataset to keep only the specified variables
filtered_dataset <- dataset %>%
  select(all_of(variables))
head(filtered_dataset)

#Distribution of 'price'
ggplot(filtered_dataset, aes(x = price)) +
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Property Prices", x = "Price", y = "Frequency") +
  theme_minimal()

#Density plot
ggplot(filtered_dataset, aes(x = price)) +
  geom_density(fill = "steelblue", color = "darkblue", alpha = 0.7, adjust = 1.5) +
  labs(title = "Figure 1: Density Plot of Property Prices", x = "Price", y = "Density") +
  theme_gray(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, color = "darkviolet"),  # Titolo viola scuro
    axis.title.x = element_text(),
    axis.text.y = element_blank(),   
    axis.ticks.y = element_blank(),  
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_x_continuous(limits = c(0, 5e05))




# Based on the plot, a Gamma distribution may be a suitable candidate for 
# modeling property prices.
# This is for:
# Positive Skewness: The histogram clearly shows a right-skewed distribution, 
#                    The Gamma distribution is well-suited for modeling 
#                    positive-skewed data.
# Continuous, Positive Values: Property prices are continuous and always positive, 
#                              which aligns with the support of the Gamma 
#                              distribution (0 to infinity).

#Let's try some exploratory models and compare them with Gamma.

# 1. Linear Regression Model
cat("### Linear Regression Model ###\n")
lm_model <- lm(price ~ propertytype + tfarea + numberrooms + 
                 ENERGY_CONSUMPTION_CURRENT + MAINHEAT_ENV_EFF + postcode_group, 
               data = filtered_dataset)
lm_summary <- summary(lm_model)
print(lm_summary)
#Adjusted R-squared:  0.7932

# 2. Linear Model with Interaction Terms
cat("\n### Linear Model with Interaction Terms ###\n")
lm_interaction <- lm(price ~ propertytype * numberrooms + 
                       tfarea * numberrooms + 
                       ENERGY_CONSUMPTION_CURRENT * MAINHEAT_ENV_EFF + 
                       postcode_group, 
                     data = filtered_dataset)
lm_interaction_summary <- summary(lm_interaction)
print(lm_interaction_summary)
#Adjusted R-squared:  0.8064

# 3. Model with Polynomial Terms for Numerical Variables
cat("\n### Linear Model with Polynomial Terms ###\n")
lm_poly <- lm(price ~ poly(tfarea, 2) + poly(numberrooms, 2) + 
                ENERGY_CONSUMPTION_CURRENT + MAINHEAT_ENV_EFF + 
                propertytype + postcode_group, 
              data = filtered_dataset)
lm_poly_summary <- summary(lm_poly)
print(lm_poly_summary)
#Adjusted R-squared:  0.8005


# 4. Generalized Additive Models (GAM) with Gamma family and identity link function
cat("GAM with Gamma Family (Identity Link) ###\n")
gam_model <- gam(price ~ s(tfarea) + s(numberrooms) + 
                   s(ENERGY_CONSUMPTION_CURRENT) + propertytype + 
                   MAINHEAT_ENV_EFF + postcode_group, 
                 data = filtered_dataset, 
                 family = Gamma(link = "identity"))

cat("Summary of GAM model with identity link:\n")
gam_summary <- summary(gam_model)
print(gam_summary)
cat("Adjusted R-squared:", round(gam_summary$r.sq, 3), "\n")
#Adjusted R-squared: 0.806 

# 5 GAM with Gamma family and log link function 
cat("\n### Model 2: GAM with Gamma Family (Log Link) ###\n")
gam_log_model <- gam(price ~ s(tfarea) + s(numberrooms) + 
                       s(ENERGY_CONSUMPTION_CURRENT) + propertytype + 
                       MAINHEAT_ENV_EFF + postcode_group, 
                     data = filtered_dataset, 
                     family = Gamma(link = "log"))

cat("Summary of GAM model with log link:\n")
gam_log_summary <- summary(gam_log_model)
print(gam_log_summary)
cat("Adjusted R-squared:", round(gam_log_summary$r.sq, 3), "\n")
#Adjusted R-squared: 0.822 )

# Comparison
cat("\n### Model Comparison ###\n")
cat("The model with the log link has a higher adjusted R-squared, suggesting better fit for skewed price data.\n")



# A Gamma model seems suitable for the data.
# Now we will test a series of different Gamma models with link log to see the best fit.
fit_gam_model <- function(formula, data) {
  model <- gam(formula, data = data, family = Gamma(link = "log"))
  summary(model)
  return(list(model = model, aic = AIC(model), r_squared = summary(model)$r.sq))
}

model_results <- list()

# Model 5: Basic GAM with splines
cat("### Model 5: Basic GAM ###\n")
model5 <- fit_gam_model(price ~ s(tfarea) + s(numberrooms) + 
                          s(ENERGY_CONSUMPTION_CURRENT) + propertytype + 
                          MAINHEAT_ENV_EFF + postcode_group, 
                        filtered_dataset)
model_results[["Model 5"]] <- model5

# Model 6: Adding interaction between numberrooms and tfarea
cat("\n### Model 6: GAM with Interaction between numberrooms and tfarea ###\n")
model6 <- fit_gam_model(price ~ s(tfarea) + s(numberrooms) + 
                          s(ENERGY_CONSUMPTION_CURRENT) + propertytype + 
                          MAINHEAT_ENV_EFF + postcode_group + 
                          s(tfarea, by = numberrooms), 
                        filtered_dataset)
model_results[["Model 6"]] <- model6

# Model 7: Adding interaction between ENERGY_CONSUMPTION_CURRENT and MAINHEAT_ENV_EFF
cat("\n### Model 7: GAM with Interaction between ENERGY_CONSUMPTION_CURRENT and MAINHEAT_ENV_EFF ###\n")
model7 <- fit_gam_model(price ~ s(tfarea) + s(numberrooms) + 
                          ENERGY_CONSUMPTION_CURRENT + propertytype + 
                          ENERGY_CONSUMPTION_CURRENT * MAINHEAT_ENV_EFF + 
                          postcode_group, 
                        filtered_dataset)
model_results[["Model 7"]] <- model7

# Model 8: Including polynomial terms for numerical variables
cat("\n### Model 8: GAM with Polynomial Terms ###\n")
model8 <- fit_gam_model(price ~ s(tfarea, k = 10) + s(numberrooms, k = 10) + 
                          s(ENERGY_CONSUMPTION_CURRENT, k = 10) + 
                          propertytype + MAINHEAT_ENV_EFF + postcode_group, 
                        filtered_dataset)
model_results[["Model 8"]] <- model8

# Model 9: Combining all interactions
cat("\n### Model 9: GAM with All Interactions ###\n")
model9 <- fit_gam_model(price ~ s(tfarea) + s(numberrooms) + 
                          s(ENERGY_CONSUMPTION_CURRENT) + 
                          propertytype + MAINHEAT_ENV_EFF + postcode_group + 
                          s(tfarea, by = numberrooms) + 
                          ENERGY_CONSUMPTION_CURRENT * MAINHEAT_ENV_EFF, 
                        filtered_dataset)
model_results[["Model 9"]] <- model9

# Print adjusted R-squared for all models
cat("\n### Model Comparison ###\n")
for (name in names(model_results)) {
  cat(name, ": Adjusted R^2 =", round(model_results[[name]]$r_squared, 3), "\n")
}



#The best model seems to be the fifth one with 
#R-sq.(adj) =  0.826
best_model <- gam(price ~ s(tfarea) + s(numberrooms) + 
                    s(ENERGY_CONSUMPTION_CURRENT) + 
                    propertytype + MAINHEAT_ENV_EFF + postcode_group + 
                    s(tfarea, by = numberrooms) + 
                    ENERGY_CONSUMPTION_CURRENT * MAINHEAT_ENV_EFF,
                  family = Gamma(link = "log"),
                  data = filtered_dataset)
summary(best_model)
#Looking at the summary, we see that all the variables are significant.
#Some of them are not signifcant in each category (like MAINHEAT_ENV_EFF), 
#but at least one of the categories is significant.
#We want to look at the overall significance of MAINHEAT_ENV_EFF.
# We re-fit the full model (with MAINHEAT_ENV_EFF)
full_model <- gam(price ~ s(tfarea) + s(numberrooms) + 
                    s(ENERGY_CONSUMPTION_CURRENT) + 
                    propertytype + MAINHEAT_ENV_EFF + postcode_group + 
                    s(tfarea, by = numberrooms) + 
                    ENERGY_CONSUMPTION_CURRENT * MAINHEAT_ENV_EFF,
                  family = Gamma(link = "log"),
                  data = filtered_dataset)
# We fit the reduced model (without MAINHEAT_ENV_EFF)
reduced_model <- gam(price ~ s(tfarea) + s(numberrooms) + 
                       s(ENERGY_CONSUMPTION_CURRENT) + 
                       propertytype + postcode_group + 
                       s(tfarea, by = numberrooms) + 
                       ENERGY_CONSUMPTION_CURRENT,
                     family = Gamma(link = "log"),
                     data = filtered_dataset)
# Then we perform a likelihood ratio test to compare the models
lrt_result <- anova(reduced_model, full_model, test = "Chisq")
print(lrt_result)
#Strongly significant

# Finally, we check for the assumptions.
par(mfrow = c(2,2))
gam.check(best_model)
#The residual plots suggest that the assumptions of the regression are reasonably met. 
#Normality: The histogram of residuals appears roughly bell-shaped.
#           The Q-Q plot shows some deviation from the straight line in the tails.
#           However, it does not seem that the deviation is severe enough to invalidate the model.
#Independence: The residual plots do not show any obvious patterns or trends.
#              The independence assumption is likely met.
#Response vs Fitted Values: good fit between the observed and predicted values.
#                           There are some sporadic outliers.




#### QUESTION 1: Impact of Total Floor Area (tfarea) on Price ####

print(max(filtered_dataset$tfarea))  # Max tfarea: 680
# However, if we count the observations with tfarea greater than 300:
cat("Observations with tfarea > 300:", sum(filtered_dataset$tfarea > 300), "\n")
# Since the plot's monotonicity is impacted by these extreme values, we limit tfarea to 300
# This is just for a matter of clarity in the plots. When including those values 
# in the plot, the range of the Confidence Intervals (due to lack of observations)
# becomes too large.
min_tfarea <- min(filtered_dataset$tfarea)
max_tfarea <- 300
tfarea_seq <- seq(min_tfarea, max_tfarea, length.out = 100)

# We prepare a data frame with constant values for all variables except tfarea
predict_data <- data.frame(
  tfarea = tfarea_seq,
  numberrooms = median(filtered_dataset$numberrooms),  # Median preferred for robustness
  ENERGY_CONSUMPTION_CURRENT = median(filtered_dataset$ENERGY_CONSUMPTION_CURRENT),
  propertytype = factor("S"),  # Set to most common property type
  MAINHEAT_ENV_EFF = factor("Good"),  # Set to most frequent heating efficiency
  postcode_group = factor("TS5")  # Set to common postcode group
)

# We then generate predictions and calculate upper and lower bounds for the 
# confidence intervals.
pred <- predict(best_model, newdata = predict_data, type = "response", se.fit = TRUE)
predictions <- pred$fit
se <- pred$se.fit
upper_bound <- predictions + 1.96 * se
lower_bound <- predictions - 1.96 * se

# We create data frame with tfarea, price, and confidence intervals
plot_data <- data.frame(
  tfarea = tfarea_seq,
  price = predictions,
  upper_bound = upper_bound,
  lower_bound = lower_bound
)

#We will use this plot in the poster
ggplot(data = plot_data, aes(x = tfarea, y = price)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "blue", alpha = 0.2) +
  labs(
    title = "Figure 2: Impact of the Total Floor Area on Predicted Price",
    x = "Total Floor Area",
    y = "Estimated Price"
  ) +
  theme(
    plot.title = element_text(
      color = rgb(0.4, 0, 0.6),  # Dark violet
      size = 14               
    ),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12)
  )

# Conclusion: The plot shows a visible positive pattern.
#             As tfarea increases, price also tends to increase.




#### QUESTION 2: Do People Pay for Energy-Efficient Homes? ####

# Check the maximum value for ENERGY_CONSUMPTION_CURRENT in the dataset
print(max(filtered_dataset$ENERGY_CONSUMPTION_CURRENT))  # Max: 1284
# If there are any outliers, let’s count observations with ENERGY_CONSUMPTION_CURRENT > 900:
cat("Observations with ENERGY_CONSUMPTION_CURRENT > 900:", sum(filtered_dataset$ENERGY_CONSUMPTION_CURRENT > 900), "\n")

# For clearer visualization, let's limit ENERGY_CONSUMPTION_CURRENT to a max value of 900
# This is only for plotting purposes to prevent the confidence intervals from expanding too much 
# due to sparse observations in high ENERGY_CONSUMPTION_CURRENT ranges.
min_energy <- min(filtered_dataset$ENERGY_CONSUMPTION_CURRENT)
max_energy <- 900
energy_seq <- seq(min_energy, max_energy, length.out = 100)

# Prepare a data frame for prediction with constant values for all variables except ENERGY_CONSUMPTION_CURRENT
predict_data <- data.frame(
  tfarea = median(filtered_dataset$tfarea),  # Set tfarea to its median value
  numberrooms = median(filtered_dataset$numberrooms),  # Set numberrooms to its median value
  ENERGY_CONSUMPTION_CURRENT = energy_seq,
  propertytype = factor("S"),  # Set to most common property type
  MAINHEAT_ENV_EFF = factor("Good"),  # Set to the most frequent heating efficiency
  postcode_group = factor("TS5")  # Set to common postcode group
)
pred <- predict(best_model, newdata = predict_data, type = "response", se.fit = TRUE)
predictions <- pred$fit
se <- pred$se.fit
upper_bound <- predictions + 1.96 * se
lower_bound <- predictions - 1.96 * se

# We create a data frame with ENERGY_CONSUMPTION_CURRENT, predicted price, and confidence intervals
plot_data <- data.frame(
  ENERGY_CONSUMPTION_CURRENT = energy_seq,
  price = predictions,
  upper_bound = upper_bound,
  lower_bound = lower_bound
)

# Plotting ENERGY_CONSUMPTION_CURRENT vs. Predicted Price
ggplot(data = plot_data, aes(x = ENERGY_CONSUMPTION_CURRENT, y = price)) +
  geom_line(color = "green", size = 1.2) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "green", alpha = 0.2) +
  labs(
    title = "Impact of ENERGY_CONSUMPTION_CURRENT on Predicted Price",
    x = "Current Energy Consumption",
    y = "Estimated Price"
  ) +
  theme(
    plot.title = element_text(
      color = "green",            
      size = 14,                
      face = "bold"               
    )
  ) +
  scale_y_log10() 


cat("Percentage of observations with ENERGY_CONSUMPTION_CURRENT < 500:", mean(filtered_dataset$ENERGY_CONSUMPTION_CURRENT < 500), "\n")
#If we plot only the data with an energy consumption less than 500, that are more than 96%,
#we get an even clearer plot:
max_energy <- 500
energy_seq <- seq(min_energy, max_energy, length.out = 100)
predict_data <- data.frame(
  tfarea = median(filtered_dataset$tfarea),  # Set to its median value
  numberrooms = median(filtered_dataset$numberrooms),  # Set to its median value
  ENERGY_CONSUMPTION_CURRENT = energy_seq,
  propertytype = factor("S"),  # Set to most common property type
  MAINHEAT_ENV_EFF = factor("Good"),  # Set to the most frequent heating efficiency
  postcode_group = factor("TS5")  # Set to common postcode group
)

# We generate predictions for price based on ENERGY_CONSUMPTION_CURRENT
pred <- predict(best_model, newdata = predict_data, type = "response", se.fit = TRUE)
predictions <- pred$fit
se <- pred$se.fit
upper_bound <- predictions + 1.96 * se
lower_bound <- predictions - 1.96 * se
plot_data <- data.frame(
  ENERGY_CONSUMPTION_CURRENT = energy_seq,
  price = predictions,
  upper_bound = upper_bound,
  lower_bound = lower_bound
)

# Plotting ENERGY_CONSUMPTION_CURRENT vs. Predicted Price
ggplot(data = plot_data, aes(x = ENERGY_CONSUMPTION_CURRENT, y = price)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), fill = "darkgreen", alpha = 0.2) +
  labs(
    title = "Figure 4: Impact of Energy Consumption on Predicted Price",
    x = "Current Energy Consumption",
    y = "Estimated Price"
  ) +
  theme(
    plot.title = element_text(
      color = rgb(0.4, 0, 0.6),           
      size = 14
    )
  ) +
  scale_y_log10()
#The plot shows a downward trend in estimated prices as energy consumption 
#increases. Energy-efficient homes tend to command higher prices. 

# We now want to explore the variable MAINHEAT_ENV_EFF
# We Set MAINHEAT_ENV_EFF as an ordered factor to control the plot order
filtered_dataset$MAINHEAT_ENV_EFF <- factor(
  filtered_dataset$MAINHEAT_ENV_EFF,
  levels = c("Very Poor", "Poor", "Average", "Good", "Very Good")
)

# We prepare the data frame for prediction with constant values for all variables except MAINHEAT_ENV_EFF
predict_data <- data.frame(
  tfarea = median(filtered_dataset$tfarea),  # Median tfarea for robustness
  numberrooms = median(filtered_dataset$numberrooms),  # Median number of rooms
  ENERGY_CONSUMPTION_CURRENT = median(filtered_dataset$ENERGY_CONSUMPTION_CURRENT),  # Median energy consumption
  propertytype = factor("S"),  # Most common property type
  MAINHEAT_ENV_EFF = factor(levels(filtered_dataset$MAINHEAT_ENV_EFF), 
                            levels = c("Very Poor", "Poor", "Average", "Good", "Very Good")),
  postcode_group = factor("TS5")  # Common postcode group
)
predictions <- predict(best_model, newdata = predict_data, type = "response")

# We combine MAINHEAT_ENV_EFF categories and predictions in a data frame for plotting
plot_data <- data.frame(
  MAINHEAT_ENV_EFF = factor(levels(filtered_dataset$MAINHEAT_ENV_EFF), 
                            levels = c("Very Poor", "Poor", "Average", "Good", "Very Good")),
  price = predictions
)

# Plotting the boxplot to show price distribution across MAINHEAT_ENV_EFF categories
# Properties with more efficient heating systems might fetch higher prices as 
# they offer lower running costs and greater environmental appeal. 
ggplot(filtered_dataset, aes(x = MAINHEAT_ENV_EFF, y = log(price), fill = MAINHEAT_ENV_EFF)) +
  geom_boxplot() +
  labs(
    title = "Figure 5: Impact of Environmental Energy Efficiency on Property Prices",
    x = "Environmental Efficiency",
    y = "Log of Estimated Price"
  ) +
  theme(
    plot.title = element_text(
      color = rgb(0.6, 0.3, 0.8),           
      size = 14,                  
      face = "bold"               
    ),
    legend.position = "none"      
  ) +
  scale_fill_brewer(palette = "Blues")

# Conclusion: There is a visible impact of the two variables we explored when
#             accounting for all the other covariates.homes with higher energy 
#             efficiency ratings generally show higher prices. This suggests 
#             that buyers are willing to pay a premium for properties with 
#             efficient energy consumption and reduced environmental impact.




#### QUESTION 3: What Are the Most Expensive Areas of the Country? ####

# We define the postcode labels based on information from Wikipedia
table(filtered_dataset$postcode_group)
postcode_labels <- data.frame(
  postcode_group = c("TS1", "TS3", "TS4", "TS5", "TS7", "TS8"),
  area_name = c("TS1 - Town Centre",
                "TS3 - Middlesbrough's east",
                "TS4 - Middlesbrough's centre-east",
                "TS5 - Middlesbrough's west",
                "TS7 - Middlesbrough's South-east",
                "TS8 - Middlesbrough's south-west")
)

# We calculate the average and median price for each postcode group
average_price_by_postcode <- aggregate(price ~ postcode_group, data = filtered_dataset, FUN = mean)
median_price_by_postcode <- aggregate(price ~ postcode_group, data = filtered_dataset, FUN = median)
colnames(average_price_by_postcode)[2] <- "average_price"
colnames(median_price_by_postcode)[2] <- "median_price"
price_by_postcode <- merge(average_price_by_postcode, median_price_by_postcode, by = "postcode_group")
price_by_postcode <- merge(price_by_postcode, postcode_labels, by = "postcode_group", all.x = TRUE)
print(price_by_postcode)
price_by_postcode <- price_by_postcode[order(-price_by_postcode$average_price), ]
cat("Most expensive areas based on average price:\n")
print(price_by_postcode)

# We reorder the postcode groups for clarity of plots
postcode_order <- c("TS1", "TS3", "TS4", "TS5", "TS8", "TS7")

ggplot(data = filtered_dataset, aes(x = factor(postcode_group, levels = postcode_order), y = price)) +
  geom_boxplot(fill = rgb(0.6, 0.3, 0.8), color = "black") +  
  scale_x_discrete(labels = c("Town Centre", 
                              "East", 
                              "Centre-East", 
                              "West", 
                              "South-West", 
                              "South-East")) +  # Update labels to match the order
  labs(
    title = "Figure 2: Property Prices by Postcode Area",
    x = "Postcode Area",
    y = "Price"
  ) +
  ylim(0, 280000) +  # Limit the y-axis to 0 to 2800000 for clarity of plots
  theme(
    plot.title = element_text(
      color = rgb(0.6, 0.3, 0.8),  
      size = 14,               
      face = "bold"              
    ),
    axis.text.x = element_text(angle = 45, hjust = 1)  
  )

# Conclusion: the most expensive districts are the ones in Middlesbrough South-est:
#             Marton, Nunthorpe, Ormesby, while the Town Centre has the lowest average prices.


#Thank you! Feel free to reach out at 3059030R@student.gla.ac.uk!

