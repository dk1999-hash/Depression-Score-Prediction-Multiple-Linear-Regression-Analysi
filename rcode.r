#H0: No difference in depression scores between males and females.
#HA: A significant difference in depression scores between males and females.
#installing packages and loading their libraries
install.packages("dplyr")
install.packages("readr")
install.packages("tidyverse")
install.packages("tibble")
install.packages("ggplot2")
library(tidyverse)
library(tibble)
# Load ggplot2 library
library(readr)
library(ggplot2)
library(dplyr)
getwd()
Group4 <- read.csv("Group4.csv", header = TRUE)
Group4

# Assuming Group4 is your dataset
training_data <- Group4[Group4$TRAINTEST == 1, ]
testing_data <- Group4[Group4$TRAINTEST == 0, ]

# T-test for SEX
t_test_sex <- t.test(DEPSCORE ~ SEX)
print(t_test_sex)

#In a Welch two-sample t-test comparing depression 
#scores between males and females, 
#the results (t(380.9) = 1.59, p = 0.1132) 
#indicate that the difference in means 
#(Females: 7.19, Males: 6.59) is 
#not statistically significant. 
#A 95% confidence interval for the mean difference includes 
#0 (-0.14, 1.35), suggesting no evidence 
#of a meaningful difference between the groups.

# T-test for SCHOOLTYPE
t_test_schooltype <- t.test(DEPSCORE ~ SCHOOLTYPE)
print(t_test_schooltype)

#A Welch two-sample t-test comparing 
#depression scores by school type found 
#a statistically significant difference 
#(t(116.47) = -2.01, p = 0.0467). 
#Students in private schools (M = 6.17) had lower
#depression scores compared to students in 
#public schools (M = 7.12). The 95% confidence 
#interval for the difference in means was [-1.89, -0.01].

# T-test for RESIDENCE
t_test_residence <- t.test(DEPSCORE ~ RESIDENCE, data = training_data)
print(t_test_residence)
#A Welch two-sample t-test comparing depression 
#scores by residence found a statistically significant 
#difference (t(108.73) = -2.97, p = 0.0036). 
#Students in rural residences (M = 8.18) had higher 
#depression scores compared to students in urban residences (M = 6.68). 
#The 95% confidence interval for the difference in means was [-2.49, -0.50].


#BOX-Plot for SEX
ggplot(Group4, aes(x = factor(SEX), y = DEPSCORE, fill = factor(SEX))) +
  geom_boxplot() +
  labs(title = "Box Plot of PHQ-9 
  Depression Score by Sex",
       x = "Sex (0 = Female, 1 = Male)",
       y = "PHQ-9 Depression Score") +
  scale_fill_manual(values = c("pink", "lightblue"), labels = c("Female", "Male")) +
  theme_minimal()

#BOX-Plot for residence
ggplot(Group4, aes(x = factor(RESIDENCE), y = DEPSCORE, fill = factor(RESIDENCE))) +
  geom_boxplot() +
  labs(title = "Box Plot of PHQ-9 
  Depression Score by Residence",
       x = "Residence (0 = Urban, 1 = Rural)",
       y = "PHQ-9 Depression Score") +
  scale_fill_manual(values = c("green", "red"), labels = c("Urban", "Rural")) +
  theme_minimal()

#BOX-plot for schooltype
ggplot(Group4, aes(x = factor(SCHOOLTYPE), y = DEPSCORE, fill = factor(SCHOOLTYPE))) +
  geom_boxplot() +
  labs(title = "Box Plot of PHQ-9 
  Depression Score by School Type",
       x = "School Typpe (0 = private, 1 = public)",
       y = "PHQ-9 Depression Score") +
  scale_fill_manual(values = c("orange", "blue"), labels = c("private", "public")) +
  theme_minimal()

# Calculate the median depression scores by SEX
median_sex <- Group4 %>%
  group_by(SEX) %>%
  summarize(median_depscore = median(DEPSCORE, na.rm = TRUE))
print(median_sex)

# Calculate the median depression scores by RESIDENCE
median_residence <- Group4 %>%
  group_by(RESIDENCE) %>%
  summarize(median_depscore = median(DEPSCORE, na.rm = TRUE))
print(median_residence)

# Calculate the median depression scores by SCHOOLTYPE
median_schooltype <- Group4 %>%
  group_by(SCHOOLTYPE) %>%
  summarize(median_depscore = median(DEPSCORE, na.rm = TRUE))
print(median_schooltype)





best_model_6 <- lm(DEPSCORE ~ BMI + HEALTH + OSLO3 + AGE + factor(SEX) + factor(RESIDENCE), data = training_data)

# View the summary of the model
summary(best_model_6)

par(mfrow = c(2, 2))
plot(best_model_6)






# Quadratic regression on log-transformed values for BMI and SEX
ggplot(training_data, aes(x = log(BMI), y = log(DEPSCORE), color = factor(SEX))) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),  
    se = FALSE,
    aes(color = factor(SEX))
  ) +
  labs(
    title = "Quadratic Regression: log(Depression Score) vs log(BMI) by SEX",
    x = "log(BMI)",
    y = "log(Depression Score)",
    color = "SEX (0 = Female, 1 = Male)"
  ) +
  scale_color_manual(values = c("pink", "blue"), labels = c("Female", "Male")) +
  theme_minimal()


# Log-Quadratic Regression Plot for HEALTH
ggplot(training_data, aes(x = log(HEALTH), y = log(DEPSCORE), color = factor(SEX))) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),  # Log-Quadratic regression
    se = FALSE,
    aes(color = factor(SEX))
  ) +
  labs(
    title = "Log-Quadratic Regression: log(DEPSCORE) vs log(HEALTH) by SEX",
    x = "log(HEALTH)",
    y = "log(DEPSCORE)",
    color = "SEX (0 = Female, 1 = Male)"
  ) +
  scale_color_manual(values = c("pink", "blue"), labels = c("Female", "Male")) +
  theme_minimal()

# Log-Quadratic Regression Plot for OSLO3
ggplot(training_data, aes(x = log(OSLO3), y = log(DEPSCORE), color = factor(SEX))) +
  geom_point(alpha = 0.6) +
  geom_smooth(
    method = "lm",
    formula = y ~ x + I(x^2),  # Log-Quadratic regression
    se = FALSE,
    aes(color = factor(SEX))
  ) +
  labs(
    title = "Log-Quadratic Regression: log(DEPSCORE) vs log(OSLO3) by SEX",
    x = "log(OSLO3)",
    y = "log(DEPSCORE)",
    color = "SEX (0 = Female, 1 = Male)"
  ) +
  scale_color_manual(values = c("pink", "blue"), labels = c("Female", "Male")) +
  theme_minimal()


# Summarize the data to calculate average BMI
avg_bmi_by_depscore <- training_data |> 
  group_by(DEPSCORE) |> 
  summarize(avg_bmi = mean(BMI, na.rm = TRUE))

#plot of average BMI vs Depression score
ggplot(avg_bmi_by_depscore, aes(x = avg_bmi, y = DEPSCORE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Average BMI vs Depression Score",
    x = "Average BMI",
    y = "Depression Score"
  ) +
  theme_minimal()
# Summarize the data to calculate average Depression Score
avg_depscore_by_age <- training_data |> 
  group_by(AGE) |> 
  summarize(avg_depscore = mean(DEPSCORE, na.rm = TRUE))
avg_depscore_by_age

#plot of average Depression Score vs Age
ggplot(avg_depscore_by_age, aes(x = AGE, y = avg_depscore)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2)) +
  labs(
    title = "Average depscore vs age",
    x = "Age",
    y = "Average Depression Score"
  ) +
  theme_minimal()


# Summarize the data to calculate average Social Support
avg_OSLO3_by_depscore <- training_data |> 
  group_by(DEPSCORE) |> 
  summarize(avg_OSLO3 = mean(OSLO3, na.rm = TRUE))
avg_OSLO3_by_depscore

#plot of average Social Support vs Depression Score
ggplot(avg_OSLO3_by_depscore, aes(x = avg_OSLO3, y = DEPSCORE)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(
    title = "Average OSLO3 vs Depression Score",
    x = "Average OSLO3",
    y = "Depression Score"
  ) +
  theme_minimal()

# Load required libraries
install.packages("leaps")
install.packages("patchwork")

library(leaps)
library(patchwork)

# Perform subset regression
check_models <- regsubsets(DEPSCORE ~ BMI + factor(SEX) + factor(RESIDENCE) + HEALTH + OSLO3+AGE, 
                           data = training_data, 
                           nbest = 1)  # Find the best model for each number of predictors

# Extract metrics
results_metrics <- summary(check_models)

# Create a data frame for plotting
data_plot <- data.frame(
  adjr2 = results_metrics$adjr2,
  bic = results_metrics$bic,
  cp = results_metrics$cp,
  num_vars = 1:length(results_metrics$adjr2)  # Number of variables in each model
)

# Base ggplot
g <- ggplot(data_plot) +
  theme_minimal() +
  aes(x = num_vars) +
  scale_x_continuous(breaks = 1:length(results_metrics$adjr2)) +
  labs(x = "Number of Variables")

# Remove individual titles
g1 <- g + geom_line(aes(y = adjr2), color = "blue") + labs(y = "Adjusted R²", title = NULL)
g2 <- g + geom_line(aes(y = bic), color = "red") + labs(y = "BIC", title = NULL)
g3 <- g + geom_line(aes(y = cp), color = "green") + labs(y = "Mallows' Cp", title = NULL)

# Combine the adjusted plots
g1 + g2 + g3




# model_full is trained model
# Predict on testing data
# Full model
model_full <- lm(DEPSCORE ~ BMI + factor(SEX) + factor(RESIDENCE)+ AGE + HEALTH + OSLO3+GRADE, data = training_data)

testing_data$predicted <- predict(model_full, newdata = testing_data)

# Calculate residuals
testing_data$residuals <- testing_data$DEPSCORE - testing_data$predicted
summary(testing_data$residuals )
# Check residuals for normality

# Predict on testing data
testing_data$predicted <- predict(model_full, newdata = testing_data)


# Calculate R-squared for test data
ss_res <- sum((testing_data$DEPSCORE - testing_data$predicted)^2, na.rm = TRUE)
ss_tot <- sum((testing_data$DEPSCORE - mean(testing_data$DEPSCORE, na.rm = TRUE))^2, na.rm = TRUE)
r_squared <- 1 - (ss_res / ss_tot)
r_squared

# Calculate RMSE
rmse <- sqrt(mean((testing_data$DEPSCORE - testing_data$predicted)^2, na.rm = TRUE))
rmse

# Create the plot and annotate R-squared and RMSE
ggplot(testing_data, aes(x = predicted, y = DEPSCORE)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Observed vs. Predicted Depression Scores",
    x = "Predicted Scores",
    y = "Observed Scores"
  ) +
  annotate("text", x = max(testing_data$predicted) * 0.7, y = max(testing_data$DEPSCORE) * 0.9,
           label = paste("R-squared:", round(r_squared, 3), "\nRMSE:", round(rmse, 3)), size = 4) +
  theme_minimal()

# Baseline model (predicts mean DEPSCORE) WITHOUT COVARIATES
baseline_prediction <- mean(training_data$DEPSCORE, na.rm = TRUE)
print(baseline_prediction)

baseline_uncertainty <- sd(training_data$DEPSCORE, na.rm = TRUE)
print(baseline_uncertainty)


# Create a simplified model with SEX as the only predictor
sex_model <- lm(DEPSCORE ~ factor(SEX), data = training_data)


# Predict depression scores based on SEX
testing_data$predicted_sex <- predict(sex_model, newdata = testing_data)

# Calculate residuals
testing_data$residuals_sex <- testing_data$DEPSCORE - testing_data$predicted_sex

# Calculate accuracy metrics
# Accuracy threshold (e.g., within 5 points of true DEPSCORE)
threshold <- 5
correct_predictions_sex <- sum(abs(testing_data$residuals_sex) <= threshold, na.rm = TRUE)
total_predictions_sex <- nrow(testing_data)

# Calculate accuracy
accuracy_sex <- correct_predictions_sex / total_predictions_sex
print(paste("Accuracy of predictions using only SEX:", round(accuracy_sex * 100, 2), "%"))

# Predict depression score for a new pupil (SEX = 0 or 1)
new_pupil_sex <- data.frame(SEX = 0)  # Replace with SEX = 1 for male
predicted_depscore_sex <- predict(sex_model, newdata = new_pupil_sex)
print(paste("Predicted depression score for new pupil with SEX =", new_pupil_sex$SEX, ":", round(predicted_depscore_sex, 2)))

new_pupil_sex2 <- data.frame(SEX = 1)
predicted_depscore_sex2 <- predict(sex_model, newdata = new_pupil_sex2)
print(paste("Predicted depression score for new pupil with SEX =", new_pupil_sex2$SEX, ":", round(predicted_depscore_sex2, 2)))

testing_data$residuals_sex <- testing_data$DEPSCORE - testing_data$predicted_sex
new_pupil <- data.frame(
  BMI = 20,
  SEX = 0,
  RESIDENCE = 1,
  OSLO3 = 10,
  AGE = 16,
  HEALTH = 3
)


print(new_pupil)
prediction <- predict(best_model_6, newdata = new_pupil, interval = "confidence")

print(prediction)

#accuracy of the model
threshold <- 5
correct_predictions <- sum(abs(testing_data$DEPSCORE - testing_data$predicted) <= threshold, na.rm = TRUE)
total_predictions <- nrow(testing_data)
accuracy <- correct_predictions / total_predictions
print(accuracy)

#Predictig pupils at most risk od depression using the full model
training_data$predicted_risk <- predict(model_full,newdata=training_data)
high_risk_pupils <- training_data[order(-training_data$predicted_risk), ]
head(high_risk_pupils, n=5)









