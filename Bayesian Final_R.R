# OLS Approaching
library(MASS)
library(leaps)
library(olsrr)

# Read and Prepare the data
housing_clean <- read.csv("/Users/sheila/Desktop/632Bayesian/project/house.csv", row.names = 1)
housing_clean$ocean_proximity <- as.factor(housing_clean$ocean_proximity)

# Fitting the model
full_model <- lm(median_house_value ~ housing_median_age + total_bedrooms + median_income + ocean_proximity, data = housing_clean)
summary(full_model)
AIC <- AIC(full_model)
BIC <- BIC(full_model)

# Model Selection
reduced_model <- stepAIC(full_model, direction = "backward")

full_model2 <- lm(median_house_value ~ housing_median_age + total_rooms + median_income + ocean_proximity, data = housing_clean)
summary(full_model2) 
AIC2 <- AIC(full_model2)
BIC2 <- BIC(full_model2)
reduced_model2 <- stepAIC(full_model2, direction = "backward")

full_model3 <- lm(median_house_value ~ housing_median_age + population + median_income + ocean_proximity, data = housing_clean)
summary(full_model3) 
AIC3 <- AIC(full_model3)
BIC3 <- BIC(full_model3)
reduced_model3 <- stepAIC(full_model3, direction = "backward")

full_model4 <- lm(median_house_value ~ housing_median_age + households + median_income + ocean_proximity, data = housing_clean)
summary(full_model4)
AIC4 <- AIC(full_model4)
BIC4 <- BIC(full_model4)
reduced_model4 <- stepAIC(full_model4, direction = "backward")

AIC_all <- c(AIC,AIC2,AIC3,AIC4)
BIC_all <- c(BIC,BIC2,BIC3,BIC4)
AIC_all
BIC_all


# Bayesian Linear Regression Approaching
# Read in and Clean data
data <- read.csv("D:/bayesian/project/house.csv")
numeric_columns <- sapply(data, is.numeric)
data[numeric_columns] <- scale(data[numeric_columns])
data$ocean_proximity <- as.factor(data$ocean_proximity)

# Split data into Train and Test
set.seed(123) 
library(caret)
splitIndex <- createDataPartition(data$median_house_value, p = .80, list = FALSE, times = 1)
train <- data[ splitIndex,]
test  <- data[-splitIndex,]

model_formula <- as.formula("median_house_value ~ housing_median_age + total_bedrooms + ocean_proximity + median_income")
ols_fit <- lm(model_formula, data = train)
beta_ols <- coef(ols_fit) 
sigma2_ols <- sum(residuals(ols_fit)^2) / ols_fit$df.residual  

result <- MCMCregress(model_formula, data = train, n.iter = 10000, burnin = 1000,
                      b0 = beta_ols, B0 = diag(rep(sigma2_ols, length(beta_ols))),
                      c0 = 1, d0 = sigma2_ols)
summary(result)

# Preliminary Evaluation Plot
mcmc_result <- as.mcmc(result)
par(mfrow=c(2, 3))
traceplot(mcmc_result)

# Evaluation the model by Test dataset
posterior_means <- colMeans(mcmc_result)
test1 <- model.matrix(~ ocean_proximity - 1, data = test)
test <- cbind(test[, !names(test) %in% 'ocean_proximity'], test1)

predict_new <- posterior_means[1] +
          posterior_means[2] * test$median_house_value +
          posterior_means[3] * test$total_bedrooms +
          posterior_means[8] * test$median_income +
          posterior_means[4] * test$ocean_proximityINLAND +
          posterior_means[5] * test$ocean_proximityISLAND +
          posterior_means[6] * test$`ocean_proximityNEAR BAY` +
          posterior_means[7] * test$`ocean_proximityNEAR OCEAN`

noise_test <- rnorm(n = nrow(test), mean = 0, sd = sqrt(posterior_means[9]))
posterior_predictive_test <- predict_new + noise_test
observed_values_test <- test$median_house_value
residuals_test <-observed_values_test - posterior_predictive_test

par(mfrow=c(1, 2))
plot(residuals_test)
abline(h = 0, col = "red")

plot(observed_values_test, residuals_test)
abline(h = 0, col = "red")
