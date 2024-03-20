## ---- packages --------
#Loading the packages needed for this exercise
library(ggplot2)
library(dplyr)
library(here)
library(tidymodels)

## ---- randomseed --------
# Set Seed for reproducibility
rngseed = 1234
set.seed(rngseed)


## ---- dataprep --------
# Set Seed for reproducibility
set.seed(rngseed)

# Select only the specified variables from the combined data (we're excluding race)
combined_subset <- combined_data %>%
  select(Y, DOSE, AGE, SEX, WT, HT) 

# View the structure of the updated dataframe
str(combined_data)

# Create training data by alloting 3/4 of the data for training
data_split <- initial_split(combined_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)

## ---- modelfitting --------
# Set Seed for reproducibility
set.seed(rngseed)

# Creating a Linear Regression model specification
lm_spec <- linear_reg() %>% 
  set_engine("lm")  %>%
  set_mode("regression")
  
  
# Fitting a linear model to the continuous outcome (Y) and DOSE
lm1 <- lm_spec %>%
  fit(Y ~ DOSE, train_data)
tidy(lm1$fit)# Viewing the model


# Fit a model to Y with all predictors
lm2 <- lm_spec %>%
  fit(Y~., data=train_data)
tidy(lm2$fit)# Viewing the model

# Creating a null model specification with parsnip
null_mod <- null_model() %>%
  set_engine("parsnip") %>% 
  set_mode("regression") %>%
  translate()

# Fit the null model to the data
null_fit <- null_mod %>%
  fit(Y ~ ., data = train_data)


## ---- modelperformance --------
# Set Seed for reproducibility
set.seed(rngseed)

# Perform prediction on lm1
lm1_predict <- predict(lm1, train_data) %>%
  bind_cols(train_data) # Combining predicted value with observed value

# Perform prediction on lm2
lm2_predict <- predict(lm2, train_data)%>%
  bind_cols(train_data) # Combining predicted value with observed value

# Predict using the null model
null_predictions <- predict(null_fit, new_data = train_data)%>%
  bind_cols(train_data) # Combining predicted value with observed value

# Calculate performance metrics of the models
metrics(lm1_predict, truth = Y, estimate = .pred)
metrics(lm2_predict, truth = Y, estimate = .pred)
metrics(null_predictions, truth = Y, estimate = .pred)

## ---- modelperformance2 --------
# For reproducibility 
set.seed(rngseed)

# Set up resampling object with cross-validation folds of 10 folds
folds <- vfold_cv(train_data, v = 10)
folds # Verifying that it was created

# Creating new model specification. 
lm_spec2 <- 
  workflow() %>% # The workflow() function initializes an empty workflow object.
  add_model(lm_spec)  # The add_model() function adds a specified model spec to the workflow
  # I'm using the linear regression spec that I added previously 

# Re-fitting the model with the resamples with DOSE as a predictor
resampled_dose <- 
  lm_spec2 %>%
  add_formula(Y ~ DOSE) %>%
  fit_resamples(folds)
resampled_dose # Viewing the results

# Re-fitting the model with the resamples with everything as a predictor
resampled_all <- 
  lm_spec2 %>%
  add_formula(Y ~ .) %>%
  fit_resamples(folds)
resampled_all # Viewing the results

# Calculating performance metrics results from resampling
collect_metrics(resampled_dose)
collect_metrics(resampled_all)

## ---- modelperformance3 --------
# Setting seed with a different value 
set.seed(5678)

# Set up resampling object with cross-validation folds of 10 folds
folds2 <- vfold_cv(train_data, v = 10)
folds2 # Verifying that it was created

# Creating new model specification. 
lm_spec3 <- 
  workflow() %>% # The workflow() function initializes an empty workflow object.
  add_model(lm_spec)  # The add_model() function adds a specified model spec to the workflow
# I'm using the linear regression spec that I added previously 

# Re-fitting the model with the resamples with DOSE as a predictor
resampled_dose2 <- 
  lm_spec3 %>%
  add_formula(Y ~ DOSE) %>%
  fit_resamples(folds2)
resampled_dose2 # Viewing the results

# Re-fitting the model with the resamples with everything as a predictor
resampled_all2 <- 
  lm_spec3 %>%
  add_formula(Y ~ .) %>%
  fit_resamples(folds2)
resampled_all2 # Viewing the results

# Calculating performance metrics results from resampling
collect_metrics(resampled_dose2)
collect_metrics(resampled_all2)

