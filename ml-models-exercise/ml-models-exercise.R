## ---- library --------
library(here) # to assist in creating a relative path
library(ggplot2) # to plot figures
library(tidymodels) # create model fits
library(dplyr) # miscellaneous data manipulation
library(tidyr) # Create tidy data
library(GGally) # made the correlation plot
library(glmnet) # help fits GLM
library(ranger) # implement Random Forests
library(yardstick) # compute metrics
library(cowplot) # organized the plots
library(gt) # Created a table

## ---- loaddata --------
# Load RDS file
data <- readRDS(here("ml-models-exercise", "combined_data.rds"))

## ---- reproducibility --------
# Set seed for reproducibility
rng = 1234
set.seed(rng)


## ---- datawrangling --------
# Mutate all occurence of '7' or '88' in RACE variable into '3'
data <- 
  data %>%
  mutate(RACE = case_when(
    RACE == "7" | RACE == "88" ~ "3",
    TRUE ~ RACE
  ))


## ---- pairwisecorrelation --------
# Selecting continuous variables and assigning them to continuous_data
continuous_data <- 
  data %>%
  select(DOSE, AGE, WT, HT)

# Changing DOSE variable into a continuous numeric variable
continuous_data$DOSE <- as.numeric(as.character(data$DOSE))

# Plot the pairwise plot 
pairs(continuous_data)

# Create a matrix of scatterplots with correlation coefficients
ggpairs(
  continuous_data,
        upper = list(continuous = wrap("cor")),
        lower = list(continuous = wrap("points")), 
        diag = list(continuous = wrap("barDiag"))
)


## ---- featureengineering --------
# I proceed to convert the height and weight variables into imperial units of lbs and inches, respectively.
data$WT <- data$WT*2.2046
data$HT <- data$HT*39.3701

# Now with the height and weight variables in imperial units, I can easily calculate and create a new variable BMI based on the formula provided from the reference shown above. 
data$BMI <- (data$WT/ (data$HT)^2)*703

## ---- recipe --------
# Creating a recipe where the outcome is Y and the predictors are all of the other variables
model_recipe <- recipe(Y ~ ., data = data) 


## ---- modelspecification --------
# Linear model with all predictors 
linear_model <- 
  linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# LASSO regression model
lasso_model <- 
  linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Random forest model
random_forest_model <- 
  rand_forest() %>%
  set_engine("ranger", seed=rng) %>%
  set_mode("regression")

## ---- workflow --------
# Setting up workflow for the linear model
lm_workflow <- 
  workflow() %>%
  add_model(linear_model) %>%
  add_recipe(model_recipe)

# Setting up workflow for the LASSO regression Model
lasso_workflow <- 
  workflow() %>%
  add_model(lasso_model) %>%
  add_recipe(model_recipe)
  
# Setting up workflow for the Random Forest regression Model
rf_workflow <- 
  workflow() %>%
  add_model(random_forest_model) %>%
  add_recipe(model_recipe)

## ---- modelfitting --------
# When attempting to fit LASSO model, an error occur regarding the variable type
# Convert all non-numeric variables to numeric
data <- sapply(data, as.numeric, na.rm = TRUE)  

# Linear model fitting
lm_fitting <- fit(lm_workflow, data)

# LASSO Model fitting
lasso_fitting <- fit(lasso_workflow, data)

# Random Forest Fitting
rf_fitting <- fit(rf_workflow, data)

## ---- modelprediction --------
# Extract the fitted linear model
fit1 <- extract_fit_parsnip(lm_fitting)

# Make predictions for the linear model and combine it to the original data 
lm_prediction <- 
  predict(fit1, new_data = data) %>%
  bind_cols(data)

# Extract the lasso regression fit 
fit2 <- extract_fit_parsnip(lasso_fitting)

# Make predictions for the LASSO regression model and combine it to the original data 
lasso_prediction <- 
  predict(fit2, new_data = data) %>%
  bind_cols(data)

# Extract the random forest fit
fit3 <- extract_fit_parsnip(rf_fitting)

# Make predictions for the random forest model and combine it to the original data 
rf_prediction <- 
  predict(fit3, new_data = data) %>%
  bind_cols(data)

## ---- modelperformance --------
# Calculate RMSE for each model
lm_rmse <- metrics(lm_prediction, truth = Y, estimate = .pred) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "Linear Model")

lasso_rmse <- metrics(lasso_prediction, truth = Y, estimate = .pred) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "LASSO Regression")

rf_rmse <- metrics(rf_prediction, truth = Y, estimate = .pred) %>%
  filter(.metric == "rmse") %>%
  mutate(model = "Random Forest")

# Combine results into a single data frame
all_rmse <- bind_rows(lm_rmse, lasso_rmse, rf_rmse) %>%
  mutate(
    Metric = .metric,
    RMSE = .estimate,
    Model = model
  ) %>%
  select(Model, RMSE)

# View the combined results
gt(all_rmse) 


## ---- modelplot --------
# Plotting the observed vs the predicted values
lm_plot <- ggplot(lm_prediction, 
       aes(x = Y, y = .pred)) +
  geom_point(aes(y = .pred), color = "black", size= 2) +
  geom_point(aes(y = Y), color = "#ff9896", size= 3)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Observed Values", y = "Predicted Values", color = "Model", shape = "Model") +
  xlim(0, 5000) + 
  ylim(0, 5000)+
  ggtitle("Linear Model Regression")+
  theme_bw()

lasso_plot <- ggplot(lasso_prediction, 
                  aes(x = Y, y = .pred)) +
  geom_point(aes(y = .pred), color = "black", size= 2) +
  geom_point(aes(y = Y), color = "#ff9896", size= 3)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Observed Values", y = "Predicted Values", color = "Model", shape = "Model") +
  xlim(0, 5000) + 
  ylim(0, 5000)+
  ggtitle("LASSO Regression")+
  theme_bw()

rf_plot <- ggplot(rf_prediction, 
                  aes(x = Y, y = .pred)) +
  geom_point(aes(y = .pred), color = "black", size= 2) +
  geom_point(aes(y = Y), color = "#ff9896", size= 3)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") + 
  labs(x = "Observed Values", y = "Predicted Values", color = "Model", shape = "Model") +
  xlim(0, 5000) + 
  ylim(0, 5000)+
  ggtitle("Random Forest Regression")+
  theme_bw()


# Arrange the plots next to each other
combined_plots <- plot_grid(lm_plot, lasso_plot, rf_plot, nrow = 1)

# View the combined plots
combined_plots

## ---- lassomodeltuning --------
# Define a grid of parameters to tune over that from 1E-5 to 1E2 with 50 values
lasso_grid <- tibble(penalty = 10^seq(-5, 2, length.out = 50))
# I seem to have an error whenever I attempt to use grid_regular, so I searched for an alternate option for this case

# LASSO model specification
lasso_model_tuned <- 
  linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# LASSO Workflow
lasso_workflow_tuned <- 
  workflow() %>%
  add_model(lasso_model_tuned) %>%
  add_recipe(model_recipe)

# Set up the grid for tuning
lasso_tuned <- 
  lasso_workflow_tuned %>%
  tune_grid(
    resamples = apparent(data), 
    grid = lasso_grid
  )

# Use autoplot to visualize tuning diagnostics
tunedlasso_wo_cv <- autoplot(lasso_tuned)
tunedlasso_wo_cv + ggtitle("Tuned LASSO Model (w/out CV)")

## ---- rfmodeltuning --------
# Repeating the same process with the Random Forest model 

# Setting up a tuning grid 
rf_grid <- grid_regular(
  mtry(range = c(1, 7)),
  min_n(range = c(1, 21)),
  levels = 7)

# Setting up the random forest model with tuned parameters
random_forest_model_tuned <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 300) %>%
  set_engine("ranger", seed=rng) %>%
  set_mode("regression")

# Random Forest workflow now tuned
rf_workflow_tuned <- 
  workflow() %>%
  add_model(random_forest_model_tuned) %>%
  add_recipe(model_recipe)

# Set up the grid for tuning
rf_tuned <- 
  rf_workflow_tuned %>%
  tune_grid(
    resamples = apparent(data), 
    grid = rf_grid
  )

# Use autoplot to visualize tuning diagnostics
rf_tuned_plot_wo_cv <- autoplot(rf_tuned)
rf_tuned_plot_wo_cv + ggtitle("Tuned RF Model (w/out CV)")

## ---- lassotuningcv --------
# Re-set the seed
set.seed(rng)

# Tuning with CV
lasso_tuned_cv <- 
  lasso_workflow_tuned %>%
  tune_grid(
    resamples = vfold_cv(data, v = 5, repeats = 5), 
    grid = lasso_grid
  )

# Use autoplot to visualize tuning diagnostics
tunedlasso <- autoplot(lasso_tuned_cv)
tunedlasso + ggtitle("Tuned LASSO Model (w/ CV)")

## ---- rftuningcv --------
# Re-set the seed
set.seed(rng)

# Set up the grid for tuning
rf_tuned_cv <- 
  rf_workflow_tuned %>%
  tune_grid(
    resamples = vfold_cv(data, v = 5, repeats = 5),  
    grid = rf_grid
  )

# Use autoplot to visualize tuning diagnostics
rf_tuned_plot <- autoplot(rf_tuned_cv)
rf_tuned_plot + ggtitle("Tuned RF Model (w/out CV)")

