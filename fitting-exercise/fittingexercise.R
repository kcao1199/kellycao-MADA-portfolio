## ---- packages --------
library(ggplot2)
library(dplyr)
library(here)
library(kableExtra)
library(tidymodels)
library(carat)
library(pROC)

## ---- loaddata --------
#Reading the data into RStudio
data <- read.csv(here("./fitting-exercise/Mavoglurant_A2121_nmpk.csv"))

#Reviewing the data with summary() and str()
summary(data)
str(data)


## ---- Cleaning --------
# Adjusting the column to factor for clarity when plotted
data$DOSE <- as.factor(data$DOSE)

# Plotted DV vs Time, stratified by dose
ggplot(data, aes(x = TIME, y = DV, color = DOSE)) +
  geom_line() +
  labs(x = "Time", y = "DV", color = "DOSE") +
  theme_minimal()

# Using Facet to show the difference
ggplot(data, aes(x = TIME, y = DV, color = DOSE)) +
  geom_line() +
  facet_wrap(~DOSE) +
  labs(x = "Time", y = "DV", color = "Dose") +
  theme_minimal()

# Removing entries of OCC=2
data <- data %>%
  filter(OCC !=2)

#Confirming OCC=2 is removed
summary(data$OCC)

# Filter out any observation of Time=0
data_wo_time0 <- data %>%
  filter(TIME != 0)
summary(data_wo_time0) 

# Summarization of DV Variable (observed value)
summarized_data <- data_wo_time0 %>%
  group_by(ID) %>%
  summarize(Y = sum(DV))
summary(summarized_data) # Frame with 120x2

# Creating Data Frame where Time==0
Time0_df <- data %>%
  filter(TIME == 0)
summary(Time0_df) # Frame with 120 x 17

# Combining the two data frames with join function
combined_data <- left_join(summarized_data, Time0_df, by = "ID")
combined_data # Frame with 120 x 18

combined_data <- combined_data %>%
  mutate(RACE = factor(RACE),
         SEX = factor(SEX)) %>%
  select(Y, DOSE, AGE, SEX, RACE, WT, HT)

## ---- DataExploration --------
# Producing a summary statistics of variable Y
summary_stats <- combined_data %>%
  group_by(DOSE) %>%
  summarise(
    Mean_Y = mean(Y),
    Median_Y = median(Y),
    Min_Y = min(Y),
    Max_Y = max(Y),
    SD_Y = sd(Y)
  )
summary_stats

# Creating a table of the dataframe with kable package
summary_stats %>%
  kable() %>%
  kable_styling(full_width = FALSE) %>%
  add_header_above(c(" " = 1, "Y Summary Statistics" = 5))
kable(summary_stats)

# Plotting Y to show Frequency Distribution
ggplot(combined_data, aes(x = Y)) +
  geom_histogram(fill = "skyblue", color = "black") +
  labs(x = "Y", y = "Frequency") +
  theme_minimal()

# Plotting Age to show Frequency Distribution
ggplot(combined_data, aes(x=AGE))+
  geom_histogram(fill = "skyblue", color = "black") +
  labs(x = "Age", y = "Frequency") +
  theme_minimal()

# Plotting Weight to show Frequency Distribution
ggplot(combined_data, aes(x=WT))+
  geom_histogram(fill = "skyblue", color = "black") +
  labs(x = "Weight", y = "Frequency") +
  theme_minimal()

# Plotting Height to show Frequency Distribution
ggplot(combined_data, aes(x=HT))+
  geom_histogram(fill = "skyblue", color = "black") +
  labs(x = "Height", y = "Frequency") +
  theme_minimal()

# Determining the distribution of the Dose factorand plotting it as a bar graph
table(combined_data$DOSE)
ggplot(combined_data, aes(x=DOSE, fill=DOSE))+
  labs(x = "Dose", y = "Frequency")+
  geom_bar()

# Determining the distribution of the Sex factor and plotting it as a bar graph
table(combined_data$SEX)
ggplot(combined_data, aes(x=SEX, fill=SEX))+ 
  labs(x = "Sex", y = "Frequency")+
  geom_bar()

#Determining the distribution of the RACE variable and plotting it as a bar graph
table(combined_data$RACE)
ggplot(combined_data, aes(x=RACE, fill=RACE))+ 
  labs(x = "Race",  y = "Frequency")+
  geom_bar()

## ---- Visual --------

# Scatterplot of Y vs other numerical predictors
ggplot(combined_data, aes(x = AGE, y = Y)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(X = "Age", y = "Sum of DV")

# Y Vs Weight
ggplot(combined_data, aes(x = WT, y = Y, color=DOSE)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(X = "Weight", y = "Sum of DV")

# Y Vs Height
ggplot(combined_data, aes(x = HT, y = Y, color = DOSE)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(X = "Height", y = "Sum of DV")

# Boxplot of Y by SEX
ggplot(combined_data, aes(x = SEX, y = Y)) +
  geom_boxplot()

# Boxplot of Y by RACE
ggplot(combined_data, aes(x = RACE, y = Y)) +
  geom_boxplot()

# Boxplot of Y by DOSE
ggplot(combined_data, aes(x = DOSE, y = Y)) +
  geom_boxplot()

# Pair/Correlation plots of the numerical variables
# Pair plot of selected variables
pairplot <- pairs(combined_data[, c("Y", "AGE", "WT", "HT")])

# Correlation matrix
correlation_matrix <- cor(combined_data[, c("Y", "AGE", "WT", "HT")])


## ---- Model --------
# Creating a Linear Regression model specification
lm_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Fitting a linear model to the continuous outcome (Y) and DOSE
lm_fit <- lm_spec %>%
  fit(Y ~ DOSE, combined_data)

# Sharing the result and summary of the fit
summary(lm_fit$fit)

# Fit linear model using all predictors
full_linear_model <- lm_spec %>%
  fit(Y ~ ., combined_data)

# Print summary of the linear model
summary(full_linear_model$fit)

# For the model with DOSE as the predictor
dose_predictions <- predict(lm_fit$fit)
dose_rmsd <- sqrt(mean((combined_data$Y - dose_predictions)^2))

# For the model with all predictors
full_predictions <- predict(full_linear_model$fit)
full_rmsd <- sqrt(mean((combined_data$Y - full_predictions)^2))

# For the model with DOSE as the predictor
dose_r_squared <- summary(lm_fit$fit)$r.squared

# For the model with all predictors
full_r_squared <- summary(full_linear_model$fit)$r.squared

#Printing R squared value and RMSD
print(dose_rmsd)
print(full_r_squared)
print(dose_r_squared)
print(full_rmsd)

# Fit a logistic model using the main predictor (DOSE)
logistic_dose <- glm(SEX ~ DOSE, data = combined_data, family = binomial)
summary(logistic_dose)

# Fit a logistic model using all predictors
logit_all <- glm(SEX ~ ., data = combined_data, family = binomial)
summary(logit_all)

# Predictions for the model using the main predictor (DOSE)
y_pred_main <- ifelse(predict(logistic_dose, newdata = combined_data, type = "response") > 0.5, 1, 0)

# Predictions for the model using all predictors
y_pred_all <- ifelse(predict(logit_all, newdata = combined_data, type = "response") > 0.5, 1, 0)

# True labels from the test data
y_true <- combined_data$SEX

# Compute accuracy
accuracy_main <- mean(y_pred_main == y_true)
accuracy_all <- mean(y_pred_all == y_true)
print(accuracy_main)
print(accuracy_all)

# Compute ROC-AUC
roc_auc_main <- roc(y_true, predict(logistic_dose, newdata = combined_data, type = "response"))$auc
roc_auc_all <- roc(y_true, predict(logit_all, newdata = combined_data, type = "response"))$auc
print(roc_auc_main)
print( roc_auc_all)

