# Freddie Mac Loan data
#
# Author: Lukas Gröninger
# Date: 04.06.2020

# Goal: Explain which loans are being prepaid and which are defaulting
# Method: logistic regression for the beginning

# required libraries
library(tidyverse)
library(tidymodels)
library(ranger)
library(themis)
library(precrec)
library(rstanarm)
library(DMwR)
library(bayesplot)
library(rstan)

# Theme_set for plots
theme_set(bayesplot::theme_default(base_family = "sans"))

# Read in the data
source("read_in_data.R")

# Reproduce the descriptive plot from the paper (p. 12)
plot_data <- loan_df %>% 
  select(loan_age_2, active, default, prepaid) %>% 
  filter(!(active == FALSE & default == FALSE & prepaid == FALSE)) %>% 
  pivot_longer(2:4) %>% 
  filter(value == TRUE)

ggplot(plot_data, aes(x = loan_age_2, fill = name, color = name)) +
  geom_histogram(bins = 22, position = "identity", alpha = 0.5) +
  labs(x = "Time in years", y = "count", fill = "Mortgage status") +
  guides(color = FALSE)

# Another plot showing the density to better compare defaulted and prepaid mortgages
filter(plot_data, name != "active") %>% 
  ggplot(aes(x = loan_age_2, fill = name, color = name)) +
  geom_histogram(aes(y = ..density..), bins = 22, alpha = 0.5, position = "identity") +
  labs(x = "Time in years", y = "density", fill = "Mortgage status") +
  guides(color = FALSE)
# looks like a lognormal distribution 

# Compare this distribution with values from the paper
# Values from the paper:
mu_default <- 2.817
sd_default <- 0.963

mu_prepaid <- 1.578
sd_prepaid <- 0.717

# Plot it again now with lognormal distribution curves
filter(plot_data, name != "active") %>% 
  ggplot(aes(x = loan_age_2, fill = name, color = name)) +
  geom_histogram(aes(y = ..density..), bins = 22, alpha = 0.5, position = "identity") +
  stat_function(fun = dlnorm, args = list(meanlog = mu_default, sdlog = sd_default), 
                colour = "red", size = 1) +
  stat_function(fun = dlnorm, args = list(meanlog = mu_prepaid, sdlog = sd_prepaid), 
                colour = "#00AFBB", size = 1) +
  labs(x = "Time in years", y = "density", fill = "Mortgage status") +
  guides(color = FALSE)


# --------------------- Regression -------------------------------
# Start with a fast logistic regression to explain mortgage default
#
# Removing the mortgages with Status active and recode the mortgage status
# to be 0 for prepaid and 1 for default

# Problem: Highly unbalanced dataset

# filter and prepare the dataset

loan_df <- loan_df %>% 
  filter(!(active == FALSE & default == FALSE & prepaid == FALSE)) %>% 
  filter(active == FALSE) %>% 
  mutate(target = if_else(prepaid == TRUE, 0, 1))

# So it is either prepaid or defaulted

# Filter for interesting variables
reg_df <- loan_df %>% 
  select(fico, flag_fthb, cnt_units, occpy_sts, dti, int_rt, mi, cltv, prop_type, target) 

# Create dummy variables
reg_df <- fastDummies::dummy_cols(reg_df) %>% 
  select(-occpy_sts, -prop_type) %>% 
  mutate(target = as.factor(target))

logit_model <- glm(target ~ . , data = reg_df, family = binomial(link="logit"))

summary(logit_model)

# reg_df %>% filter(prop_type == "MH") %>% view()
# 15% (15/149) of manufactured housing mortgages (MH) are defaulted

# Interestingly the fico value (credit score is not significant)
# Significant variables (p<0.05): 
# a higher debt-to-income ratio (as expected) increases the odds of default
# a higher interest rate increases the odds of default
# number of borrowers >1 reduces the odds of default

# Further steps: 
# Split in train and test set, 
# continue to evaluate the model
# use different machine learning algorithms (especially for highly unbalanced datasets)

#------------------------- Bayesian logistic regression ----------------------
#
# Bayesian logistic regression with rstanarm

reg_df <- reg_df %>% drop_na()

target <- reg_df$target

reg_df$target <- NULL
reg_df$ppmt_pnlty_NA <- NULL 
reg_df$cnt_borr_NA <- NULL

for (i in 1:ncol(reg_df)) {
  reg_df[i] <- scale(reg_df[i])
}

reg_df$target <- target

# Test the stan_glm with a smaller undersampled dataframe
balanced_df <- UBL::RandUnderClassif(target ~ . , as.data.frame(reg_df), "balance")

# The dataset is extremely unbalanced
table(reg_df$target)
table(balanced_df$target)

y <- as.numeric(balanced_df$target) - 1

# Specify the model using uninformative priors
model_1 <- stan_glm(target ~ ., data = balanced_df,
                  family = binomial(link = "logit"), 
                  prior = normal(0,1), prior_intercept = normal(0,1))

stan_trace(model_1, pars = names(model_1$coefficients))
posterior <- as.matrix(model_1)

summary(model_1)

round(coef(model_1), 2)
round(posterior_interval(model_1, prob = 0.9), 2)

p <- plot(model_1, pars = names(model_1$coefficients),
          prob = 0.5, prob_outer = 0.9)
p + ggplot2::ggtitle("Posterior medians \n with 50% and 90% intervals")

bayesplot::color_scheme_set("red")
plot(model_1, "acf", pars = names(model_1$coefficients))


# Predicted probabilities
linpred <- posterior_linpred(model_1)
preds <- posterior_linpred(model_1, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)

# 73% were classified correctly


#------------------------ ML Part ----------------------------------------
#
# Split dataset into train and test set

# Prepare the dataframe (with NA's)
reg_df <- loan_df %>% 
  select(fico, flag_fthb, cnt_units, occpy_sts, dti, int_rt, mi, cltv, prop_type, target) 

# Create dummy variables
reg_df <- fastDummies::dummy_cols(reg_df) %>% 
  select(-occpy_sts, -prop_type) %>% 
  mutate(target = as.factor(target))

set.seed(12345)
# split the data into trainng (75%) and testing (25%)
reg_df_split <- initial_split(reg_df, prop = 3/4)

# extract training and testing sets
reg_train <- training(reg_df_split)
reg_test <- testing(reg_df_split)

# create CV (cross validation) object from training data
reg_cv <- vfold_cv(reg_train)

# define the recipe
ml_recipe <- 
  # which consists of the formula (outcome ~ predictors)
  recipe(target ~ . , data = reg_df) %>%
  # and some pre-processing steps
  step_normalize(all_numeric()) %>%  # normalize the variables
  step_knnimpute(all_predictors()) %>% # use k nearest neighbour imputation
  step_smote(target, over_ratio = 0.3) %>% # Synthetic Minority Over-sampling Technique (smote)
  #step_nearmiss(target, under_ratio = 5) %>% # Under sampling
  #step_rose(target, over_ratio = 0.3) %>% 
  prep()

#--------------------------- Random Forest model ---------------------------------

rf_model <- 
  # specify that the model is a random forest
  rand_forest() %>%
  # specify that the `mtry` parameter needs to be tuned
  set_args(mtry = tune()) %>%
  # select the engine/package that underlies the model
  set_engine("ranger", importance = "impurity_corrected") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification") 


# set the workflow
rf_workflow <- workflow() %>%
  # add the recipe
  add_recipe(ml_recipe) %>%
  # add the model
  add_model(rf_model)


# specify which values meant to try
rf_grid <- expand.grid(mtry = c(2, 3, 4))
# extract results
rf_tune_results <- rf_workflow %>%
  tune_grid(resamples = reg_cv, #CV object
            grid = rf_grid, # grid of values to try
            metrics = metric_set(accuracy, roc_auc)) # metrics we care about
  
# print results
rf_tune_results %>%
  collect_metrics()

param_final <- rf_tune_results %>%
  select_best(metric = "accuracy")
# mtry = 2 yields the best results

# Add this parameter to the workflow
rf_workflow <- rf_workflow %>%
  finalize_workflow(param_final)

rf_fit <- rf_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(reg_df_split)

test_performance <- rf_fit %>% collect_metrics()
test_performance

# generate predictions from the test set
test_predictions <- rf_fit %>% collect_predictions()
# Change the probability threshold
test_predictions <- test_predictions %>% 
  mutate(.pred_class_2 = as.factor(if_else(.pred_1 >= 0.3, "1", "0")))

# generate a confusion matrix
rf_conf_mat <- test_predictions %>% 
  conf_mat(truth = target, estimate = .pred_class_2)
# quite bad...
summary(rf_conf_mat)

ggplot2::autoplot(rf_conf_mat, type = "heatmap")

test_predictions %>%
  ggplot() +
  geom_density(aes(x = .pred_1, fill = target), 
               alpha = 0.5)
# low probabilities for defaulted mortgages (target = 1)

final_model <- fit(rf_workflow, reg_df)

ranger_obj <- pull_workflow_fit(final_model)$fit
ranger_obj

ranger_obj$variable.importance

#---------------------- logistic regression model --------------------------

# Define the model
lr_model <- 
  # specify that the model is a logistic regression
  logistic_reg() %>%
  # select the engine/package that underlies the model
  set_engine("glm") %>%
  # choose either the continuous regression or binary classification mode
  set_mode("classification")

# set the logistic regression workflow
lr_workflow <- workflow() %>%
  # add the recipe
  add_recipe(ml_recipe) %>%
  # add the model
  add_model(lr_model)

lr_fit <- lr_workflow %>%
  # fit on the training set and evaluate on test set
  last_fit(reg_df_split)

lr_test_performance <- lr_fit %>% collect_metrics()
lr_test_performance

# generate predictions from the test set
lr_test_predictions <- lr_fit %>% collect_predictions()
lr_test_predictions <- lr_test_predictions %>% 
  mutate(.pred_class_2 = as.factor(if_else(.pred_1 >= 0.3, "1", "0")))

# generate a confusion matrix
lr_conf_mat <- lr_test_predictions %>% 
  conf_mat(truth = target, estimate = .pred_class_2)
# again more or less the same picture: bad specificity
summary(lr_conf_mat)

lr_test_predictions %>%
  ggplot() +
  geom_density(aes(x = .pred_1, fill = target), 
               alpha = 0.5)

# Plot the roc curves
# for logistic regression
roc_logistic <- evalmod(scores = as.numeric(lr_test_predictions$.pred_class_2),
                        labels = lr_test_predictions$target)
autoplot(roc_logistic)

# for random forest model
roc_forest <- evalmod(scores = as.numeric(test_predictions$.pred_class_2),
                        labels = test_predictions$target)
autoplot(roc_forest)

# save.image(file='upsampling_session.RData')













