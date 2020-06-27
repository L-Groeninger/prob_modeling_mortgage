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

#------------------------------Reading in the sample data ---------------------------
# It is divided into an origination file and a performance file
colnames_orig <- c(
  'fico','dt_first_pi','flag_fthb','dt_matr','cd_msa','mi_pct','cnt_units','occpy_sts','cltv',
  'dti','orig_upb','ltv','int_rt','channel','ppmt_pnlty','prod_type','st','prop_type','zipcode','id_loan',
  'loan_purpose','orig_loan_term','cnt_borr','seller_name','servicer_name','flag_sc')

orig_df <- read_delim("sample_orig_1999.txt", delim = "|", col_names = colnames_orig)

#------------------------- performance file 
svcgclass <- c(
  'character','integer','real','character','integer','integer','character','character','character',
  'integer','real','real','integer','integer','character','integer','integer','integer','integer',
  'integer','integer','real','real','character','character','real','real','real')

readfile_svcg <- function(filenm){read.table(filenm,sep="|", header=FALSE, colClasses=svcgclass)}

file_list_svcg <- list.files(path=getwd(), pattern=c("sample_svcg_1999",".txt"), full.names=F)

svcgdata <- do.call("rbind", lapply(file_list_svcg,readfile_svcg))

names(svcgdata) <- c(
  'id_loan','svcg_cycle','current_upb','delq_sts','loan_age','mths_remng','repch_flag','flag_mod','cd_zero_bal',
  'dt_zero_bal','current_int_rt','non_int_brng_upb','dt_lst_pi','mi_recoveries','net_sale_proceeds',
  'non_mi_recoveries','expenses','legal_costs','maint_pres_costs','taxes_ins_costs','misc_costs','actual_loss',
  'modcost','stepmod_ind','dpm_ind','eltv','zb_removal_upb','dlq_acrd_int')

########

# Okay, step by step
# Which loans are considered prepaid or defaulted or still active?

# prepaid = zero_balance = 01 and repurchase = "N"
# default = zero_balance = 03, 06, 09
# active = not classified as default or prepaid and latest reporting date for the loan
#          is 201906 (latest possible date) and delinquincy is 
#          not equal to R at the latest date

perf_df <- select(svcgdata, 1:11, 27:28) %>% 
  mutate(prepaid = if_else((cd_zero_bal == "01" & repch_flag == "N"), TRUE, FALSE),
         default = if_else((cd_zero_bal == "03" | cd_zero_bal == "06" |
                              cd_zero_bal == "09"), TRUE, FALSE),
         active = if_else((prepaid == FALSE & default == FALSE & 
                             svcg_cycle == 201906 & delq_sts != "R"), TRUE, FALSE))

# How many of the loans are defaulted/prepaid/active?
perf_df %>% filter(default == TRUE) %>% distinct(id_loan) %>% nrow() # 698
perf_df %>% filter(prepaid == TRUE) %>% distinct(id_loan) %>% nrow() # 48478
perf_df %>% filter(active == TRUE) %>% distinct(id_loan) %>% nrow()  # 655

# In our sample are: - 1.396% of loans defaulted (698/50000)
#                    - 96.96% of loans prepaid (48478/50000)
#                    - 1.31% of loans still active (655/50000)
# This sums up to 99.666%
# The rest of the loans has zero balance code 02 (third party sale) 
# or 15 (Note sale/reperforming sale) 
# We might remove these loans later on

# Calculate the time of prepayment/default
# Keep only the rows with the latest entry of the svcg_cycle
last_perf <- perf_df %>% 
  arrange(desc(svcg_cycle)) %>% 
  distinct(id_loan, .keep_all = TRUE) 

last_perf %>% filter(active == FALSE & default == FALSE & prepaid == FALSE) # %>% view() # 169

loan_df <- orig_df %>% 
  inner_join(last_perf) %>% 
  mutate(loan_start = as.numeric(substr(as.character(dt_first_pi), 1, 4)),
         loan_end = as.numeric(substr(as.character(svcg_cycle), 1, 4)),
         loan_age_2 = loan_end - loan_start)

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

# Recode (NA values)
loan_df$flag_fthb[loan_df$flag_fthb == "9"] <- NA
loan_df$cnt_units[loan_df$cnt_units == 99] <- NA
loan_df$occpy_sts[loan_df$occpy_sts == "9"] <- NA
loan_df$occpy_sts <- as.factor(loan_df$occpy_sts)
loan_df$dti[loan_df$dti == 999] <- NA
loan_df$ppmt_pnlty <- as.factor(loan_df$ppmt_pnlty)
loan_df$cnt_borr[loan_df$cnt_borr == "99"] <- NA
loan_df$cnt_borr <- as.factor(loan_df$cnt_borr)

loan_df <- loan_df %>% 
  mutate(mi = if_else(mi_pct == "000", 0, 1),
         flag_fthb = if_else(flag_fthb == "Y", 1, 0))

loan_df$mi_pct[loan_df$mi_pct == "999" | loan_df$mi_pct == "000"] <- NA
loan_df$mi_pct <- as.numeric(loan_df$mi_pct)
loan_df$cltv[loan_df$cltv == 999] <- NA
loan_df$prop_type <- as.factor(loan_df$prop_type)

# Filter for interesting variables
reg_df <- loan_df %>% 
  select(fico, flag_fthb, cnt_units, occpy_sts, dti, int_rt, 
         ppmt_pnlty, cnt_borr, mi, cltv, prop_type, target) 

# Create dummy variables
reg_df <- fastDummies::dummy_cols(reg_df) %>% 
  select(-occpy_sts, -ppmt_pnlty, -cnt_borr, -prop_type) %>% 
  mutate(target = as.factor(target))

logit_model <- glm(target ~ . , data = reg_df, family = binomial(link="logit"))

summary(logit_model)

reg_df %>% filter(prop_type == "MH") %>% view()
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

target <- reg_df$target

reg_df$target <- NULL
reg_df$ppmt_pnlty_NA <- NULL 
reg_df$cnt_borr_NA <- NULL

for (i in 1:ncol(reg_df)) {
  reg_df[i] <- scale(reg_df[i])
}

reg_df$target <- target

# Test the stan_glm with a smaller undersampled dataframe
balanced_df <- RandUnderClassif(target ~ . , as.data.frame(reg_df), "balance")

# The dataset is extremely unbalanced
table(reg_df$target)
table(balanced_df$target)

y <- balanced_df %>% 
  drop_na() %>% 
  pull(target)


# Specify the model using uninformative priors
model_1 <- stan_glm(target ~ ., data = balanced_df,
                  family = binomial(link = "logit"), 
                  prior = normal(0,1), prior_intercept = normal(0,1))

stan_trace(model_1, pars = names(model_1$coefficients))
posterior <- as.matrix(model_1)

plot_title <- ggtitle("Posterior distributions",
                      "with medians and 90% intervals")

bayesplot::mcmc_areas(posterior,
           prob = 0.9) + plot_title

summary(model_1)

round(coef(model_1), 2)
round(posterior_interval(model_1, prob = 0.9), 2)

# Predicted probabilities
linpred <- posterior_linpred(model_1)
preds <- posterior_linpred(model_1, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)

# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)

# 69% were classified correctly


#------------------------ ML Part ----------------------------------------
#
# Split dataset into train and test set

# For shorter calculations I use a fraction of the sample
#reg_df <- sample_frac(reg_df, 0.2)

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


save.image(file='upsampling_session.RData')













