# Freddie Mac Loan data
#
# Author: Lukas Gröninger
# Date: 04.06.2020

# Goal: Explain which loans are being prepaid and which are defaulting
# Method: logistic regression for the beginning

# required libraries
library(tidyverse)


# Theme_set for plots
theme_set(sjPlot::theme_sjplot())

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

# Recode NA values
loan_df$flag_fthb[loan_df$flag_fthb == "9"] <- NA
loan_df$flag_fthb <- as.factor(loan_df$flag_fthb)
loan_df$cnt_units[loan_df$cnt_units == 99] <- NA
loan_df$occpy_sts[loan_df$occpy_sts == "9"] <- NA
loan_df$occpy_sts <- as.factor(loan_df$occpy_sts)
loan_df$dti[loan_df$dti == 999] <- NA
loan_df$ppmt_pnlty <- as.factor(loan_df$ppmt_pnlty)
loan_df$cnt_borr[loan_df$cnt_borr == "99"] <- NA
loan_df$cnt_borr <- as.factor(loan_df$cnt_borr)

# Filter for interesting variables
log_df <- loan_df %>% 
  select(fico, flag_fthb, cnt_units, occpy_sts, dti, int_rt, 
         ppmt_pnlty, cnt_borr, target)

logit_model <- glm(target ~ . , data = log_df, family = binomial(link="logit"))

summary(logit_model)

# Interestingly the fico value (credit score is not significant)
# Significant variables (p<0.05): 
# a higher debt-to-income ratio (as expected) increases the odds of default
# a higher interest rate increases the odds of default
# number of borrowers >1 reduces the odds of default

# Further steps: 
# Split in train and test set, 
# continue to evaluate the model
# use different machine learning algorithms (especially for highly unbalanced datasets)



