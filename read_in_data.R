# Read in the data


library(tidyverse)


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

# remove the datasets which are not necessary
rm(svcgdata)
rm(orig_df)

# Recode (NA values)
loan_df$fico[loan_df$fico == 9999] <- NA
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

