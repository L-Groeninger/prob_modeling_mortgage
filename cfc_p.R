# Trying different packages
#
#

library(CFC)
library(BSGW)
library(survival)
library(skimr)


source("read_in_data.R")

# prepare data and formula for Bayesian cause-specific survival regression
# using R package BSGW
df <- loan_df %>% 
  filter(!(active == FALSE & default == FALSE & prepaid == FALSE)) %>% 
  select(loan_age_2, fico, cltv, active, prepaid, default) %>% 
  mutate(cause = case_when(prepaid == TRUE ~ 1,
                            default == TRUE ~ 2,
                            TRUE ~ 0),
         fico = scale(fico),
         cltv = scale(cltv)) %>% 
  dplyr::rename(time = loan_age_2) %>% 
  select(time, cause, fico, cltv) %>% 
  drop_na() %>% 
  as.data.frame() 
  

table(bmt$cause)

bmt <- df %>% 
  sample_n(1000)

# splitting data into training and prediction sets
idx.train <- sample(1:nrow(bmt), size = 0.7 * nrow(bmt))
idx.pred <- setdiff(1:nrow(bmt), idx.train)
nobs.train <- length(idx.train)
nobs.pred <- length(idx.pred)

# prepare data and formula for Bayesian cause-specific survival regression
# using R package BSGW
out.prep <- cfc.prepdata(formul = survival::Surv(time, cause) ~ fico + cltv, dat = bmt)
f1 <- out.prep$formula.list[[1]]
f2 <- out.prep$formula.list[[2]]
dat <- out.prep$dat
tmax <- out.prep$tmax

# estimating cause-specific models
# set nsmp to larger number in real-world applications
nsmp <- 10
reg1 <- bsgw(f1, dat[idx.train, ], control = bsgw.control(iter = nsmp)
             , ordweib = T, print.level = 0)
reg2 <- bsgw(f2, dat[idx.train, ], control = bsgw.control(iter = nsmp)
             , ordweib = T, print.level = 0)

# defining survival function for this model
survfunc <- function(t, args, n) {
  nobs <- args$nobs; natt <- args$natt; nsmp <- args$nsmp
  alpha <- args$alpha; beta <- args$beta; X <- args$X
  idx.smp <- floor((n - 1) / nobs) + 1
  idx.obs <- n - (idx.smp - 1) * nobs
  return (exp(- t ^ alpha[idx.smp] * 
                exp(sum(X[idx.obs, ] * beta[idx.smp, ]))));
}

# preparing function and argument lists
X.pred <- as.matrix(cbind(1, bmt[idx.pred, c("fico", "cltv")]))
arg.1 <- list(nobs = nobs.pred, natt = 4, nsmp = nsmp
              , alpha = exp(reg1$smp$betas), beta = reg1$smp$beta, X = X.pred)
arg.2 <- list(nobs = nobs.pred, natt = 4, nsmp = nsmp
              , alpha = exp(reg2$smp$betas), beta = reg2$smp$beta, X = X.pred)
arg.list <- list(arg.1, arg.2)
f.list <- list(survfunc, survfunc)

# cause-specific competing-risk
# set rel.tol to smaller number in real-world applications
tout <- seq(from = 0.0, to = tmax, length.out = 10)
out.cfc <- cfc(f.list, arg.list, nobs.pred * nsmp, tout, rel.tol = 1e-2)
    
mtcars %>% 
  mutate(cg = case_when(carb <= 2 ~ "low",
                        carb > 2  ~ "high"))

out.prep <- cfc.prepdata(Surv(time, cause) ~ fico + cltv, bmt)
f1 <- out.prep$formula.list[[1]]
f2 <- out.prep$formula.list[[2]]
dat <- out.prep$dat
tmax <- out.prep$tmax



table(bmt$cause)

data("bmt")
class(bmt)
# splitting data into training and prediction sets
idx.train <- sample(1:nrow(bmt), size = 0.7 * nrow(bmt))
idx.pred <- setdiff(1:nrow(bmt), idx.train)
nobs.train <- length(idx.train)
nobs.pred <- length(idx.pred)

# prepare data and formula for Bayesian cause-specific survival regression
# using R package BSGW
out.prep <- cfc.prepdata(Surv(time, cause) ~ platelet, bmt)
f1 <- out.prep$formula.list[[1]]
f2 <- out.prep$formula.list[[2]]
dat <- out.prep$dat
tmax <- out.prep$tmax

# estimating cause-specific models
# set nsmp to larger number in real-world applications
nsmp <- 10
reg1 <- bsgw(f1, dat[idx.train, ], control = bsgw.control(iter = nsmp)
             , ordweib = T, print.level = 0)
reg2 <- bsgw(f2, dat[idx.train, ], control = bsgw.control(iter = nsmp)
             , ordweib = T, print.level = 0)

# defining survival function for this model
survfunc <- function(t, args, n) {
  nobs <- args$nobs; natt <- args$natt; nsmp <- args$nsmp
  alpha <- args$alpha; beta <- args$beta; X <- args$X
  idx.smp <- floor((n - 1) / nobs) + 1
  idx.obs <- n - (idx.smp - 1) * nobs
  return (exp(- t ^ alpha[idx.smp] * 
                exp(sum(X[idx.obs, ] * beta[idx.smp, ]))));
}

# preparing function and argument lists
X.pred <- as.matrix(cbind(1, bmt[idx.pred, c("platelet")]))
arg.1 <- list(nobs = nobs.pred, natt = 4, nsmp = nsmp
              , alpha = exp(reg1$smp$betas), beta = reg1$smp$beta, X = X.pred)
arg.2 <- list(nobs = nobs.pred, natt = 4, nsmp = nsmp
              , alpha = exp(reg2$smp$betas), beta = reg2$smp$beta, X = X.pred)
arg.list <- list(arg.1, arg.2)
f.list <- list(survfunc, survfunc)

# cause-specific competing-risk
# set rel.tol to smaller number in real-world applications
tout <- seq(from = 0.0, to = tmax, length.out = 10)
out.cfc <- cfc(f.list, arg.list, nobs.pred * nsmp, tout, rel.tol = 1e-2)





