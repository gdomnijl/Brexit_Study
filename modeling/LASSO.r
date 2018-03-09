## Check if dataset is synchronized
library(tidyverse)
library(caret)
library(glmnet)
library(Amelia)
jinlin_dat <- read.csv("opinion_switch.csv")
an_dat <- read.csv("data/switches_calculated.csv")

jinlin_dat$switch_ratio <- plyr::round_any(jinlin_dat$switch_ratio,0.0001, f = round)
an_dat$proportion_switches <- plyr::round_any(an_dat$proportion_switches,0.0001, f = round)

diff <- an_dat %>%
  select(-X) %>%
  inner_join(jinlin_dat) %>%
  filter(switch_ratio != proportion_switches) 

## splitting into test/training set
set.seed(1234)

trainIndex <- createDataPartition(switch_dat$switch_ratio, 
                                  p = 0.75, list = FALSE,times = 1)


train_dat <- switch_dat[trainIndex,]
test_dat <- switch_dat[-trainIndex,]


write.csv(train_dat, "data/training_switch_ratio.csv")
write.csv(test_dat, "data/test_switch_ratio.csv")


trainIndex2 <- createDataPartition(switch_dat$ifswitch, 
                                  p = 0.75, list = FALSE,times = 1)
train_dat_ifswitch <- switch_dat[trainIndex2,]
test_dat_ifswitch <- switch_dat[-trainIndex2,]
write.csv(train_dat_ifswitch, "data/training_ifswitch.csv")
write.csv(test_dat_ifswitch, "data/test_ifswitch.csv")

# ------------------------------------------------------------------------------------------

## Exam NAs
#missmap(train_ratio)

## Modeling:
factor<-read.csv("data/dataset_static_factors.csv") 
wave_index<-grep("^wave\\d+$",names(factor))
factor <- factor %>% 
     select(-euRefLA, -onscode,-ageGroup,-X.1,-X,-num_wave_present,
            -num_switch,-switch_ratio,-ifswitch, -wave_index,
            -fatherNumEmployees,-motherNumEmployees, 
            -profile_socialgrade_cie,-profile_lea,-profile_oslaua)
## on switch ratio:
train_ratio <- read.csv("data/training_switch_ratio.csv") %>% 
  select(id,switch_ratio) %>%
  inner_join(factor)

## change to factor 
for(i in 1:length(names(train_ratio))){
  if(!(names(train_ratio)[i] %in% c("switch_ratio","Age",
                                "profile_household_children", 
                                "profile_gross_personal",
                                "profile_gross_household",
                                "profile_educaiton_age"))){
      train_ratio[,i] <- as.factor(train_ratio[,i])
  }
}
  

## LASSO:
f <- reformulate(response=NULL, termlabels=names(train_ratio)[-1],intercept =FALSE)
options(na.action='na.pass')
train_ratio_factor <- model.matrix(f, train_ratio)  ## CHECK: DID IT JUST ELIMINATE ANY NA???
## TODO: Need to deal with NAs
##  only left with 9043 without NAs
## DECISION: Replaced all NAs with 0s
train_ratio_factor[is.na(train_ratio_factor)] <- 0

fit <- glmnet(x = train_ratio_factor[,-1], y = train_ratio_factor[,1],
              alpha = 1, nlambda = 10, standardize = TRUE)
plot(fit, xvar = "lambda", label = TRUE)
plot(fit, xvar = "dev", label = TRUE)
coef(fit, s = 0.001)


ridge <- glmnet(x = train_ratio_factor[,-1], y = train_ratio_factor[,1],
              alpha = 0, nlambda = 10, standardize = TRUE)

# NOTES:
# Df (the number of nonzero coefficients), 
# %dev (the percent deviance explained) and 
# Lambda (the corresponding value of λ).

#----------------------------------------------------------------------------------------
## on Ifswitch:
train_ifswitch <- read.csv("data/training_ifswitch.csv") %>% 
  select(id,ifswitch) %>%
  inner_join(factor)

## change to factor 
for(i in 1:length(names(train_ifswitch))){
  if(!(names(train_ifswitch)[i] %in% c("switch_ratio","Age",
                                    "profile_household_children", 
                                    "profile_gross_personal",
                                    "profile_gross_household",
                                    "profile_educaiton_age"))){
    train_ifswitch[,i] <- as.factor(train_ifswitch[,i])
  }
}


## LASSO:
f2 <- reformulate(response=NULL, termlabels=names(train_ifswitch)[-c(1,2)],intercept =FALSE)
options(na.action='na.pass')
train_ifswitch_factor <- model.matrix(f2, train_ifswitch)  ## CHECK: DID IT JUST ELIMINATE ANY NA???
## TODO: Need to deal with NAs
##  only left with 9043 without NAs
## DECISION: Replaced all NAs with 0s
train_ifswitch_factor[is.na(train_ifswitch_factor)] <- 0

fit2 <- glmnet(x = train_ifswitch_factor, y = train_ifswitch[,2],
              alpha = 1, nlambda = 10, standardize = TRUE, family = "binomial")
plot(fit2, xvar = "lambda", label = TRUE)
plot(fit2, xvar = "dev", label = TRUE)
coef(fit2, s = 0.001)

# TODO: correlation analysis
# How to choose s
# Then coefficient

#----------------------------------------------------------------------------------------




## Using dataset with dummy from An:
an_train <- read.csv("data/training_switch_ratio_with_factors_after_dummy.csv") 
  
remove <- c(grep("^onscode_+",names(an_train)))
train  <- an_train %>% select(-remove)
        # -X.1,-X,-num_wave_present,
        #  -num_switch,-switch_ratio,-ifswitch, -wave_index,
        #  -fatherNumEmployees,-motherNumEmployees, 
        #  -profile_socialgrade_cie,-profile_lea,-profile_oslaua)
train_factor <- as.matrix(train[,-c(1:5)])



## full model:
full_r <- lm()
