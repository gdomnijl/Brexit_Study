## Check if dataset is synchronized
library(tidyverse)
library(caret)
library(glmnet)
library(Amelia)

# ------------------------------------------------------------------------------------------

jinlin_dat <- read.csv("./data/jinlin_switch.csv")

an_dat <- read.csv("./data/switches_calculated.csv")

jinlin_dat$switch_ratio <- plyr::round_any(jinlin_dat$switch_ratio,0.0001, f = round)
an_dat$proportion_switches <- plyr::round_any(an_dat$proportion_switches,0.0001, f = round)

diff <- an_dat %>%
  select(-X) %>%
  inner_join(jinlin_dat) %>%
  filter(switch_ratio != proportion_switches) 

# ------------------------------------------------------------------------------------------

switch_dat<-read.csv("data/dataset_static_factors.csv")%>%
  select(-personality_neuroticism, -personality_openness, 
         -personality_agreeableness, -personality_conscientiousness,
         -personality_extraversion,-Age, -motherNumEmployees, -fatherNumEmployees, -X)%>%
  inner_join(an_dat, by = c("id", "id")) %>%
  select(-X)%>%
  filter(proportion_switches>0)
## splitting into test/training set
set.seed(1234)
trainIndex <- createDataPartition(switch_dat$proportion_switches, 
                                  p = 0.75, list = FALSE,times = 1)

train_dat <- switch_dat[trainIndex,]
test_dat <- switch_dat[-trainIndex,]


write.csv(train_dat, "data/training_switch_ratio2.csv")
write.csv(test_dat, "data/test_switch_ratio2.csv")


trainIndex2 <- createDataPartition(switch_dat$ifswitch, 
                                  p = 0.75, list = FALSE,times = 1)
train_dat_ifswitch <- switch_dat[trainIndex2,]
test_dat_ifswitch <- switch_dat[-trainIndex2,]
write.csv(train_dat_ifswitch, "data/training_ifswitch.csv")
write.csv(test_dat_ifswitch, "data/test_ifswitch.csv")

# ------------------------------------------------------------------------------------------


## Modeling:
factor<-read.csv("./data/dataset_static_factors.csv") %>% 
      # Note: removed Age; Reason: set NA to 0 to other levels, so use factor(AgeGroup) instead of continuous (Age) 
      # Note: remove mother/fatherNumEmployees(LOTS OF NAs & they are continuous)
      # 31 factors other than id
     select(-Age,-X, -motherNumEmployees, -fatherNumEmployees) 

factor$headHouseholdPast<-plyr::revalue(as.factor(factor$headHouseholdPast), 
                                        c("1"="My father", "2"="My mother", 
                                          "3" = "Someone else",
                                          "4" = "No one in my house worked",
                                          "9999" = "Don't know"))
# ------------------------------------------------------------------------------------------

## on switch ratio:

train_ratio <- read.csv("./data/training_switch_ratio2.csv") %>%
  select(-X)
train_ratio$headHouseholdPast<-plyr::revalue(as.factor(train_ratio$headHouseholdPast), 
                                        c("1"="My father", "2"="My mother", 
                                          "3" = "Someone else",
                                          "4" = "No one in my house worked",
                                          "9999" = "Don't know"))

## TODO: Need to deal with NAs
## only left with 9043 without NAs
## PREVIOUS DECISION: Replaced all NAs with 0s 
      #train_ratio_factor[is.na(train_ratio_factor)] <- 0
## WRONG because all 0's can mean both: the default factor level or NA
## CURRENT DECISION: Turn NA into a new factor level:
for(i in 2:length(names(train_ratio))-1){
  if(class(train_ratio[,i]) == "factor"){
    train_ratio[,i]<-addNA(train_ratio[,i])

  }
}



## TODO: intercept?
## TODO: choose only factor-level or factor
f <- reformulate(response = NULL, termlabels=names(train_ratio)[-c(1)], intercept = TRUE)

options(na.action='na.pass')
train_ratio_factor <- model.matrix(f, train_ratio) 
## 51470 points, 289 factor-levels with NA levels (not including default levels)
## but including default level for first variable in place of intercept QUESTION

lasso_ratio <- cv.glmnet(x = train_ratio_factor[,-c(1,2)], y = train_ratio_factor[,2],
              alpha = 1, nlambda = 10, standardize = TRUE, family= "binomial", type.measure = "mse")

## 10-fold default
#plot(lasso_ratio, xvar = "lambda", label = TRUE)
#plot(lasso_ratio, xvar = "mse", label = TRUE)

## MSE vs lambda
plot.cv.glmnet(lasso_ratio)
 coef<- coef(lasso_ratio, s = lasso_ratio$lambda.1se)
non_zero_fact <- coef@i[-1]
non_zero_coef <- coef@x[-1]
lasso_ratio_factors <- as.data.frame(cbind2(c("intercept",colnames(train_ratio_factor)[non_zero_fact+2]), ## here it is lining up just nicely

                             c(coef@x[1],non_zero_coef)))
write.csv(lasso_ratio_factors, "21_lasso_factors_ratio.csv")

## TODO: is there a way to find the 'best' alpha range
ridge_ratio <- glmnet(x = train_ratio_factor[,-1], y = train_ratio_factor[,1],
              alpha = 0, nlambda = 10, standardize = TRUE)
#elastic_ratio <- 

#----------------------------------------------------------------------------------------
## on Ifswitch:
train_ifswitch <- read.csv("data/training_ifswitch2.csv") %>% 

  select(id,ifswitch) %>%
  inner_join(factor)

## Treat NA as another factor-level
for(i in 3:length(names(train_ifswitch))){
  if(class(train_ifswitch[,i]) == "factor"){
    train_ifswitch[,i]<-addNA(train_ifswitch[,i])
  }else{
    train_ifswitch[,i] <- as.factor(train_ifswitch[,i])
    train_ifswitch[,i]<-addNA(train_ifswitch[,i])
  }
}

## LASSO for ifswitch:
f2 <- reformulate(response=NULL, termlabels=names(train_ifswitch)[-c(1)],intercept =TRUE)
options(na.action='na.pass')
train_ifswitch_factor <- model.matrix(f2, train_ifswitch)  

lasso_ifswitch <- cv.glmnet(x = train_ifswitch_factor[,-c(1,2)], y = train_ifswitch_factor[,2],
              alpha = 1, nlambda = 10, standardize = TRUE, family = "binomial",type.measure = "auc")
## AUC vs lambda
plot.cv.glmnet(lasso_ifswitch)
coef2 <- coef(lasso_ifswitch, s = 0.01)#lasso_ifswitch$lambda.1se) #36 factors
non_zero_fact2 <- coef2@i[-1]
non_zero_coef2 <- coef2@x[-1]
lasso_ifswitch_factors <- as.data.frame(cbind2(c("intercept",colnames(train_ifswitch_factor)[non_zero_fact2+2]), ## here it is lining up just nicely
                                            c(coef2@x[1],non_zero_coef2)))
write.csv(lasso_ifswitch_factors, "36_lasso_factors_ifswitch.csv")

## 

dplyr::setdiff(lasso_ifswitch_factors$V1,lasso_ratio_factors$V1)

lasso_ifswitch_factors %>%
  inner_join(lasso_ratio_factors, by = "V1")

##
test_ifswitch <- read.csv("./data/test_ifswitch.csv") %>%
  inner_join(factor) %>% 
  select(-c(3:5))

## Treat NA as another factor-level
for(i in 4:length(names(test_ifswitch))){
  if(class(test_ifswitch[,i]) == "factor"){
    test_ifswitch[,i]<-addNA(test_ifswitch[,i])
  }else{
    test_ifswitch[,i] <- as.factor(test_ifswitch[,i])
    test_ifswitch[,i]<-addNA(test_ifswitch[,i])
  }
}
test_matrix <-  model.matrix(f2,test_ifswitch)
test_result<-predict(lasso_ifswitch, newx = test_matrix[,-c(1,2)], type = "class", s = 0.05)
colnames(test_result) <- "response"
test_result <- as.data.frame(test_result)
result <- test_result %>%  
  mutate(X = row_number()) %>%
  inner_join(test_ifswitch[,1:3])

## prediction table
ptb<-table(result$response,result$ifswitch)
sum(ptb[1,1]+ptb[2,2])/sum(ptb)
# TODO: PUT BACK SOCIAL GRADE! 

# TODO: correlation analysis
# How to choose s
# Then coefficient

# plot relevant factors:
train_ifswitch$ifswitch <- relevel(as.factor(train_ifswitch$ifswitch), "1")

plot <- train_ifswitch %>% 
  group_by(profile_past_vote_2015,ifswitch) %>%
  summarise(count = n())%>%
  #spread(ifswitch,count)%>%
  #mutate(sum = )
  ggplot(aes(x = profile_past_vote_2015, y = count, fill = as.factor(ifswitch))) + 
  geom_bar(stat="identity") + scale_fill_discrete(name="ifswitch",
                                                  breaks=c("1", "0"),
                                                  labels=c("Yes", "No"))

## only plotting from training set!
# plot_this <- function(v){
#   index <- grep(v, colnames(train_ifswitch))
#   plot_r <- train_ifswitch %>% 
#     group_by(train_ifswitch[,index],ifswitch) %>%
#     summarise(count = n())%>%
#     group_by(train_ifswitch[,index]) %>%
#     mutate(total = sum(count)) %>%
#     mutate(ratio = count/total) %>%
#     filter(ifswitch == 1)%>%
#     ggplot(aes(x = train_ifswitch[,index], y = ratio, color = train_ifswitch[,index])) + 
#     geom_line()+geom_point() +  ylab("% switched their opinions") +
#     theme(axis.text.x = element_blank())
#   return(plot_r)
# }

train_ifswitch$ageGroup <- relevel(train_ifswitch$ageGroup, "Under 18")
plot_r <- train_ifswitch %>% 
  group_by(countryOfBirth,ifswitch) %>%
  summarise(count = n())%>%
  group_by(countryOfBirth) %>%
  mutate(total = sum(count)) %>%
  mutate(ratio = count/total) %>%
  filter(ifswitch == 1)%>%
  ggplot(aes(x = countryOfBirth, y = ratio, color = countryOfBirth)) + 
  geom_line()+geom_point() +  ylab("% switched their opinions") +
  theme(axis.text.x = element_blank())
#----------------------------------------------------------------------------------------
# use caret
fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)
set.seed(825)
gbmFit1 <- train(switch_ratio ~ ., data = train_ratio[,-c(1)], 
                 method = "lasso", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)


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
