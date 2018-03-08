## Check if dataset is synchronized
library(tidyverse)
library(caret)

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

## 

## on switch ratio:
## full model:
full_r <- lm()
