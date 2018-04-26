library(tidyverse)
dat <- read.csv("data/voter_type_data.csv")
dat$voter_type <- factor(dat$voter_type, levels = c("0.Stay/remain in the EU", 
                                                    "1.Stay/remain in the EU", 
                                                    "1.Leave the EU",
                                                    "0.Leave the EU",
                                                    "0.Don't know",
                                                    "1.Don't know"))
## Build up: 
## why we look at ifswitch:

## 1:
## The highly skewed portion 
table(dat$ifswitch)
# 0     1 
# 48327 20298 

dat %>%
  ggplot(aes(switch_ratio)) + geom_histogram(binwidth = 0.03)

# NOTE: bin width: 0.5 peaked!
dat %>%
  filter(switch_ratio!=0) %>%
  ggplot(aes(switch_ratio)) + geom_histogram(binwidth = 0.02)

## 2: immigration sentiment appears consistent irrespective of switch_ratio
dat %>%
  filter(switch_ratio!=0) %>%
  ggplot(aes(x = as.factor(switch_ratio), y = immig_index5)) + geom_boxplot() +
  xlab("Switch Ratio Amongst 'Switchers'") + ylab("Anti-immigrant Sentiment Index")+
  scale_x_discrete(breaks=c(0:1))


## Distribution of immigration sentiment across public
dat %>%
  ggplot(aes(immig_index5)) + geom_histogram(binwidth = 0.02)

## Plot according to type
dat %>%  
  filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
  ggplot(aes(x = voter_type, y = immig_index5)) + geom_boxplot() + 
  facet_grid(country ~ .) + 
  scale_x_discrete(labels=c("Hardcore Stay", "Ambivalent Stay", "Ambivalent Leave", "Hardcore Leave"))


  