library(readstata13)
library(tidyverse)
dataset <- read.dta("H:/DAT/BrExit/data/BES2017_W13_Panel_v1.2.dta")



de <- c("competent", "authLong", "grammar", "LookAfter", "overseasAid","^W1","^W2","^W3","^W4","^W5","^W6","^W10","^W11","^W12","^W13")
for(i in 1:length(remove)){
  index <-grep(remove[i], names(dataset))
  dat_789 <- dat_789 %>%
    select(-index)
}


opinion_index <- grep("euRefVoteW",colnames(dataset))
opinion <- dataset()



## filter out variable names with 'Wi' or 'wi'
wave_spec_index <- c(grep("W\\d+", colnames(dataset)), grep("w\\d+",colnames(dataset)))

## dataset that contains factors that were asked only once (static)
static_factors <- dataset %>%
  select(-wave_spec_index) ## 72 of them

write.csv(static_factors,"dataset_static_factors.csv")
