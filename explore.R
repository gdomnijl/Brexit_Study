library(readstata13)
library(tidyverse)

## wave1 - wave13, id, eurefvote_i, weight_

## Reduce to a condensed dataset
condensed <- dat[,c(1:67,72,75,index)]
write.csv(condensed, "wt_age_origin.csv")

index <-c()
for(i in 1:13){
  reg <- paste0("^euRefVoteW",i,"$")
  col_num<-grep(reg,colnames(dat))
  index <- c(index, col_num)
}

grep("^euRefVoteW1$", colnames(dat)) #265

## Collect general stay percent from each wave
stay_p <-c()
for(i in 1:4){
  wave_name <- paste0("^wave",i,"$")
  wave_col <- grep(wave_name, colnames(condensed))
  
  vote_name <- paste0("^euRefVoteW",i,"$")
  vote_col<-grep(vote_name,colnames(condensed))
  
  wt_name <- paste0("wt_core_W",i)
  wt_col <- grep(wt_name,colnames(condensed))
  
  wave <- condensed %>%
    filter(condensed[,wave_col]==1) 
  
  wave_stay <- wave %>% 
    filter(wave[,vote_col]=="Stay/remain in the EU")
  
  total <- sum(wave[,wt_col], na.rm = TRUE)
  stay <- sum(wave_stay[,wt_col], na.rm = TRUE)
  stay_p <- c(stay_p, stay/total)
}

