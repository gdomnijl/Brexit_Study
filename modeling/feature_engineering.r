
#--------------------------------------------

## feature engineering
#immig <- read.csv("data/immig_cols.csv") NOTE: this will reorder the levels which we don't want 
test <- as.numeric(immig_data$changeImmigW1)
new_col = c(2,5,0)[as.numeric(factor_col)]


## convert levels into numerics
for(i in 2:length(names(immig_data))){
  immig_data[,i] <- as.numeric(immig_data[,i])
}

#immigrationLevel # 1: decreased a lot, 5 increased a lot, 9: dk
#changeImmig # 1:a lot lower, 5: a lot higher, 6: dk
#immigEcon # 1:bad 7:good 8:dk
#immigCultural # 1:bad 7:good 8:dk
#immigrantsWelfareState # 1: strongly disagree 5: strongly agree 6:dk

#controlImmig # 1:no control 5:complete control 6:dk
#effectsEUImmigration # 1: much lower 5:much higher 6: dk

write.csv(immig_data, "data/immig_cols.csv")


## standardize all responses between 0 and 1
## 1: anti-immigrants 0: pro-immigrants
lvl <- grep("immigrationLevel", names(immig_data))
chg <- grep("changeImmig", names(immig_data))
econ <- grep("immigEcon", names(immig_data))
cul <- grep("immigCultural", names(immig_data))
wel <- grep("immigrantsWelfareState", names(immig_data))
ctr <- grep("controlImmig", names(immig_data))
eu <- grep("effectsEUImmigration", names(immig_data))

for(i in 2:length(names(immig_data))){
  ## TODO: change dk to NA
  
  ## 5 is pro
  if(i %in% c(lvl, eu)){
    immig_data[,i] <- (immig_data[,i]-1)/4
  } 
  
  ## 1 is pro
  if(i %in% c(ctr,wel,chg)){
    immig_data[,i] <- 1-(immig_data[,i]-1)/4
  }
  
  ## 7 is pro
  if(i %in% c(econ, cul)){
    immig_data[,i] <- (immig_data[,i]-1)/6
  } 
  
}

