
## feature engineering
#--------------------------------------------
# dataframe dictionary:
# immig_data: responses to the 7 immigration questions on wave-to-wave level
# immig_plvl: responses on personal level across all waves


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

write.csv(immig_data, "data/immig_cols.csv", row.names = FALSE) 
## TODO: re-write, don't create a new id column 

## --------------------------------------------------------
## all responses are in numeric
immig_data<-read.csv("data/immig_cols.csv", row.names = NULL)
## standardize all responses between 0 and 1
## 1: anti-immigrants 0: pro-immigrants
lvl <- grep("immigrationLevel", names(immig_data))
chg <- grep("changeImmig", names(immig_data))
econ <- grep("immigEcon", names(immig_data))
cul <- grep("immigCultural", names(immig_data))
wel <- grep("immigrantsWelfareState", names(immig_data))
ctr <- grep("controlImmig", names(immig_data))
eu <- grep("effectsEUImmigration", names(immig_data))

for(i in 3:length(names(immig_data))){

  ## 5 is pro
  if(i %in% c(lvl, eu)){
    immig_data[,i] <- 1-(immig_data[,i]-1)/4
  } 
  
  ## 1 is pro
  if(i %in% c(ctr,wel,chg)){
    immig_data[,i] <- (immig_data[,i]-1)/4
  }
  
  ## 7 is pro
  if(i %in% c(econ, cul)){
    immig_data[,i] <- 1-(immig_data[,i]-1)/6
  } 
  
  # Since 'dk' are at extreme values,
  # after standardization, they will fall out of bound [0,1]
  # so, turn them into NA
  immig_data[,i] <- ifelse(immig_data[,i] > 1 | immig_data[,i] < 0,
                           NA, immig_data[,i])
}



#-------------------------------------------------------------------------------------

## TODO: measure how many people had drastic change in immigration attitude across waves
## Cross the 0.5 line
for(i in 3:length(names(immig_data))){
  ## only compute from the second wave of response for each question
  lvl_c <-immig_data[,c(2,lvl)]
  chg_c <-immig_data[,c(2,chg)]
  econ_c <-immig_data[,c(2,econ)]
  cul_c <-immig_data[,c(2,cul)]
  ctr_c <-immig_data[,c(2,ctr)]
  eu_c <-immig_data[,c(2,eu)]
  wel_c <-immig_data[,c(2,wel)]
}

t<-eu_c %>% 
  gather(wave,index,-id)%>%
  group_by(id)%>%
  arrange(id)%>%
  summarize(r = range(index)) 
    #extreme = ifelse(range >= 0.5, 1, 0))
  
#-------------------------------------------------------------------------------------

## compute average metric on a personal level
immig_plvl <- immig_data %>%
  mutate(immigrationLevel = rowMeans(immig_data[,lvl], na.rm = TRUE),
         changeImmig = rowMeans(immig_data[,chg], na.rm = TRUE),
         immigEcon = rowMeans(immig_data[,econ], na.rm = TRUE),
         immigCultural = rowMeans(immig_data[,cul], na.rm = TRUE),
         controlImmig = rowMeans(immig_data[,ctr], na.rm = TRUE),
         effectsEUImmigration = rowMeans(immig_data[,eu], na.rm = TRUE),
         immigrantsWelfareState = rowMeans(immig_data[,wel], na.rm = TRUE)) %>%
  select(id,immigrationLevel,changeImmig, immigEcon, immigCultural, 
         controlImmig,effectsEUImmigration,immigrantsWelfareState) 
write.csv(immig_plvl,"./data/immig_cols_standardized.csv")

immig_plvl <- read.csv("data/immig_cols_standardized.csv")
immig_plvl_index <- immig_plvl %>%
  mutate(AIS = rowMeans(immig_plvl[,c(5,6,9)], na.rm = TRUE)) %>%
  select(-X)
  
write.csv(immig_plvl_index, "data/AIS.csv", row.names = FALSE)
## Correlation between these 7 responses

library("corrplot")
res <- cor(immig_plvl[,-1], use = "na.or.complete")
corrplot(res, method = "shade", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)

## So now compute the immig_index using average of the 5 questions
immig_plvl_index5 <- immig_plvl %>%
  mutate(immig_index5 = rowMeans(immig_plvl[,c(2:5,8)], na.rm = TRUE))


immig_factor <- immig_plvl %>%
  select(c(1,6,7)) %>%
  inner_join(immig_plvl_index5)%>%
  select(id,controlImmig,effectsEUImmigration,immig_index5)

### TODO: currently on with TRAINING SET
###  merge table with switch ratio and static factors:
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


vote <-read.csv("./data/dataset_static_factors.csv")
train_ratio <- read.csv("./data/training_switch_ratio.csv") %>% 
  select(id,switch_ratio) %>%
  inner_join(immig_plvl_index5) %>%
  inner_join(vote)
  
train_ratio %>%
  ggplot(aes(x = profile_eurefvote, y = immig_index5)) + geom_boxplot()
## why immig_index raised amongst switchers


train_ratio %>%
  filter(switch_ratio!=0) %>%
  ggplot(aes(switch_ratio)) + geom_histogram(binwidth = 0.1)

train_ratio %>%
  filter(switch_ratio!=0) %>%
  ggplot(aes(y = immig_index5, x = as.factor(switch_ratio)))+geom_boxplot()

## TODO: switch_ratio is extremely "right skewed" because 2/3 of the response don't switch
## TODO: Look at switch_ratio of the population who indeed switch
## NOTE: it is still heavily right skewed within all the "switchers"
## TODO: 
## model 1: all population using ifswitch
## model 2: use switch_ratio amongst "switchers"
train_bi <- read.csv("./data/jinlin_switch.csv") %>%
  select(id,ifswitch) %>%
  inner_join(immig_plvl_index5) %>%
  inner_join()
  inner_join(vote)

train_bi %>% 
  filter(profile_)
  ggplot(aes(x = as.factor(ifswitch), y = immig_index5)) + geom_boxplot()

## TODO: plot all groups in one graph
## This group of people is very interesting:
## Voted for stay yet switched opinion
## TODO: direction of switch

train_bi %>%
  ggplot(aes(x = interaction(as.factor(ifswitch), profile_eurefvote), y = immig_index5)) + geom_boxplot() +
  ggtitle("Voted leave")

  

  
## Correlation analysis

## VISUALIZING VOTER_TYPE
  
  
## Correlation analysis
  ## f(x) = 2x - 1
  # map from [0,1] to [-1,1]
  
  
