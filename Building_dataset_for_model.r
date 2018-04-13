
library(tidyverse)
library(readstata13)


dataset2 <- read.dta13("H:/DAT/BrExit/data/BES2017_W13_Panel_v1.2.dta", 
                      convert.factors = TRUE,
                      generate.factors = TRUE)

dataset <- haven::read_dta("H:/DAT/BrExit/data/BES2017_W13_Panel_v1.2.dta")
#------------------------------------------------------------------
### columns of immigration-related factors
immig <- c("immigrationLevel", "changeImmig", "immigEcon","immigCultural","controlImmig",
           "effectsEUImmigration", #"euPriorityBalance", "govtHandleImmig"
           "immigrantsWelfareState")#"promiseMigration","proposalMigration"
           
immig_index <-c()
for(i in 1:length(immig)){
  immig_index <-c(immig_index,grep(immig[i], names(dataset)))
}

labour_index <- grep(c("Lab","lab"),names(dataset)[immig_index])

immig_index <- immig_index[-labour_index]
immig_data <- dataset %>%

  select(1,immig_index)


test <- as.numeric(immig_data$changeImmigW1)
new_col = c(2,5,0)[as.numeric(factor_col)]

write_csv(immig_data, "data/immig_cols.csv")


#------------------------------------------------------------------
### SMALL PIECES HERE AND THERE
## remove seemingly irrelevant factors
de <- c("competent", "authLong", "grammar", "LookAfter", "overseasAid","^W1","^W2","^W3","^W4","^W5","^W6","^W10","^W11","^W12","^W13")
for(i in 1:length(remove)){
  index <-grep(remove[i], names(dataset))
  dat_789 <- dat_789 %>%
    select(-index)
}

## opinion columns
opinion_index <- grep("euRefVoteW",colnames(dataset))
opinion <- dataset()


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

#------------------------------------------------------------------
### Make the dataset VERTICAL (WAVE-SPECIFIC)
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



#------------------------------------------------------------------
### Select STATIC FACTORS
## filter out variable names with 'Wi' or 'wi'
wave_spec_index <- c(grep("W\\d+", colnames(dataset)), grep("w\\d+",colnames(dataset)))
wave_index <- grep("^wave\\d+",colnames(dataset)) # remove'wave1:wave13'

## dataset that contains factors that were asked only once (static)
static_factors <- dataset %>%
  select(-wave_spec_index,-wave_index) %>%## 52 of them
  select(-profile_lea, -profile_oslaua, -profile_socialgrade_cie,-euRefLA,-onscode)
# contain both age and age_group at this point


## TODO1:


write.csv(static_factors,"data/dataset_static_factors.csv")


#------------------------------------------------------------------
### Compute SWITCHES
## Building dataset with opinion switch columns

## getting the columns: id, wave, euRefVote
dat <-read.csv("data/wt_age_origin.csv")
col_wave <- grep("^wave\\d+",names(dat))
col_vote <- grep("^euRefVoteW\\d+",names(dat))

opinion_dat <- dat %>% 
  select(c(id,col_wave,col_vote))

# 6301 cases who DID participated in the study but has an NA response
temp <- c() ## all responses
for(i in 1:13){
  
  if(i!= 5){
    wave_name <- paste0("^wave",i,"$")
    wave_col <- grep(wave_name, colnames(opinion_dat))
    
    vote_name <- paste0("^euRefVoteW",i,"$")
    vote_col<-grep(vote_name,colnames(opinion_dat))
    
    wave_i <- opinion_dat %>% 
      mutate(wave = i) %>%
      mutate(voted = ifelse(is.na(opinion_dat[,vote_col]), 0, 1)) %>%
      select(id, wave,vote_col, wave_col,voted)
    
    names(wave_i) <- sub(wave_name, "present", names(wave_i))
    names(wave_i) <- sub(vote_name, "vote", names(wave_i))
  
  } else {
    wave_name <- paste0("^wave",i,"$")
    wave_col <- grep(wave_name, colnames(opinion_dat))

    wave_i <- opinion_dat %>%
      select(id,wave_col) %>%
      mutate(vote = NA,
             voted = NA,
             wave = 5)
    names(wave_i) <- sub(wave_name, "present", names(wave_i))
  }
  temp <- rbind2(temp,wave_i)
}

temp <- temp %>% arrange(id)

# Citation:
# http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
fillNAgaps <- function(x, firstBack=FALSE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  x
}

## Extend NA values
temp_full <- temp %>%
  group_by(id) %>%
  mutate(full_vote = fillNAgaps(vote)) %>%
  mutate(pre_vote = lag(full_vote,1)) %>%
  mutate(switch = ifelse(wave==1, 0, 
                         ifelse(full_vote!=pre_vote, 1, 0))) 

## Compute switch
switch_dat <- temp_full %>%
  summarise(num_wave_voted = sum(voted,na.rm = TRUE),
            num_switch = sum(switch,na.rm = TRUE)) %>% 
  mutate(switch_ratio = ifelse(num_wave_voted == 0, 0, num_switch/num_wave_voted),
         ifswitch = ifelse(num_switch>0, 1,0))

dataset_static <- inner_join(switch_dat,static)
write.csv(dataset_static, "data/dataset_static_factors.csv")

