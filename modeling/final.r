library(tidyverse)
dat <- read.csv("data/voter_type_data.csv")
new_relvl <-read.csv("data/Final_relvl_bi_voter_type.csv")%>%
  select(id, rl_profile_ethnicity, rl_profile_religion_denom,
         rl_profile_religion, rl_profile_newspaper)
bi_dat <- read.csv("data/Relvl_bi_voter_type.csv")%>%
  select(-profile_ethnicity, -profile_religion_denom, -profile_religion, -profile_newspaper,
         -profile_eurefvote)%>%
  inner_join(new_relvl)

dat$voter_type <- factor(dat$voter_type, levels = c("0.Stay/remain in the EU", 
                                                    "1.Stay/remain in the EU", 
                                                    "1.Leave the EU",
                                                    "0.Leave the EU",
                                                    "0.Don't know",
                                                    "1.Don't know"))
bi_dat$bi_voter_type <- factor(bi_dat$bi_voter_type, levels = c("0.Stay/remain in the EU", 
                                                    "1.Stay/remain in the EU", 
                                                    "1.Leave the EU",
                                                    "0.Leave the EU",
                                                    "0.Don't know",
                                                    "1.Don't know"))


immig_data <- read.csv("data/AIS.csv")
ulti_dat <- bi_dat %>% 
  inner_join(immig_data)
write.csv(ulti_dat,"data/ultimate_data.csv", row.names = FALSE)

immig_measure <- ulti_dat %>%
  select(1,13,14,28:33)%>%
  gather(measure, value,-id)

ulti_plot <- ulti_dat %>%
  select(id,bi_voter_type) %>%
  inner_join(immig_measure)

ulti_plot$measure <- factor(ulti_plot$measure, levels = c("AIS",
                                                            "immigCultural",
                                                            "immigEcon",
                                                            "immigrantsWelfareState",
                                                            "changeImmig",
                                                            "controlImmig",
                                                            "effectsEUImmigration",
                                                            "immigrationLevel"))

ulti_dat$ageGroup <- relevel(ulti_dat$ageGroup,"Under 18")
ulti_dat$rl_gross_household <- factor(ulti_dat$rl_gross_household, levels = c("0-14,999 per year",
                                                                              "15,000-29,999 per year",
                                                                              "30,000 to 49,999 per year",
                                                                              "50,000-149,999 per year",
                                                                              "150,000 and above"))
write.csv(ulti_plot, "data/ultimate_plot.csv", row.names = FALSE)

library(ggplot2)
ggplot(bi_dat, aes_string(x = "bi_voter_type", y = "immig_index5", fill = "bi_voter_type")) + 
  geom_boxplot() + 
  scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
  #annotate("text", x = 1:4, y = 0, label = c(as.character(ct$count)))+
  scale_fill_manual(values=c("#2c7fb8","#7fcdbb","#fdae6b","#e6550d"))

## Build up: 
## why we look at ifswitch:

## 1:
## The highly skewed portion 
table(dat$ifswitch)
# 0     1 
# 48327 20298 
table(bi_dat$bi_ifswitch)
# 0     1 
# 58837  9788 


dat %>%
  ggplot(aes(switch_ratio)) + geom_histogram(binwidth = 0.03)


bi_dat %>%
  ggplot(aes(bi_switch_ratio)) + geom_histogram(binwidth = 0.03)
# NOTE: bin width: 0.5 and 1.0 peaked!
# TODO: WHY look at the samples in those bins

# NOTE: bin width: 0.5 peaked!


## 2: immigration sentiment appears consistent irrespective of switch_ratio
## TODO: Put the distribution graph tgt with this graph

dat %>%
  filter(switch_ratio!=0) %>%
  ggplot(aes(x = as.factor(switch_ratio), y = immig_index5)) + geom_boxplot() +
  xlab("Switch Ratio Amongst 'Switchers'") + ylab("Anti-immigrant Sentiment Index")+
  scale_x_discrete(breaks=c(0:1))

bi_dat %>%
  filter(bi_switch_ratio!=0) %>%
  ggplot(aes(x = as.factor(bi_switch_ratio), y = immig_index5)) + geom_boxplot() +
  xlab("Binary Switch Ratio Amongst 'Polarized Switchers'") + ylab("Anti-immigrant Sentiment Index")+
  scale_x_discrete(breaks=c(0:1))


## Distribution of immigration sentiment across public
dat %>%
  ggplot(aes(immig_index5)) + geom_histogram(binwidth = 0.02)


## Compute sample size for the plot below
ct<-bi_dat %>%
  select(bi_voter_type) %>%
  filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))%>%
  group_by(bi_voter_type) %>%
  summarise(count = n())

## Plot according to type
p<-bi_dat %>%  
  filter(!bi_voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(bi_voter_type))%>%
  ggplot(aes(x = bi_voter_type, y = immig_index5, color = country)) + 
  geom_boxplot()+
  #geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), adjust = 0.5) + 

## Plot according to type
dat %>%  
  filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
  ggplot(aes(x = voter_type, y = immig_index5, color = country)) + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), adjust = 0.5) + 
#  facet_grid(country ~ .) + 
  scale_x_discrete(labels=c("Always-Stay", "Switched-Stay", "Switched-Leave", "Always-Leave"))+
annotate("text", x = 1:4, y = 0, label = c(as.character(ct$count)))

ggplotly(p) %>% 
 # layout(height = input$plotHeight, autosize=TRUE) %>%
  # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
  layout(boxmode = "group")
ct<-dat %>%
  select(voter_type) %>%
  filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))%>%
  group_by(voter_type) %>%
  summarise(count = n())


ready <-dat %>%  
  filter(!voter_type %in% c("0.Don't know", "1.Don't know") & !is.na(voter_type))

plot_ly(p, y = ~immig_index5, x = ~voter_type, color = ~country, type = "box") %>%
  layout(boxmode = "group")

#------------------------------------------------------------------------------------------------------
## To relevel:
## Check sample sizes in each group factor:

dat %>%
  filter(!is.na(profile_religion_denom)) %>%
  ggplot(aes(profile_religion_denom)) + geom_histogram(stat = "count") + coord_flip()

## Relevel:
# household _children
dat_lv <- bi_dat %>%
  mutate(rl_profile_household_children = 
           ifelse(profile_household_children %in% c("Prefer not to say", "Don't know"), 
                  NA,
                  ifelse(profile_household_children == "0", 
                         "no children",
                         "have children"))) %>%
  mutate(rl_profile_household_size = 
           ifelse(profile_household_size %in% c("Prefer not to say", "Don't know"), 
                  NA,
                  ifelse(profile_household_size == "1", 
                         "1",
                         ifelse(profile_household_size == "2",
                         "2",
                         "3 or more"))))  %>%
  mutate(rl_marital = 
           ifelse(marital %in% c("Civil Partnership",
                                 "Living as married",
                                 "Married"),
                                 "Married",
                                 ifelse(marital %in% c("Divorced",
                                                       "Seperated (after being married)",
                                                       "Widowed"),
                                        "Single",
                                        ifelse(marital == "Never married",
                                               "Never Married", NA)))) %>%

    mutate(rl_countryOfBirth = ifelse(countryOfBirth %in% 
                                        c("Northen Ireland",
                                          "Republic of Ireland",
                                          "Scotland",
                                          "Wales"),
                                      "UK_nonEngland",
                                      ifelse(countryOfBirth == "England", 
                                               "England",
                                               ifelse(countryOfBirth == "Other: European Union member country",
                                                      "EU_counties",
                                                      ifelse(countryOfBirth %in% 
                                                               c("Other: Commonwealth member country",
                                                                 "Other: Rest of world"),
                                                             "Rest_of_world",NA))))) %>%
  mutate(rl_gross_personal  =
           ifelse(profile_gross_personal %in% 
           c("under £5,000 per year",
            "£5,000 to £9,999 per year"),
         "0-9,999 per year",
         ifelse(profile_gross_personal == "£10,000 to £14,999 per year",
                "10,000-14,999 per year",
                ifelse(profile_gross_personal %in% 
                         c("£15,000 to £19,999 per year",
                           "£25,000 to £29,999 per year"),
                       "15,000-29,999 per year",
                       ifelse(profile_gross_personal %in% 
                                c("£30,000 to £34,999 per year",
                                  "£40,000 to £44,999 per year",
                                  "£45,000 to £49,999 per year",
                                  "£50,000 to £59,999 per year",
                                  "£60,000 to £69,999 per year",
                                  "£70,000 to £99,999 per year"),
                              "30,000-99,999 per year", NA))))) %>%

  mutate(rl_gross_household  =
         ifelse(profile_gross_household %in% 
                  c("under £5,000 per year",
                    "£5,000 to £9,999 per year",
                    "£10,000 to £14,999 per year"),
                "0-14,999 per year",
                       ifelse(profile_gross_household %in% 
                                c("£15,000 to £19,999 per year",
                                  "£25,000 to £29,999 per year"),
                              "15,000-29,999 per year",
                           ifelse(profile_gross_household %in% 
                                    c("£30,000 to £34,999 per year",
                                      "£40,000 to £44,999 per year",
                                      "£45,000 to £49,999 per year"),
                                     "30,000 to 49,999 per year",
                              ifelse(profile_gross_household %in% 
                                       c("£50,000 to £59,999 per year",
                                         "£60,000 to £69,999 per year",
                                         "£70,000 to £99,999 per year",
                                         "£100,000 and £149,999 per year"),
                                     "50,000-149,999 per year", 
                                     ifelse(profile_gross_household == "£150,000 and over",
                                            "150,000 and above",
                                       NA)))))) %>%

  mutate(rl_disability = ifelse(disability == "No", "No",
                                ifelse(disability %in%
                                         c("Yes, limited a little",
                                           "Yes, limited a lot"), 
                                       "Yes",
                                       NA))) %>%
  select(-profile_household_children,
         -profile_household_size, 
         -marital, 
         -countryOfBirth,
         -profile_gross_personal,
         -profile_gross_household,
         -disability,
         -profile_house_tenure,
         -headHouseholdPast)%>%

select(-bi_ifswitch,-bi_switch_ratio)%>%
  mutate(bi_voter_type = ifelse(bi_voter_type == "1.Stay/remain in the EU",
                                "1.Stay/remain in the EU",
                                ifelse(bi_voter_type == "0.Stay/remain in the EU",
                                       "0.Stay/remain in the EU",
                                       ifelse(bi_voter_type == "1.Leave the EU",
                                              "1.Leave the EU",
                                              ifelse(bi_voter_type == "0.Leave the EU",
                                                     "0.Leave the EU",
                                                     NA))))) %>%
  filter(!is.na(bi_voter_type))

write.csv(dat_lv, "data/Relvl_bi_voter_type.csv", row.names = FALSE)

bi_index <- createDataPartition(ulti_dat$bi_voter_type, 
                                     p = 0.75, list = FALSE,times = 1)

## NOTE: Eliminating NAs from effectsEUImmigration and controlImmig
train_dat2 <- ulti_dat[bi_index,]
test_dat2 <- ulti_dat[-bi_index,]
write.csv(train_dat2, "data/Relvl_bi_training.csv", row.names = FALSE)
write.csv(test_dat2, "data/Relvl_bi_testing.csv", row.names = FALSE)

#------------------------------------------------------------------------------------------------------
train_dat2<-read.csv("data/Relvl_bi_training.csv")
test_dat2<- read.csv("data/Relvl_bi_testing.csv")

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10)

set.seed(825)

train_dat <- train_dat2 %>%
  filter(!is.na(immig_index5) & 
           !is.na(controlImmig) & 
           !is.na(effectsEUImmigration))%>%
  select(immig_index5,bi_voter_type, controlImmig,effectsEUImmigration)
train_dat$bi_voter_type <- as.factor(train_dat$bi_voter_type)

test_dat <- test_dat2 %>%
  filter(!is.na(immig_index5) & 
           !is.na(controlImmig) & 
           !is.na(effectsEUImmigration))%>%
  select(immig_index5,bi_voter_type, controlImmig,effectsEUImmigration)
#------------------------------------------------------------------------------------------------------
## using just multinom
library(nnet)
mod <- multinom(bi_voter_type ~ ., data = train_dat)

# Function to predict multinomial logit choice model outcomes
# model = nnet class multinomial model
# newdata = data frame containing new values to predict
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}

pred <- predictMNL(mod, test_dat)

 df <- cbind(test_dat,pred = pred)
 df %>%
   ggplot(aes(x = immig_index5, y = controlImmig, color = bi_voter_type)) + geom_point()
   
 #------------------------------------------------------------------------------------------------------
## Using rpart
  
library(rpart)
 # only with immig factors
 mod2<-rpart(bi_voter_type ~ ., data = train_dat)
 pred2 <- predict(mod2, test_dat[,c(1,3,4)], type="class")
 table(pred2, test_dat[,2])
 
 # with all 
 train_dat3 <- train_dat2 %>% 
   select(-profile_eurefvote)
 test_dat3 <- test_dat2 %>% 
   select(-profile_eurefvote)
 
 mod3<-rpart(bi_voter_type ~ ., data = train_dat3)
 pred3 <- predict(mod3, test_dat3[,c(1:18, 20:26)], type="class")
 table(pred3, test_dat3[,19])
 
 # accuracy 0.692699
 library(rpart.plot)				# Enhanced tree plots
 library(rattle)	
 prp(mod3)
tree <- prp(mod3,snip=TRUE)$obj # interactive
fancyRpartPlot(mod3)
gbmFit1 <- train(bi_voter_type ~ ., data = train_dat, 
                 method = "adaboost", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)
#method = 'adaboost')

#------------------------------------------------------------------------------------------------------

# check correlation between immig cols
library("corrplot")
immig_col <- read.csv("data/immig_cols_standardized.csv")
res <- cor(immig_col[,-c(1,2)], use = "na.or.complete")

corrplot(res, method = "shade", type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.7)
library("Hmisc")
res2<-rcorr(as.matrix(immig_col[,c(4,5,6,9)]))

corrplot.mixed(res2$r, lower = "number", upper = "circle", tl.col = "black", tl.cex = 0.7)
# ,  order="hclust", #type="upper",
#          p.mat = res2$P, sig.level = 0.01, 
#          tl.srt = 45, insig = "blank",
#          tl.col = "black", tl.cex = 0.75, number.cex = 0.5)

#------------------------------------------------------------------------------------------------------

# Plot the very first graph 
# x= Timeline y = voting_ratio
# x= Timeline y = immig_index5 

library(tidyverse)
library(lubridate)
data <- read.csv("data/wt_age_origin.csv")

## Join with releveled data
demo_dat <- read.csv("data/Relvl_bi_voter_type.csv")

data2 <- c() ## all responses
for(i in 1:13){
  if(i!= 5){
    wave_name <- paste0("^wave",i,"$")
    wave_col <- grep(wave_name, colnames(data))
    
    vote_name <- paste0("^euRefVoteW",i,"$")
    vote_col<-grep(vote_name,colnames(data))
    
    if(i<=9){
      wt_name <- paste0("^wt_core_W",i,"$")
      wt_col <- grep(wt_name,colnames(data))
    }else if (i == 10){
      wt_name <- paste0("^wt_full_W",i,"$")
      wt_col <- grep(wt_name,colnames(data))
    }else{
      wt_name <- paste0("^wt_new_W",i,"$")
      wt_col <- grep(wt_name,colnames(data))
    }
    
    wave_i <- data %>% 
      filter(data[,wave_col]==1) %>%
      select(id, wt_col, vote_col, -wave_col) %>%
      mutate(wave = i) 
    
    names(wave_i) <- sub(wt_name, "wt", names(wave_i))
    names(wave_i) <- sub(vote_name, "vote", names(wave_i))
    
    data2 <- rbind2(data2,wave_i)
  }
}

ins_dat <- data2 %>% 
  inner_join(demo_dat)

ins_dat <-ins_dat %>% mutate(endtime = ifelse(wave == 1, "2014-03-09",  ifelse (wave == 2, "2014-06-25",
                                                                                ifelse (wave == 3, "2014-10-17",
                                                                                        ifelse (wave == 4, "2015-03-30",
                                                                                                ifelse (wave == 5, "2015-05-06", 
                                                                                                        ifelse (wave == 6, "2015-05-26",
                                                                                                                ifelse (wave == 7,"2016-05-4",
                                                                                                                        ifelse (wave == 8, "2016-06-22",
                                                                                                                                ifelse (wave == 9, "2016-07-04",
                                                                                                                                        ifelse (wave == 10, "2016-12-12",
                                                                                                                                                ifelse (wave == 11,"2017-05-03",
                                                                                                                                                        ifelse (wave == 12,"2017-06-07",
                                                                                                                                                                "2017-06-23")))))))))))))

ins_dat$endtime <- as.Date(ins_dat$endtime)
write.csv(ins_dat, "data/timeline_plot_vote.csv")

ins_dat <- read.csv("data/timeline_plot_vote.csv")
ref_time <- as.Date(as.POSIXct("2016-06-23"))
ref_time2 <- as.Date(as.POSIXct("2016-06-24"))

all_12_id <- ins_dat %>%
  group_by(id) %>%
  summarize(waves = n()) %>%
  filter(waves==12)

ins_dat_12 <- ins_dat %>%
  inner_join(all_12_id) %>%
  select(-waves,-X,-id,-wave)

raw_p <-ins_dat %>% 
  select(endtime, vote, wt) %>%
  group_by(endtime,vote) %>%
  mutate(count = sum(wt,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(endtime) %>%
  mutate(total_count = sum(wt, na.rm = TRUE)) %>%
  mutate(ratio = count / total_count) %>%
  ggplot(aes(x = endtime, y= ratio, group = vote, colour = vote)) + geom_line() + geom_point() +
  annotate("segment", x = ref_time, xend = ref_time, y = 0, yend = 0.8,
           colour = "black") +
  annotate("text", x = ref_time, y = 0.9, label = "Referendum")


grp_p <-ins_dat %>% 
  select(endtime, vote, wt, rl_countryOfBirth) %>%
  group_by(endtime,vote, rl_countryOfBirth) %>%
  mutate(count = sum(wt,na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(endtime, rl_countryOfBirth) %>%
  mutate(total_count = sum(wt, na.rm = TRUE)) %>%
  mutate(ratio = count / total_count) %>%
  ggplot(aes(x = endtime, y= ratio, group = interaction(vote, rl_countryOfBirth), 
             colour =  rl_countryOfBirth)) + geom_line() + geom_point() +
  annotate("segment", x = ref_time, xend = ref_time, y = 0, yend = 0.8,
           colour = "black") +
  annotate("text", x = ref_time, y = 0.9, label = "Referendum")

ggplotly(grp_p) %>% 
  layout(autosize=TRUE) %>%
  # plot_ly(data(), y = ~immig_index5, x = ~voter_type, color = paste0("~",input$group_choice), type = "box") %>%
  layout(boxmode = "group")


#---------------------------------------------------------------------------------------------------
immig_check <- read.csv("data/immig_cols.csv") %>%
  gather(questions, response, -X, -id) %>% 
  arrange(id)
  #mutate(qn = ifelse(questions))



check <- immig_check %>%
  group_by(id, questions) %>%
    mutate(response = as.numeric(response)) %>%
    filter(!is.na(response)) %>%
  mutate(sd = sd(response, na.rm = TRUE))

#---------------------------------------------------------------------------------------------------
# get all the combi options of the 4 immig questions:
response <- c(0,0,0,0)
questions <- c(1/5,1/5,1/7,1/7)
change <-c()
for(i in 1:4) {
  response[i] = 1;
  for(j in i:4){
    response[j] = 1;
    print(response)
    change <- c(change, sum(questions*response))
    response[j] = 0;
  }
}

  
#---------------------------------------------------------------------------------------------------
## To investigate different dimensions of the immigration sentiments:
# * Perception of size
# * Economy
# * Cultural
# * Welfare State
library(readr)
immig_check <- read.csv("data/immig_cols.csv") %>%
  gather(questions, response, -X, -id) %>% 
  arrange(id) %>%
  select(-X) %>%
  mutate(wave = parse_number(questions)) %>%
  mutate(questions = sub("W\\d+","",questions)) %>%
  filter(questions %in% c("changeImmig", "immigrantsWelfareState",
                          "immigCultural", "immigEcon", "immigrationLevel"))%>%
  mutate(std = ifelse(questions %in% c("changeImmig", "immigrantsWelfareState"),
         (response-1)/4,
         ifelse(questions == "immigrationLevel",
                1-(response-1)/4,
                1-(response-1)/6))) %>%
  mutate(std = ifelse(std > 1 | std < 0, NA, std))
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
all_13_id <- read.csv("data/all_waves.csv") %>%
  select(id)

check <- all_13_id %>%
  inner_join(immig_check) %>%
  group_by(id,questions) %>%
  arrange(id,questions,wave) %>%
  mutate(full_std = fillNAgaps(std)) %>%
  mutate(pre = lag(full_std,1)) %>%
  mutate(change = full_std - pre) %>%
  mutate(tag = ifelse(questions %in% c("changeImmig", "immigrantsWelfareState") &
                  change >= 0.25,  #0.25 count 'jump at least one level' as a change 
                  "more-anti",
                  ifelse(questions %in% c("changeImmig", "immigrantsWelfareState") &
                           change <= -0.25, 
                         "less-anti",
                         ifelse(questions %in% c("immigCultural", "immigEcon") &
                                  change >= 0.166, #0.333, # 0.33 count 'jump at least two levels' as a change
                                "more-anti",
                                ifelse(questions %in% c("immigCultural", "immigEcon") &
                                         change <= -0.166,  #-0.333, 
                                       "less-anti",
                                       ifelse(is.na(change), NA,
                                              "no change"))))))

more <- check %>%
  filter(!is.na(tag))%>%
  group_by(questions, wave, tag) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  group_by(questions, wave) %>%
  mutate(total = n()) %>%
  mutate(ratio = num/total) %>%
  filter(questions == "immigrantsWelfareState")%>%
  ungroup() %>%
  select(wave, ratio, tag) %>%
  unique() %>%
  ggplot(aes(x = wave, y = ratio, group = tag, fill = tag)) + 
  geom_bar(stat = "identity") +
scale_x_continuous(breaks = 2:13) +
  #ggtitle("Response to questions: \nDo you think immigration is good or bad \nfor Britain’s economy?")
ggtitle("Response to question: \n'Immigrants are a burden on the welfare state.' ")

more <- check %>%
  filter(!is.na(tag))%>%
  group_by(questions, wave, tag) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  group_by(questions, wave) %>%
  mutate(total = n()) %>%
  mutate(ratio = num/total) %>%
  filter(tag=="more-anti") %>%
  ggplot(aes(x = wave, y = ratio, group = questions,color = questions )) + geom_line() +geom_point()+
  scale_x_continuous(breaks = 2:13)+
  ggtitle("more anti-immigrant")


less <- check %>%
  filter(!is.na(tag))%>%
  group_by(questions, wave, tag) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  group_by(questions, wave) %>%
  mutate(total = n()) %>%
  mutate(ratio = num/total) %>%
  filter(tag=="less-anti") %>%
  ggplot(aes(x = wave, y = ratio, group = questions,color = questions )) + geom_line() +geom_point()+
  scale_x_continuous(breaks = 2:13)+
  ggtitle("less anti-immigrant")


no_change <-check %>%
  group_by(questions, wave, tag) %>%
  mutate(num = n()) %>%
  ungroup() %>%
  group_by(questions, wave) %>%
  mutate(total = n()) %>%
  mutate(ratio = num/total) %>%
  filter(tag=="no change") %>%
  ggplot(aes(x = wave, y = ratio, group = questions,color = questions )) + geom_line() +geom_point()+
scale_x_continuous(breaks = 2:13) +
  ggtitle("relatively constant")

library(gridExtra)

grid.arrange(more, less, no_change, ncol = 1)

#---------------------------------------------------------------------------------------------------
# Max and mean for all people
# TODO: add immigLevel
max_min<-immig_check %>%
  group_by(id, questions) %>%
  filter(questions!="immigrationLevel")%>%
  mutate(max = max(std, na.rm = TRUE),
         min = min(std, na.rm = TRUE),
         diff = max - min) %>%
  mutate(tag = ifelse(questions %in% c("changeImmig", "immigrantsWelfareState") &
                        diff >= 0.5,  #jump at least two level' 
                      "significant change",
           ifelse(questions %in% c("changeImmig", "immigrantsWelfareState") &
                        diff >= 0.25,  #jump at least one level'
                      "tolerable change",
                        ifelse(questions %in% c("immigCultural", "immigEcon") &
                                      diff >= 0.5, # jump at least three levels' 
                                    "significant change",
                               ifelse(questions %in% c("immigCultural", "immigEcon") &
                                        diff >= 0.333, #'jump at least two levels' 
                                      "tolerable change",
                                           ifelse(is.na(diff), NA,
                                                  "no change"))))))
max_min$tag<-factor(as.factor(max_min$tag),level =  c("significant change",
"tolerable change", "no change", NA))

require(scales)

max_min_count <- max_min %>%
  ungroup()%>%
  filter(!is.na(tag))%>%
  select(id,questions,tag) %>%
  unique() %>%
  group_by(questions,tag)%>%
  mutate(count = n()) %>%
  ungroup() %>%
  group_by(questions) %>%
  mutate(total = n())

ct2 <- max_min_count %>%
  ungroup()%>%
  arrange(questions) %>%
  select(total) %>%
  unique()%>%
  unlist(total, use.names = FALSE)

max_min_count %>%  
  mutate(ratio = count/total) %>%
  select(ratio, questions, tag) %>%
  unique() %>%
  ggplot(aes(x = questions, y = ratio, group = tag, fill = tag)) +
  geom_bar(stat = "identity") +
  labs(y = "Percent of the respondents", x = "Questions", 
       fill = "Range of change \n(Max - Min)") +
  annotate("text", x = 1:4, y = 0.1, label = c(as.character(ct2)))



