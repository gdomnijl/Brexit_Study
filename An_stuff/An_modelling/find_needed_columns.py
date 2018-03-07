import pandas as pd
import numpy as np

print("started running")
df = pd.read_stata("/Users/An/Desktop/Classes/AppliedDataScience/BES2017_W13_Panel_v1.2.dta")

df_waves = df.loc[:,"wave1":"wave13"]
df_euRefVote = df.loc[:,"euRefVote1":"euRefVote13"]

df = df.loc[:,['id','country', 'countryOfBirth', 'disability', 'ageGroup',
       'euRefLA', 'onscode', 'headHouseholdPast', 'fatherNumEmployees',
       'motherNumEmployees', 'gender', 'Age', 'marital', 'housing', 'gor',
       'profile_education_age', 'profile_ethnicity', 'profile_lea',
       'profile_oslaua', 'profile_gross_household', 'profile_gross_personal',
       'profile_household_size', 'profile_household_children',
       'profile_newspaper', 'profile_past_vote_2005', 'profile_past_vote_2010',
       'profile_religion', 'profile_religion_denom', 'profile_house_tenure',
       'profile_socialgrade_cie', 'profile_past_vote_2015',
       'profile_turnout_2015', 'profile_eurefvote', 'profile_eurefturnout',
       'personality_agreeableness', 'personality_conscientiousness',
       'personality_extraversion', 'personality_neuroticism',
       'personality_openness']]
df = pd.concat([df_waves,df_euRefVote,df], axis=1)
df.to_csv("static_var_and_switches.csv")

def compute_switch(df):
    row = df.iloc[1,:]