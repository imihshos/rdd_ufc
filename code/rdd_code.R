# Remove everything from environment
rm(list = ls(all = TRUE)); graphics.off() ; gc()

# Libraries
library(magrittr)
library(tidyverse)
library(rdd)
library(rddtools)

## Data Preparation 
path_root = "."
path_data=file.path(path_root, "data")
df <- read.csv(file.path(path_data, "ufc-master.csv"))

# round odds to nearest 10
df$R_odds = round(df$R_odds/10)*10

# find the percentage of wins for each odds
covariates_col <- c( "R_wins", "R_losses", "win_dif", "loss_dif", "win_streak_dif", "lose_streak_dif", "total_round_dif", "longest_win_streak_dif")
R_odds_prob <- df %>% select(R_odds, Winner, covariates_col) %>% mutate(R_odds=ifelse(R_odds>0, R_odds-100, R_odds+100)) %>% filter(R_odds<500 & R_odds>-500 ) %>% mutate(WinCount=ifelse(Winner=="Red", 1,0)) %>% group_by(R_odds) %>% summarise(across(everything(), mean)) %>% select(-Winner)

R_odds_prob = R_odds_prob %>% mutate(percentage_win=WinCount, treatment = ifelse(R_odds >= 0, TRUE, FALSE))

## RDD

R_odds_prob %>% 
  ggplot(aes(x = R_odds, y = percentage_win, color = treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")

cov_df <- R_odds_prob %>% select(covariates_col)
rdd_data_cov<-rdd_data(R_odds_prob$percentage_win, R_odds_prob$R_odds, covar=cov_df, cutpoint = 0)

rdd_mod <- rdd_reg_lm(rdd_object = rdd_data_cov, slope = "separate")
rdd_mod
#check for covariates balance 
covarTest_mean(rdd_data_cov)

## Regression Adjustment using parametric fit 

# Create df for regression
reg_df <- df %>% select(R_odds, Winner, R_losses, loss_dif,win_streak_dif,lose_streak_dif,total_round_dif, longest_win_streak_dif) %>% mutate(R_odds=ifelse(R_odds>0, R_odds-100, R_odds+100)) %>% mutate(R_favourite = ifelse(R_odds >= 0, TRUE, FALSE)) %>% mutate(R_win=ifelse(Winner=="Red", 1,0)) %>% mutate(R_odds_cubic=R_odds**3) %>% select(-Winner)

# linear regression
reg_df_linear <- reg_df %>% select(-R_odds_cubic)
reg <- lm(data = reg_df_linear, R_win ~ .)
summary(reg)

# nonlinear regression
nl_reg <- lm(data = reg_df, R_win ~ .)
summary(nl_reg)