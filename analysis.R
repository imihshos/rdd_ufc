
### LIBRARIES ###
# First, we load the necessary libraries required for the causal analysis.

library(rdd)
library(rddtools)
library(magrittr)
library(tidyverse)

### DATA PREPARATION ###

path_root = "."
path_data=file.path(path_root, "data")
df <- read.csv(file.path(path_data, "ufc-master.csv"))

# round odds to nearest 10 so that there are no odds with only 1 data point.
df$R_odds = round(df$R_odds/10)*10

# Covariates selection
# Due to the large number of covariates in the dataset, we included only the following 8 covariates that we’ve deemed important in the analysis.
# - R_wins: Number of wins that “Red” fighter had before this fight
# - R_losses: Number of losses that “Red” fighter had before this fight
# - win_dif: Difference in total past wins between “Red” and “Blue” fighters
# - loss_dif: The difference in total past losses between “Red” and “Blue” fighters
# - win_streak_dif: Difference in win streak between fighters going into the fight
# - lose_streak_dif: Difference in lose streak between fighters going into the fight
# - total_round_dif: Difference in rounds fought between fighters going into the fight
# - longest_win_streak_dif: Difference in longest win streak going into the fight
 
covariates_col <- c( "R_wins", "R_losses", "win_dif", "loss_dif", "win_streak_dif", "lose_streak_dif", "total_round_dif", "longest_win_streak_dif")

# find the percentage of wins for each odds
R_odds_prob <- df %>% select(R_odds, Winner, covariates_col) %>% mutate(R_odds=ifelse(R_odds>0, R_odds-100, R_odds+100)) %>% filter(R_odds<500 & R_odds>-500 ) %>% mutate(WinCount=ifelse(Winner=="Red", 1,0)) %>% group_by(R_odds) %>% summarise(across(everything(), mean)) %>% select(-Winner)

R_odds_prob = R_odds_prob %>% mutate(percentage_win=WinCount, treatment = ifelse(R_odds >= 0, TRUE, FALSE))

R_odds_prob

### RDD: LINEAR REGRESSION ###

# In an initial analysis, we plot a simple linear regression. From the plot, it appears that the likelihood of “Red” fighters that are betting favourites (<0 R_odds) winning is about 15%. 

# We ran a parametric linear regression to determine the fitted value. The coefficient estimate is significant at 1% level of significance as p-value< 0.01. The RDD linear regression results suggests that the “Red” fighter being a favourite leads to 15.8% higher likelihood of winning.

R_odds_prob %>% 
  ggplot(aes(x = R_odds, y = percentage_win, color = treatment)) +
  geom_point() + 
  geom_smooth(method = "lm")

cov_df <- R_odds_prob %>% select(covariates_col)
rdd_data_cov<-rdd_data(R_odds_prob$percentage_win, R_odds_prob$R_odds, covar=cov_df, cutpoint = 0)

rdd_mod <- rdd_reg_lm(rdd_object = rdd_data_cov, slope = "separate")
rdd_mod


### COVARIATE BALANCE ###
#For RDD to be valid, the covariates need to be balanced to be as if RCT. 
#Based on the results, the betting favourites and underdogs are different in the covariate values for R_losses, loss_dif, lose_streak_dif, win_streak_dif,total_round_dif, longest_win_streak_dif, therefore there is no like vs like comparison between the two groups. To handle the covariate imbalance, OLS was used for regression adjustments.

covarTest_mean(rdd_data_cov)


### REGRESSION ADJUSTMENTS: PARAMETRIC FIT ###
# We ran the OLS regression using “Red” fighter winning as the dependent variable. “Red” fighter being a betting favourite, R_odds and the imbalanced covariates (R_losses, loss_dif, lose_streak_dif, win_streak_dif, total_round_dif,

# Create df for regression
reg_df <- df %>% select(R_odds, Winner, R_losses, loss_dif,win_streak_dif,lose_streak_dif,total_round_dif, longest_win_streak_dif) %>% mutate(R_odds=ifelse(R_odds>0, R_odds-100, R_odds+100)) %>% mutate(R_favourite = ifelse(R_odds >= 0, TRUE, FALSE)) %>% mutate(R_win=ifelse(Winner=="Red", 1,0)) %>% mutate(R_odds_cubic=R_odds**3) %>% select(-Winner)

# linear regression
reg_df_linear <- reg_df %>% select(-R_odds_cubic)
reg <- lm(data = reg_df_linear, R_win ~ .)
summary(reg)

# nonlinear regression
reg_nl <- lm(data = reg_df, R_win ~ .)
summary(reg_nl)

# In both the linear and non-linear fit, we find that R_favouriteTRUE is significant as p-value < 0.001. Under unconfounding assumption, the linear fit suggests that “Red” fighter being a favourite leads to 13.3% higher likelihood of winning while the non-linear fit suggests that “Red” fighter being a favourite leads to 8.3% higher likelihood of winning.
