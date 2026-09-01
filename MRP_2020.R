############################
# MRP ----- 2020
# Last modified: 12/15/2023
# Group Nº4
# Data Science for Political Analytics
############################

library(tidyverse)
library(rstanarm)
library(Matrix)
library(brms)
library(bayesplot)
theme_set(bayesplot::theme_default())
library(ggplot2)
library(ggthemes)

#Post-stratification data
url='https://raw.githubusercontent.com/JuanLopezMartin/MRPCaseStudy/master/data_public/chapter1/data/poststrat_df.csv'
poststrat_df1 <- read_csv(url)
View(poststrat_df1)

# Recode some variable to merge with the anes dataset
poststrat_df <- poststrat_df1 %>%
  mutate(educ = ifelse(educ == 'Post-grad', '4-Year College +', ifelse(
    educ == '4-Year College', '4-Year College +', educ
  )))

#Setting working directory
setwd('/Users/jordanschmidt/Desktop/POLS4716/Final Project /Datasets')
anes <- read_csv('anes2020')

#Recoding outcome variable
newanes <- anes %>% mutate(predictor = case_when(
  VCF0806 == 1 ~ 1,
  VCF0806 == 2 ~ 1,
  VCF0806 == 3 ~ 1,
  VCF0806 == 4 ~ 0,
  VCF0806 == 5 ~ 0,
  VCF0806 == 6 ~ 0,
  VCF0806 == 7 ~ 0,
  TRUE ~ NA
))

newanes <- newanes %>%
  mutate(state = VCF0901b)

# Expand state level predictors to the individual level
statelevel_predictors_df <- read_csv('/Volumes/TOSHIBA EXT/1.1_Columbia University/Fall 2023/POLSGU4716_001_2023_3 - Data Science for Political Analytics/data_science_pol_anlt/Project/data/statelevel_predictors.csv')
newanes <- left_join(newanes, statelevel_predictors, by = 'state')


# Fit in stan_glmer
new_fit <- stan_glmer(predictor ~ (1 | state) + (1 | eth) + (1 | educ) + male +
                        (1 | male:eth) + (1 | educ:age) + (1 | educ:eth) + repvote,
                      family = binomial(link = "logit"),
                      data = newanes,
                      prior = normal(0, 1, autoscale = TRUE),
                      prior_covariance = decov(scale = 0.50),
                      adapt_delta = 0.99)

print(new_fit)

# Expand state level predictors to the post-stratification
poststrat_df <- left_join(poststrat_df1, statelevel_predictors_df, by = "state")

epred_mat <- posterior_epred(fit, newdata = poststrat_df, draws = 1000)
mrp_estimates_vector <- epred_mat %*% poststrat_df$n / sum(poststrat_df$n)
mrp_estimate <- c(mean = mean(mrp_estimates_vector), sd = sd(mrp_estimates_vector))

######################
# Estimates for states
##################### 

state_df <- data.frame(
  State = 1:50,
  model_state_sd = rep(-1, 50),
  model_state_pref = rep(-1, 50),
  sample_state_pref = rep(-1, 50),
  true_state_pref = rep(-1, 50),
  N = rep(-1, 50)
)

states <- c("AK", "AZ", "AR", "CA", "CO", "CT", "DE", "HI", "IL", "IN", "IA", "KS", "KY", "LA", "MD", "MA", "MI", "MN", "MT", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OR", "PA", "RI", "VT", "VA", "WA", "WV", "WI", "WY", 'OK', 'NE', 'ID', 'MO', 'UT', 'SD', 'ME', 'TX', 'AL', 'GA', 'MS', 'TN', 'FL', 'SC')

for (i in 1:length(states)) {
  state <- states[i]
  poststrat_state <- poststrat_df[poststrat_df$state == state, ]
  posterior_prob_state <- posterior_linpred(
    new_fit,
    transform = TRUE,
    draws = 1000,
    newdata = as.data.frame(poststrat_state)
  ) 
  
  poststrat_prob_state <- (posterior_prob_state %*% poststrat_state$n) / sum(poststrat_state$n)
  
  #This is the estimate for popn in state:
  state_df$model_state_pref[i] <- round(mean(poststrat_prob_state), 4)
  state_df$model_state_sd[i] <- round(sd(poststrat_prob_state), 4)
  
}


estimates20 <- state_df[c(1:3)]
estimates20 <- estimates20 %>%
  mutate(state = recode(State, "1" = "Alaska",
                        "2" = "Arizona", 
                        "3" = "Arkansas",
                        "4" = "California", 
                        "5" = "Colorado", 
                        "6" = "Connecticut",
                        "7" = "Delaware", 
                        "8" = "Hawaii", 
                        "9" = "Illinois",
                        "10" = "Indiana",
                        "11" = "Iowa",
                        "12" = "Kansas",
                        "13" = "Kentucky",
                        "14" = "Louisiana",
                        "15" = "Maryland",
                        "16" = "Massachusetts",
                        "17" = "Michigan",
                        "18" = "Minnesota",
                        "19" = "Montana",
                        "20" = "Nevada",
                        "21" = "New Hampshire",
                        "22" = "New Jersey",
                        "23" = "New Mexico",
                        "24" = "New York",
                        "25" = "North Carolina",
                        "26" = "North Dakota",
                        "27" = "Ohio",
                        "28" = "Oregon",
                        "29" = "Pennsylvania",
                        "30" = "Rhode Island",
                        "31" = "Vermont",
                        "32" = "Virginia",
                        "33" = "Washington",
                        "34" = "West Virginia",
                        "35" = "Wisconsin",
                        "36" = "Wyoming",
                        "37" = "Oklahoma",
                        "38" = "Nebraska", 
                        "39" = "Idaho",
                        "40" = "Missouri", 
                        "41" = "Utah", 
                        "42" = "South Dakota",
                        "43" = "Maine", 
                        "44" = "Texas",
                        "45" = "Alabama", 
                        "46" = "Georgia",
                        "47" = "Mississippi", 
                        "48" = "Tennessee", 
                        "49" = "Florida",
                        "50" = "South Carolina")) %>% 
  select(state, model_state_pref, model_state_sd)

### Saving estimates
write.csv(estimates20, '/Users/jordanschmidt/Desktop/POLS4716/Final Project /Datasets/2020estimates.cvs', row.names = TRUE)



