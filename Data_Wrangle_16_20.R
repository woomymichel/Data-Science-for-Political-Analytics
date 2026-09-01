################################################################################
# Data Wrangle ANES 2016 & 2020
# Group Nº4
# Data Science for Political Analytics
################################################################################

setwd('/Volumes/TOSHIBA EXT/1.1_Columbia University/Fall 2023/POLSGU4716_001_2023_3 - Data Science for Political Analytics/data_science_pol_anlt/Project/data/')
library(haven)
library(tidyverse)

anes2016 <- read_csv('anes_timeseries_cdf_csv_20220916.csv')
anes2020 <- read_csv('anes_timeseries_cdf_csv_20220916.csv')

################################################################################
#Selecting variables for ANES 2016
################################################################################
anes2016 <- anes2016 %>% filter(VCF0004 == 2016) %>% select('VCF0004',
                                                            'VCF0017',
                                                            'VCF0050b',
                                                            'VCF0101',
                                                            'VCF0104', #Gender
                                                            'VCF0105b', #eth
                                                            'VCF0114',
                                                            'VCF0303',
                                                            'VCF0310',
                                                            'VCF0606',
                                                            'VCF0613',
                                                            'VCF0648',
                                                            'VCF0805',
                                                            'VCF0806',
                                                            'VCF0839',
                                                            'VCF0873',
                                                            'VCF0894',
                                                            'VCF0901b',
                                                            'VCF9004',
                                                            'VCF9077',
                                                            'VCF9131',
                                                            'VCF9150',
                                                            'VCF9154',
                                                            'VCF9151',
                                                            'VCF9155',
                                                            'VCF9251',
                                                            'VCF9281',
                                                            'VCF0101', #Age
                                                            'VCF0110') #Educ

#gender.race 
anes2016 <- anes2016 %>% 
  mutate(Gender.Race = case_when(VCF0105b == 1 & VCF0104 == 1 ~ 'Male White',
                                 VCF0105b == 2 & VCF0104 == 1 ~ 'Male Black',
                                 VCF0105b == 3 & VCF0104 == 1 ~ 'Male Hispanic',
                                 VCF0105b == 4 & VCF0104 == 1 ~ 'Male Other',
                                 VCF0105b == 1 & VCF0104 == 2 ~ 'Female White',
                                 VCF0105b == 2 & VCF0104 == 2 ~ 'Female Black',
                                 VCF0105b == 3 & VCF0104 == 2 ~ 'Female Hispanic',
                                 VCF0105b == 4 & VCF0104 == 2 ~ 'Female Other',
                                 VCF0105b == 1 & VCF0104 == 3 ~ 'Other White',
                                 VCF0105b == 2 & VCF0104 == 3 ~ 'Other Black',
                                 VCF0105b == 3 & VCF0104 == 3 ~ 'Other Hispanic',
                                 VCF0105b == 4 & VCF0104 == 3 ~ 'Other Other'))

countofstates <- anes2016 %>% count(VCF0901b)


## Recode some variables to be compatible with the poststratification data 
#Age
table(anes2016$VCF0101, anes2016$age)

anes2016 <- anes2016 %>%
mutate(age = case_when(
  VCF0101 >= 18 & VCF0101 < 30 ~ '18-29',
  VCF0101 >= 30 & VCF0101 < 39 ~ '30-39',
  VCF0101 >= 40 & VCF0101 < 49 ~ '40-49',
  VCF0101 >= 50 & VCF0101 < 59 ~ '50-59',
  VCF0101 >= 60 & VCF0101 < 69 ~ '60-69',
  VCF0101 >= 70 ~ '70+',
  TRUE ~ NA
))

# Education: No HS, HS, Some college, 4-year college, Post-grad 
table(anes2016$VCF0110)

anes2016 <- anes2016 %>%
mutate(educ = case_when(
  VCF0110 == 0 ~ 'No HS',
  VCF0110 == 1 ~ 'No HS',
  VCF0110 == 2 ~ 'HS',
  VCF0110 == 3 ~ 'Some college',
  VCF0110 == 4 ~ '4-year college +',
  TRUE ~ NA
))

table(anes2016$VCF0110, anes2016$educ)

#male
#Gender: Female, Male 
table(anes2016$VCF0104)


anes2016 <- anes2016 %>%
mutate(male = case_when(
  VCF0104 == 1 ~ 0.5,
  VCF0104 == 2 ~ -0.5,
  TRUE ~ NA
))

table(anes2016$VCF0104, anes2016$male)

#eth
#GEthnicity: (Non-hispanic) White, Black, Hispanic, Other (which also includes Mixed) (R=4)
 
table(anes2016$VCF0105b)

#1. White non-Hispanic
#2. Black non-Hispanic
#3. Hispanic
#4. Other or multiple races, non-Hispanic


anes2016 <- anes2016 %>%
mutate(eth = case_when(
  VCF0105b == 1 ~ 'White',
  VCF0105b == 2 ~ 'Black',
  VCF0105b == 3 ~ 'Hispanic',
  VCF0105b == 4 ~ 'Other',
  TRUE ~ NA
))

table(anes2016$VCF0105b, anes2016$eth)


write.csv(anes2016, 'anes2016.csv')

anes2016 <- anes2016 %>% mutate(predictor = case_when(
  VCF0806 == 1 ~ 1,
  VCF0806 == 2 ~ 1,
  VCF0806 == 3 ~ 1,
  VCF0806 == 4 ~ 0,
  VCF0806 == 5 ~ 0,
  VCF0806 == 6 ~ 0,
  VCF0806 == 7 ~ 0,
  TRUE ~ NA
))

### Creating descriptive table
df <- anes2016 %>%
  select(age, educ, male, eth, predictor)

datasummary_balance(~ 1, data = df, output = 'table.txt')

################################################################################
#Selecting variables for ANES 2020
################################################################################

#Selecting variables 
anes2020 <- anes2020 %>% filter(VCF0004 == 2020) %>% select('VCF0004',
                                                            'VCF0017',
                                                            'VCF0050b',
                                                            'VCF0101',
                                                            'VCF0104', #Gender
                                                            'VCF0105b', #eth
                                                            'VCF0114',
                                                            'VCF0303',
                                                            'VCF0310',
                                                            'VCF0606',
                                                            'VCF0613',
                                                            'VCF0648',
                                                            'VCF0805',
                                                            'VCF0806',
                                                            'VCF0839',
                                                            'VCF0873',
                                                            'VCF0894',
                                                            'VCF0901b',
                                                            'VCF9004',
                                                            'VCF9077',
                                                            'VCF9131',
                                                            'VCF9150',
                                                            'VCF9154',
                                                            'VCF9151',
                                                            'VCF9155',
                                                            'VCF9251',
                                                            'VCF9281',
                                                            'VCF0101', #Age
                                                            'VCF0110') #Educ

#gender.race 
anes2020 <- anes2020 %>% 
  mutate(Gender.Race = case_when(VCF0105b == 1 & VCF0104 == 1 ~ 'Male White',
                                 VCF0105b == 2 & VCF0104 == 1 ~ 'Male Black',
                                 VCF0105b == 3 & VCF0104 == 1 ~ 'Male Hispanic',
                                 VCF0105b == 4 & VCF0104 == 1 ~ 'Male Other',
                                 VCF0105b == 1 & VCF0104 == 2 ~ 'Female White',
                                 VCF0105b == 2 & VCF0104 == 2 ~ 'Female Black',
                                 VCF0105b == 3 & VCF0104 == 2 ~ 'Female Hispanic',
                                 VCF0105b == 4 & VCF0104 == 2 ~ 'Female Other',
                                 VCF0105b == 1 & VCF0104 == 3 ~ 'Other White',
                                 VCF0105b == 2 & VCF0104 == 3 ~ 'Other Black',
                                 VCF0105b == 3 & VCF0104 == 3 ~ 'Other Hispanic',
                                 VCF0105b == 4 & VCF0104 == 3 ~ 'Other Other'))

countofstates <- anes2020 %>% count(VCF0901b)


## Recode some variables to be compatible with the poststratification data 
#Age
table(anes2020$VCF0101, anes2020$age)

anes2020 <- anes2020 %>%
  mutate(age = case_when(
    VCF0101 >= 18 & VCF0101 < 30 ~ '18-29',
    VCF0101 >= 30 & VCF0101 < 39 ~ '30-39',
    VCF0101 >= 40 & VCF0101 < 49 ~ '40-49',
    VCF0101 >= 50 & VCF0101 < 59 ~ '50-59',
    VCF0101 >= 60 & VCF0101 < 69 ~ '60-69',
    VCF0101 >= 70 ~ '70+',
    TRUE ~ NA
  ))

# Education: No HS, HS, Some college, 4-year college, Post-grad 
table(anes2020$VCF0110)

anes2020 <- anes2020 %>%
  mutate(educ = case_when(
    VCF0110 == 0 ~ 'No HS',
    VCF0110 == 1 ~ 'No HS',
    VCF0110 == 2 ~ 'HS',
    VCF0110 == 3 ~ 'Some college',
    VCF0110 == 4 ~ '4-year college +',
    TRUE ~ NA
  ))

table(anes2020$VCF0110, anes2020$educ)

#male
#Gender: Female, Male 
table(anes2020$VCF0104)


anes2020 <- anes2020 %>%
  mutate(male = case_when(
    VCF0104 == 1 ~ 0.5,
    VCF0104 == 2 ~ -0.5,
    TRUE ~ NA
  ))

table(anes2020$VCF0104, anes2020$male)

#eth
#GEthnicity: (Non-hispanic) White, Black, Hispanic, Other (which also includes Mixed) (R=4)

table(anes2020$VCF0105b)

#1. White non-Hispanic
#2. Black non-Hispanic
#3. Hispanic
#4. Other or multiple races, non-Hispanic


anes2020 <- anes2020 %>%
  mutate(eth = case_when(
    VCF0105b == 1 ~ 'White',
    VCF0105b == 2 ~ 'Black',
    VCF0105b == 3 ~ 'Hispanic',
    VCF0105b == 4 ~ 'Other',
    TRUE ~ NA
  ))

table(anes2020$VCF0105b, anes2020$eth)

# Saving dataset 
write.csv(anes2020, 'anes2020.csv')

### Recoding our outcome variable
anes2020 <- anes2020 %>% mutate(predictor = case_when(
  VCF0806 == 1 ~ 1,
  VCF0806 == 2 ~ 1,
  VCF0806 == 3 ~ 1,
  VCF0806 == 4 ~ 0,
  VCF0806 == 5 ~ 0,
  VCF0806 == 6 ~ 0,
  VCF0806 == 7 ~ 0,
  TRUE ~ NA
))


### Creating descriptive table

df <- anes2020 %>%
  select(age, educ, male, eth, predictor)

datasummary_balance(~ 1, data = df, output = 'table2.txt')



