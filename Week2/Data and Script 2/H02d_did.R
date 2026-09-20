########################################################################################## 
# H02d: Difference in Differences (DID) Model
# By William Yu, Harvard Extension
# 5/19/2023
# https://lfoswald.github.io/2021-spring-stats2/materials/session-8/08-online-tutorial/
########################################################################################## 
library(dplyr)
library(ggplot2)
library(tidyr)

soda_tax_df = read.csv("https://raw.githubusercontent.com/seramirezruiz/hertiestats2/master/data/soda_tax_df.csv") 

# Method 1:
soda_tax_df = soda_tax_df %>% mutate(change = post_tax - pre_tax) 
did01 = lm(change ~ treatment, data = soda_tax_df)
summary(did01)

# Method 2:
soda_tax_df_long = soda_tax_df %>% 
  pivot_longer(cols = c(pre_tax, post_tax), names_to = "period", values_to = "soda_drank") %>% 
  mutate(after_tax = ifelse(period == "post_tax", 1, 0))

head(soda_tax_df_long, 10)

did02 = lm(soda_drank ~ treatment + after_tax + treatment*after_tax, data = soda_tax_df_long) 
summary(did02)
