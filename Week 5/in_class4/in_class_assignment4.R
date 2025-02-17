library(tidyverse)
library(haven)
library(readxl)
library(car)
library(lme4)
library(lmerTest)

df <- read_sav("Nurses.sav")
attach(df)

# Null Model
null_model <- lmer(stress ~ (1|hospital), REML = F, data = df)
summary(null_model)

icc <- 0.2730 / (0.2730 + 0.6863)
icc

# Random Intercept Model
names(df)
df <- df %>% 
  group_by(hospital) %>% 
  mutate(mean_exp = mean(experien, na.rm = T)) %>% 
  ungroup()

ri_model <- lmer(stress ~ experien + age + mean_exp + as_factor(hospsize) + (1|hospital), REML = F, data = df)
summary(ri_model)

# Random Slope Model
rs_model <- lmer(stress ~ experien + age + mean_exp + as_factor(hospsize) + (1|hospital) + (1|hospital:experien), REML = F, data = df)
summary(rs_model)
rand(rs_model)
