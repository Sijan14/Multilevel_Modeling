library(haven)
library(tidyverse)
library(lme4)
library(lmerTest)

df <- read_csv("schools.csv")

## NULL MODEL
names(df)
attach(df)

null_model <- lmer(math_score ~ (1|school), REML = F, data = df)
summary(null_model)

# Intraclass Coefficient
8.553 / (39.148 + 8.553)

## RANDOM INTERCEPT MODEL
df <- df %>% 
  group_by(school) %>% 
  mutate(gr_ses = mean(ses, na.rm = T))

options(scipen = 999)

ri_model <- lmer(math_score ~ ses + gr_ses + school_size + private + (1|school), REML = F, data = df)
summary(ri_model)
rand(ri_model)

# BG-PRV
null_BG <- 8.553
ri_BG <- 2.256

BG_PRV <- (null_BG - ri_BG) / null_BG
BG_PRV

# WG-PRV
null_WG <- 39.148
ri_WG <- 37.015

WG_PRV <- (null_WG - ri_WG) / null_WG
WG_PRV

## RANDOM SLOPE MODEL
rs_model <- lmer(math_score ~ ses + gr_ses + school_size + private + (1|school) + (1|school:ses), REML = F, data = df)
summary(rs_model)
rand(rs_model)
