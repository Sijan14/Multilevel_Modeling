library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)

# Load the model
df <- read_sav("lq2002.sav")
attach(df)

# Null model
null_model <- lmer(LEAD ~ (1|COMPID), REML = FALSE, data = df)
summary(null_model)
rand(null_model)

icc = 0.07225/ (0.07225 + 0.61516)
icc
