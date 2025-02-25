library(lme4)
library(lmerTest)
library(tidyverse)
library(haven)
library(interactions)

df <- read_sav("Nurses.sav")

# Grand Mean Centering
df$experien_cent <- df$experien - mean(df$experien, na.rm = T)

# Group Mean Centering
df <- df %>% 
  group_by(hospital) %>% 
  mutate(Gr_experien = mean(experien, na.rm = T))

df <- df %>% 
  mutate(gr_experien_cent = experien - Gr_experien)

# Level 1 Interaction (Moderation)
ri_level1 <- lmer(stress ~ age*experien + (1|hospital), REML = F, data = df)
summary(ri_level1)

# Cross Level Interaction (Moderation)
ri_cross_level <- lmer(stress ~ experien*Gr_experien + (1|hospital), REML = F, data = df)
summary(ri_cross_level)

# Cross Level Interaction (Moderation)
ri_cross_levelf <- lmer(stress ~ experien*as_factor(hospsize) + (1|hospital), REML = F, data = df)
summary(ri_cross_levelf)

# Plotting Interactions
interact_plot(ri_level1, pred = age, modx = experien)

# Simple Slopes
sim_slopes(ri_level1, pred = age, modx = experien, modx.values = c(10, 15, 20))
