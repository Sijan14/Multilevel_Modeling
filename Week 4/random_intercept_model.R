library(tidyverse)
library(haven)
library(lme4)
library(lmerTest)

df <- read_sav("lq2002.sav")

attach(df)

# Aggregate Score
df <- df %>% 
  group_by(COMPID) %>% 
  mutate(GTSIG_check = mean(TSIG))

cor(df$GTSIG, df$GTSIG_check) # checks out

"""
• HOSTILE: Individual-level Hostility Scale score is the outcome variable.
• TSIG: Individual-level Task Significance score is a level 1 covariate.
• COMPID: This is the level 2 grouping (level) variable, defining 49 groups.
• GTSIG: Company-level aggregate Task Significance score is a level 2 covariate.
"""

# Null Model
NullModel <- lmer(HOSTILE ~ (1|COMPID), REML = F, data = df)
summary(NullModel)

# Random Intercept Model
RIModel <- lmer(HOSTILE ~ TSIG + GTSIG + (1|COMPID), REML = F, data = df)
summary(RIModel)

# Random effects (A Likelihood Ratio Test with a reduced model {random effect removed})
rand(RIModel)
"The random effect (1 | COMPID) significantly improves the model fit."

step(RIModel)
"The step() command will drop terms to arrive at a final model. However, in this example there are no non-significant terms, so the final model is the starting model."

# Likelihood Ratio Test
anova(NullModel, RIModel)

# or
library(lmtest)
lrtest(NullModel, RIModel)

# scaled residuals
quantile(residuals(RIModel, "pearson", scaled = TRUE))


