library(tidyverse)
library(haven)
library(readxl)
library(lme4)
library(lmerTest)

company_A <- read_excel("session4_companyA.xlsx")
company_B <- read_csv("session4_companyB.csv")
company_C <- read_sav("session4_companyC.sav")

# Create unique company ID
company_A$company_id <- 1
company_B$company_id <- 2
company_C$company_id <- 3

# Recode individual id values
company_B <- company_B %>% 
  mutate(id = id + nrow(company_A))

company_C <- company_C %>% 
  mutate(id = id + nrow(company_B))

# Append the three company data
names(company_A)
names(company_B)
names(company_C)

company_A <- rename(company_A, "job_satisfaction" = "js")
company_C <- rename(company_C, "job_satisfaction" = "Job_Satisfaction")

df <- bind_rows(company_A, company_B, company_C)

# Create a Null Model

attach(df)

null_model <- lme4::lmer(job_satisfaction ~ (1|company_id), data = df, REML = FALSE)
summary(null_model)

# ICC
icc <- 0.9078 / (0.1527 + 0.9078)
icc

# job satisfaction scores by group
df %>% 
  group_by(company_id) %>% 
  summarise(Mean = mean(job_satisfaction), SD = sd(job_satisfaction))
