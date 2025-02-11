library(tidyverse)
library(haven)
library(readxl)
library(car)
library(lme4)
library(lmerTest)

## Part A: Multiple Linear Regression 

# Question 1
df_s <- read_sav("satisfaction.sav")

# Question 2 (labels)
attributes(df_s$contact)
attributes(df_s$dept)

# Question 3 to 7 (multiple linear regression)
model <- lm(satisfaction_mean ~ age + contact + as.factor(dept), data = df_s)
summary(model)

# Question 8
vif(model)

# Question 9
hist(residuals(model))

# Question 10
plot(model, 3)

## Part B: Data Wrangling Longitudinal Data
# Question 1
HW_time1 <- read_excel("HW_time1.xlsx")
HW_time2 <- read_excel("HW_time2.xlsx")

# Question 2
HW_time2 <- rename(HW_time2, "id" = "id_T2") # rename id column
merged_df <- inner_join(HW_time1, HW_time2, by = "id") # inner join

sum(is.na(merged_df)) # no missing data

cor(condition, condition_T2) # redundant

# Question 3
merged_df %>% 
  summarise(Time1_mean = mean(math), Time1_sd = sd(math),
            Time2_mean = mean(math_T2), Time2_sd = sd(math_T2))

# Question 4
t.test(math, math_T2, paired = TRUE)

# Question 5
merged_df <- rename(merged_df, "math_T1" = "math") # renaming time 1 match scores
long_df <- gather(merged_df, key = "time", value = "math_score", math_T1, math_T2)

# Question 6
long_df <- long_df %>% 
  select(!condition_T2) %>% 
  mutate(time = if_else(time == "math_T1", 1, 2)) %>% 
  arrange(id)
  
# Question 7
long_df %>% 
  group_by(condition, time) %>% 
  summarize(Mean = mean(math_score), SD = sd(math_score))

## Part C: Null Multilevel Models
# Question 1
df <- read_stata("cognitive.dta")

# Question 2 to 7
null_model <- lmer(arithmetic ~ (1|schoolid), REML = FALSE, data = df)
summary(null_model)

avg_student_per_school <- 2598/12
round(avg_student_per_school)

# Question 6
icc <- 0.1409 / (3.0648 + 0.1409)
icc
rand(null_model)

