library(tidyverse)
library(haven)

# Loading the datasets
wave1 <- readxl::read_excel("session2_time1.xlsx")
wave2 <- readxl::read_excel("session2_time2.xlsx")

# Renaming columns
wave1 <- rename(wave1, anxietyT1 = anxiety)
wave2 <- rename(wave2, anxietyT2 = anxiety)

# Merging the dfs
merged <- left_join(wave1, wave2, by = "id")
  
# Descriptive Stats
merged %>% 
  summarize(Mean_anxietyT1 = mean(anxietyT1),
            Sd_anxietyT1 = sd(anxietyT1),
            Mean_anxietyT2 = mean(anxietyT2),
            Sd_anxietyT2 = sd(anxietyT2))

# t-test
merged_long <- merged %>% 
  gather(key = "wave", value = "anxiety_score", "anxietyT1", "anxietyT2")

t.test(anxiety_score ~ wave, data = merged_long, paired = TRUE, var.equal = TRUE)

# group by function
merged_long %>% 
  group_by(wave) %>% 
  summarise(Mean = mean(anxiety_score),
            SD = sd(anxiety_score))

# generate random numbers
data.frame(random_num) <- rnorm(50,mean = 10, sd = 2.5)

