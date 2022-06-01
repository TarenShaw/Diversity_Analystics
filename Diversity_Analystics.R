pacman::p_load(tidyverse, readr, skimr, janitor)

df <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv") %>% clean_names()

skim(df)

glimpse(df)

table(df$gender,df$marital_status)

table(df$gender)

df %>% count(gender) %>% mutate(n_tot = sum(n), p_tot = n/n_tot)

ggplot(data=df) +      
  geom_bar(aes(x= job_role, fill=gender), , position = "fill") +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer()


