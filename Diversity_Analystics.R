pacman::p_load(tidyverse, readr, skimr, janitor)

df <- read_csv("WA_Fn-UseC_-HR-Employee-Attrition.csv") %>% clean_names()

skim(df)

glimpse(df)

table(df$gender, df$marital_status)

table(df$gender)

df %>%
  group_by(gender, job_role) %>%
  summarise(n = n()) %>%
  tibble() %>%
  left_join(., df %>% count(job_role), by = "job_role") %>%
  mutate(prop = n.x / n.y)

male_fct <- df %>%
  group_by(gender, job_role) %>%
  summarise(n = n()) %>%
  tibble() %>%
  left_join(., df %>% count(job_role), by = "job_role") %>%
  mutate(prop = n.x / n.y) %>%
  filter(gender == "Male") %>%
  select(job_role, prop) %>%
  arrange(desc(prop)) %>% print()

df$job_role <- factor(df$job_role)

df <- left_join(df, male_fct, by = "job_role")

df <- df %>% mutate(job_role = fct_reorder(df$job_role, df$prop, max))

ggplot(data = df) +
  geom_bar(aes(x = job_role, fill = gender), position = "fill") +
  coord_flip() +
  theme_bw() +
  scale_fill_brewer()
