library(tidyverse)
library(skimr)
library(patchwork)

insurance_costs_raw <- read_csv("data/insurance_costs.csv")


# Översikt över data
glimpse(insurance_costs_raw)
skim_without_charts(insurance_costs_raw)


# Kontrollera outliers
insurance_costs_raw %>%
  arrange(desc(charges)) %>%
  select(age, bmi, smoker, charges) %>%
  head(10)


# Kategoriska variabler
insurance_costs_raw %>% distinct(sex)
insurance_costs_raw %>% distinct(region)
insurance_costs_raw %>% distinct(smoker)
insurance_costs_raw %>% distinct(chronic_condition)
insurance_costs_raw %>% distinct(exercise_level)
insurance_costs_raw %>% distinct(plan_type)



insurance_costs_raw %>% count(sex, sort = TRUE)
insurance_costs_raw %>% count(region, sort = TRUE)
insurance_costs_raw %>% count(smoker, sort = TRUE)
insurance_costs_raw %>% count(plan_type, sort = TRUE)



p1 <- ggplot(insurance_costs_raw, aes(x = sex)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Fördelning av kön")

p2 <- ggplot(insurance_costs_raw, aes(x = bmi)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "Fördelning av BMI")

p3 <- ggplot(insurance_costs_raw, aes(x = smoker)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Antal rökare vs icke-rökare")

p4 <- ggplot(insurance_costs_raw, aes(x = charges)) +
  geom_histogram(fill = "steelblue") +
  labs(title = "Fördelning av försäkringskostnad")

p1 + p2 + p3 + p4


# Viktiga datatyper:
# - age, region, bmi, children, smoker, chronic_condition, exercise_level, charges

# - sex, region, smoker, plan_type, chronic_condition, exercise_level -> faktorer
# - age, bmi, children, prior_accidents, prior_claims, annual_checkups charges -> numeriska
