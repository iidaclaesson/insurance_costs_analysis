insurance_costs_raw <- read_csv("data/insurance_costs.csv")
insurance_costs_clean <- insurance_costs_raw 

names(insurance_costs_clean)

colSums(is.na(insurance_costs_clean))



insurance_costs_clean <- insurance_costs_clean %>%
  mutate(
    region = str_to_lower(region),
    smoker = str_to_lower(smoker),
    plan_type = str_to_lower(plan_type)
  )

# Ändrar datatyper
insurance_costs_clean <- insurance_costs_clean %>%
  mutate(
    sex = as.factor(sex),
    smoker = as.factor(smoker),
    region = as.factor(region),
    plan_type = as.factor(plan_type),
    chronic_condition = as.factor(chronic_condition),
    exercise_level = as.factor(exercise_level)
  )






# Skapar nya variabler
insurance_costs_clean <- insurance_costs_clean %>%
  mutate(
    bmi_category = case_when(
      bmi < 18.5 ~ "Underweight",
      bmi < 25 ~ "Normal",
      bmi < 30 ~ "Overweight",
      TRUE ~ "Obese"
    )
  )

insurance_costs_clean <- insurance_costs_clean %>%
  mutate(
    bmi_category = factor(
      bmi_category,
      levels = c("Underweight", "Normal", "Overweight", "Obese"),
      ordered = TRUE
    )
  )

insurance_costs_clean <- insurance_costs_clean %>%
  mutate(
    age_group = case_when(
      age < 30 ~ "Young",
      age < 50 ~ "Middle",
      TRUE ~ "Older"
    )
  )

insurance_costs_clean <- insurance_costs_clean %>%
  mutate(
    risk_score = prior_accidents + prior_claims
  )



glimpse(insurance_costs_clean)


# Saknade värden
# - bmi hade 28 saknade värden
# - annual_checkups hade 20 saknade värden
# - exercise_level hade 22 saknade värden
# - Saknade värden är relativt få så de lämnas och hanteras med na.rm = TRUE 

