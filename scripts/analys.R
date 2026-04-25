
source("scripts/stadning.R")

# Fördelning av kostnaderna
p_cost_distribution <- ggplot(insurance_costs_clean, aes(x = charges)) +
  geom_histogram(fill = "steelblue", bins = 20)

p_cost_distribution

# Histogramet visar att försäkringskostnaderna för de flesta ligger runt 10000kr, dock har en del kunder en högre kostnad.


# Samband mellan ålder och kostnad
insurance_costs_clean %>%
  summarise(correlation = cor(age, charges, use = "complete.obs"))

p_age_charge <- ggplot(insurance_costs_clean, aes(x= age, y = charges)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Correlation between age and cost",
    x = "Age",
    y = "Cost"
  ) +
  theme_minimal()

p_age_charge

# Correlation = 0.258
# Finns ett positivt samband mellan ålder och kostnader, som visar att äldre har högre försäkringskostnader.


# Samband mellan BMI och kostnad 
insurance_costs_clean %>%
  summarise(correlation = cor(bmi, charges, use = "complete.obs"))

p_bmi_charge <- ggplot(insurance_costs_clean, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Correlation mellan BMI och cost",
    x = "BMI",
    y = "Cost"
  ) +
  theme_minimal()

p_bmi_charge

# Correlation = 0.152
# Sambandet mellan BMI och kostnader är positivt, vilket tyder på att ett högre BMI värde kan vara kopplat till högre kostnader.


# Skillnad mellan BMI grupper
insurance_costs_clean %>%
  group_by(bmi_category) %>%
  summarise(avg_cost = mean(charges, na.rm = TRUE))

p_bmi_groups <- ggplot(insurance_costs_clean, aes(x = bmi_category, y = charges)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Cost for BMI-categories",
    x = "BMI",
    y = "Cost"
    ) +
  coord_flip() +
  theme_minimal() 

p_bmi_groups

# Median för grupperna verkar öka i samband med BMI-kategori.
# För grupperna Overweight och Obese är kostnaderna större än för resterande grupper med median kostnad 10867kr respektive 10119kr, som tyder på att BMI-grupper kan påverka försäkringskostnader.



# Skillnader mellan rökare och icke-rökare
insurance_costs_clean %>%
  group_by(smoker) %>%
  summarise(avg_cost = mean(charges, na.rm = TRUE))


p_smoker_cost <- ggplot(insurance_costs_clean, aes(x =smoker, y =charges)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Cost for smoker vs non smoker",
    x = "Smoking?",
    y = "Cost"
  ) +
  theme_minimal() 

p_smoker_cost

# Medianen för kostnader för de som röker verkar dubbelt så stor som för icke-rökare som har en kostnad på 8586kr och för rökare 16537kr.
# Boxploten visar tydligt att kostnaden är högre för rökare än för icke-rökare som tyder på att rökning har en betydande roll för försäkringskostnaden.


# Samband mellan kronisk sjukdom och försäkringskostnad
insurance_costs_clean %>%
  group_by(chronic_condition) %>%
  summarise(avg_cost = mean(charges, na.rm = TRUE))

p_chronic_condition_cost <- ggplot(insurance_costs_clean, aes(x = chronic_condition, y = charges)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Cost depending on chronic condition",
    x = "Cronic disease?",
    y = "Cost"
    ) +
  theme_minimal() 

p_chronic_condition_cost

# Individer med kroniska sjukdommar har högre kostnader, som visar att hälsotillstånd är en betydande faktor som påverkar försäkringskostnader.



# Regressionsanalys






# Sparar visualiseringarna
ggsave("output/visualisering/cost_distribution.png", p_cost_distribution, width = 8, height = 5)
ggsave("output/visualisering/age_charge.png", p_age_charge, width = 8, height = 5)
ggsave("output/visualisering/bmi_charge.png", p_bmi_charge, width = 8, height = 5)
ggsave("output/visualisering/bmi_groups.png", p_bmi_groups, width = 8, height = 5)
ggsave("output/visualisering/smoker_cost.png", p_smoker_cost, width = 8, height = 5)
ggsave("output/visualisering/chronic_condition_cost.png", p_chronic_condition_cost, width = 8, height = 5)

