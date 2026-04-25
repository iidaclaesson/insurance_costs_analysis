

# Fördelning av kostnaderna

ggplot(insurance_costs_clean, aes(x = charges)) +
  geom_histogram(fill = "steelblue", bins = 20)

# Histogramet visar att försäkringskostnaderna för de flesta ligger runt 10000kr, dock har en del kunder en högre kostnad.


# Samband mellan ålder och kostnad

insurance_costs_clean %>%
  summarise(correlation = cor(age, charges, use = "complete.obs"))

p_age_charge <- ggplot(insurance_costs_clean, aes(x= age, y = charges)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Samband mellan ålder och kostnad",
    x = "Ålder",
    y = "Kostnad"
  ) +
  theme_minimal()

p_age_charge

# correlation=0.258
# Finns ett positivt samband mellan ålder och kostnader, som visar att äldre har högre försäkringskostnader


# Samband mellan BMI och kostnad 

insurance_costs_clean %>%
  summarise(correlation = cor(bmi, charges, use = "complete.obs"))

p_bmi_charge <- ggplot(insurance_costs_clean, aes(x = bmi, y = charges)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Samband mellan BMI och kostnad",
    x = "BMI",
    y = "Kostnad"
  ) +
  theme_minimal()

p_bmi_charge

# correlation = 0.152
# Sambandet mellan BMI och kostnader är positivt, vilket tyder på att ett högre BMI värde kan vara kopplat till högre kostnader



# Skillnad mellan BMI grupper

insurance_costs_clean %>%
  group_by(bmi_category) %>%
  summarise(avg_cost = mean(charges, na.rm = TRUE))

p_bmi_groups <- ggplot(insurance_costs_clean, aes(x = bmi_category, y = charges)) +
  geom_boxplot(alpha = 0.7) +
  labs(
    title = "Kostnad för BMI-kategorier",
    x = "BMI",
    y = "Kostnad"
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
    title = "Försäkringskostnad för rökare vs icke-rökare",
    x = "Rökare",
    y = "Kostnad"
  ) +
  coord_flip() +
  theme_minimal() 

p_smoker_cost

# Medianen för försäkringskostnader för de som röker verkar dubbelt så stor som för icke-rökare som har en kostnad på 8586kr och för rökare 16537kr.
# Boxploten visar tydligt att försäkringskostnaden är högre för rökare än för icke-rökare som tyder på att rökning har en betydande roll

