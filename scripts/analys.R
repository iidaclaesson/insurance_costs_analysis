

# Fördelning av kostnaderna

ggplot(insurance_costs_clean, aes(x = charges)) +
  geom_histogram(fill = "steelblue", bins = 40)

#TOLKNING


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

