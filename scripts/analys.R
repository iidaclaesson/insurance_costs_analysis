

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
