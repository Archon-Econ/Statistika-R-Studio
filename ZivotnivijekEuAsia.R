library(WDI)
library(dplyr)
library(ggplot2)

# Define indicators
indicators <- c(GDP = "NY.GDP.PCAP.CD", broj_GODINA = "SP.DYN.LE00.IN")

# Fetch data from WDI
podacii <- WDI(country = "all", indicator = indicators, start = 2021, end = 2021)

# List of EU countries
eu_zemlje <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
  "Slovenia", "Spain", "Sweden"
)

# Filter and process EU data
eu_podacii <- podacii %>%
  filter(country %in% eu_zemlje) %>%
  select(country, GDP, broj_GODINA) %>%
  na.omit() %>%
  mutate(kontinent = "EU")

# Manually assign region for Asia countries
azija_zemlje <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
  "Brunei Darussalam", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia",
  "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", 
  "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
  "Myanmar", "Nepal", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Republic of Korea", 
  "Sri Lanka", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", 
  "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Viet Nam", "Yemen"
)

# Filter and process Asia data
azija_podacii <- podacii %>%
  filter(country %in% azija_zemlje) %>%
  select(country, GDP, broj_GODINA) %>%
  na.omit() %>%
  mutate(kontinent = "Asia")

# Combine EU and Asia data
kombinovano <- bind_rows(eu_podacii, azija_podacii)

# Create scatter plot
ggplot(kombinovano, aes(x = GDP, y = broj_GODINA, color = kontinent, label = country)) +
  geom_point(size = 3) +
  geom_text(vjust = -0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, aes(group = kontinent), linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "GDP Vs Zivotni vijek EU vs Asia (2021)",
    x = "GDP",
    y = "Zivotni vijek",
    color = "Regija"
  ) +
  theme_minimal()

# Fit linear models
model_eu <- lm(broj_GODINA ~ log10(GDP), data = eu_podacii)
model_azija <- lm(broj_GODINA ~ log10(GDP), data = azija_podacii)

# Display model summaries
summary(model_eu)
summary(model_azija)

