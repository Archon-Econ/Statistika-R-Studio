library(WDI)
library(dplyr)
library(ggplot2)
library(ggflags)
library(countrycode)

indicators <- c(GDP = "NY.GDP.PCAP.CD", broj_GODINA = "SP.DYN.LE00.IN")
podacii <- WDI(country = "all", indicator = indicators, start = 2021, end = 2021)

eu_zemlje <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
               "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
               "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
               "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
               "Slovenia", "Spain", "Sweden")

azija_zemlje <- c("Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
                  "Brunei Darussalam", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia",
                  "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", 
                  "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
                  "Myanmar", "Nepal", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Republic of Korea", 
                  "Sri Lanka", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", 
                  "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Viet Nam", "Yemen")

eu_podacii <- podacii %>%
  filter(country %in% eu_zemlje) %>%
  select(country, GDP, broj_GODINA) %>%
  na.omit() %>%
  mutate(kontinent = "EU")

azija_podacii <- podacii %>%
  filter(country %in% azija_zemlje) %>%
  select(country, GDP, broj_GODINA) %>%
  na.omit() %>%
  mutate(kontinent = "Asia")

kombinovano <- bind_rows(eu_podacii, azija_podacii) %>%
  mutate(iso2 = tolower(countrycode(country, origin = "country.name", destination = "iso2c"))) %>%
  filter(!is.na(iso2))

ggplot(kombinovano, aes(x = GDP, y = broj_GODINA)) +
  geom_flag(aes(country = iso2, color = kontinent), size = 7.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = kontinent, color = kontinent), linetype = "dashed") +
  scale_x_log10() +
  labs(
    title = "EU vs Azija (2021)",
    x = "GDP",
    y = "Zivotni Vijek (Godine)",
    color = "Region"
  ) +
  theme_minimal()

model_eu <- lm(broj_GODINA ~ log10(GDP), data = eu_podacii)
model_azija <- lm(broj_GODINA ~ log10(GDP), data = azija_podacii)

gdp_test <- 20000
pred_eu <- predict(model_eu, newdata = data.frame(GDP = gdp_test))
pred_azija <- predict(model_azija, newdata = data.frame(GDP = gdp_test))

cat("EU predikcija za GDP =", gdp_test, ":", round(pred_eu, 2), "godina\n")
cat("Azija predikcija za GDP =", gdp_test, ":", round(pred_azija, 2), "godina\n")
