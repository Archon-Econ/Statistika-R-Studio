library(WDI)
library(dplyr)
library(ggplot2)

indicators <- c(GDP = "NY.GDP.PCAP.CD", broj_GODINA = "SP.DYN.LE00.IN")

podacii <- WDI(country = "all", indicator = indicators, start = 2021, end = 2021)

eu_zemlje <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
  "Slovenia", "Spain", "Sweden"
)

eu_podacii <- podacii %>%
  filter(country %in% eu_zemlje) %>%
  select(country, GDP, broj_GODINA) %>%
  na.omit()

korelacija <- round(cor(eu_podacii$GDP, eu_podacii$broj_GODINA), 6)

ggplot(eu_podacii, aes(x = GDP, y = broj_GODINA)) +
  geom_point(color = "blue", size = 2.5) +
  geom_text(aes(label = country), vjust = -0.7, size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  scale_x_log10() +
  labs(
    title = "Dijagram rasprsenosti - Veze izmedju promjenljivih 2021.",
    x = "GDP ",
    y = "Zivotni vijek (godine)"
  ) +
  annotate(
    "text",
    x = quantile(log10(eu_podacii$GDP), 0.3),
    y = min(eu_podacii$broj_GODINA),
    label = paste0("Korelacija = ", korelacija),
    size = 4,
    color = "darkblue"
  ) +
  theme_minimal()
