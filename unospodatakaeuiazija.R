library(WDI)
library(dplyr)

# Definiši indikatore: BDP po glavi i očekivani životni vijek
indikatori <- c(BDP_po_glavi = "NY.GDP.PCAP.CD", zivotni_vijek = "SP.DYN.LE00.IN")

# Preuzmi podatke za 2021. godinu
podaci <- WDI(country = "all", indicator = indikatori, start = 2021, end = 2021)

# Popis zemalja članica Evropske unije
eu_zemlje <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
  "Slovenia", "Spain", "Sweden"
)

# Filtriraj podatke samo za EU zemlje i ukloni nedostajuće vrijednosti
eu_podaci <- podaci %>%
  filter(country %in% eu_zemlje) %>%
  select(drzava = country, BDP_po_glavi, zivotni_vijek) %>%
  na.omit()

# Kreiraj regresioni model sa logaritam transformacijom BDP-a
model_log <- lm(zivotni_vijek ~ log10(BDP_po_glavi), data = eu_podaci)

# Unos vrijednosti BDP-a od strane korisnika
unos <- as.numeric(readline(prompt = "Unesi BDP po glavi stanovnika (u USD): "))

# Provjera unosa i predikcija
if (!is.na(unos) && unos > 1) {
  predikcija <- predict(model_log, newdata = data.frame(BDP_po_glavi = unos))
  cat("Očekivani životni vijek za BDP od", unos, "USD je:", round(predikcija, 2), "godina.\n")
} else {
  cat("Unio si nevažeću vrijednost. Pokušaj ponovo s pozitivnim brojem.\n")
}
