library(shiny)
library(WDI)
library(dplyr)
library(ggplot2)

# Preuzmi podatke i pripremi ih (ovo možeš izvan servera, da se ne ponavlja svaki put)
indicators <- c(GDP = "NY.GDP.PCAP.CD", broj_GODINA = "SP.DYN.LE00.IN")
podacii <- WDI(country = "all", indicator = indicators, start = 2021, end = 2021)

eu_zemlje <- c(
  "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czech Republic",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Hungary",
  "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Poland", "Portugal", "Romania", "Slovak Republic",
  "Slovenia", "Spain", "Sweden"
)

azija_zemlje <- c(
  "Afghanistan", "Armenia", "Azerbaijan", "Bahrain", "Bangladesh", "Bhutan",
  "Brunei Darussalam", "Cambodia", "China", "Cyprus", "Georgia", "India", "Indonesia",
  "Iran", "Iraq", "Israel", "Japan", "Jordan", "Kazakhstan", "Kuwait", "Kyrgyzstan", 
  "Lao People's Democratic Republic", "Lebanon", "Malaysia", "Maldives", "Mongolia", 
  "Myanmar", "Nepal", "Oman", "Pakistan", "Palestine", "Philippines", "Qatar", "Republic of Korea", 
  "Sri Lanka", "Syrian Arab Republic", "Tajikistan", "Thailand", "Timor-Leste", "Turkey", 
  "Turkmenistan", "United Arab Emirates", "Uzbekistan", "Viet Nam", "Yemen"
)

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

kombinovano <- bind_rows(eu_podacii, azija_podacii)

# Fit regresione modele
model_eu <- lm(broj_GODINA ~ log10(GDP), data = eu_podacii)
model_azija <- lm(broj_GODINA ~ log10(GDP), data = azija_podacii)


# UI
ui <- fluidPage(
  titlePanel("Predikcija životnog vijeka na osnovu GDP-a"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("regija", "Izaberi regiju:", choices = c("EU", "Asia")),
      numericInput("gdp_input", "Unesi GDP per capita (USD):", value = 30000, min = 1000, step = 100),
      actionButton("predikcija_btn", "Izračunaj predikciju"),
      br(),
      br(),
      verbatimTextOutput("rezultat_predikcije")
    ),
    
    mainPanel(
      plotOutput("plot_graf")
    )
  )
)


# Server
server <- function(input, output, session) {
  
  predikcija <- eventReactive(input$predikcija_btn, {
    gdp <- input$gdp_input
    if (is.na(gdp) || gdp <= 0) {
      return("Molimo unesite validan, pozitivan broj za GDP.")
    }
    
    # Izaberi model prema regiji
    model <- if (input$regija == "EU") model_eu else model_azija
    
    # Napravi predikciju (log10 GDP)
    pred <- predict(model, newdata = data.frame(GDP = gdp))
    paste("Očekivani životni vijek za GDP od", gdp, "USD u regiji", input$regija, "je:", round(pred, 2), "godina.")
  })
  
  output$rezultat_predikcije <- renderText({
    predikcija()
  })
  
  output$plot_graf <- renderPlot({
    ggplot(kombinovano, aes(x = GDP, y = broj_GODINA, color = kontinent)) +
      geom_point(size = 3, alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, aes(group = kontinent), linetype = "dashed") +
      scale_x_log10() +
      labs(
        title = "GDP vs Životni vijek: EU vs Azija (2021)",
        x = "GDP per Capita (log skala)",
        y = "Životni vijek (godine)",
        color = "Regija"
      ) +
      theme_minimal() +
      # Dodaj tačku korisničke predikcije na graf
      {
        gdp <- input$gdp_input
        if (!is.na(gdp) && gdp > 0) {
          pred_val <- if (input$regija == "EU") {
            predict(model_eu, newdata = data.frame(GDP = gdp))
          } else {
            predict(model_azija, newdata = data.frame(GDP = gdp))
          }
          geom_point(aes(x = gdp, y = pred_val), color = "black", size = 5, shape = 17)
        }
      }
  })
  
}

# Run the app
shinyApp(ui, server)
