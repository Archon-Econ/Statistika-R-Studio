library(shiny)
library(WDI)
library(dplyr)

# Preuzmi podatke i pripremi model van server funkcije (da se ne radi svaki put)
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

model_log <- lm(broj_GODINA ~ log10(GDP), data = eu_podacii)

# UI
ui <- fluidPage(
  titlePanel("Predikcija životnog vijeka na osnovu GDP per capita (EU zemlje 2021)"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("gdp_input", "Unesi GDP per capita (USD):", value = 30000, min = 1000, max = 200000),
      actionButton("predict_btn", "Predvidi životni vijek")
    ),
    mainPanel(
      verbatimTextOutput("prediction_text"),
      plotOutput("plot_gdp_life")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Reakcija na klik dugmeta za predikciju
  pred <- eventReactive(input$predict_btn, {
    req(input$gdp_input)
    if (input$gdp_input <= 0 || is.na(input$gdp_input)) {
      return("Unijeli ste nevažeću vrijednost GDP-a. Molimo unesite pozitivan broj.")
    }
    predikcija <- predict(model_log, newdata = data.frame(GDP = input$gdp_input))
    paste0("Očekivani životni vijek za GDP od ", input$gdp_input, " USD je: ", round(predikcija, 2), " godina.")
  })
  
  output$prediction_text <- renderText({
    pred()
  })
  
  # Prikaz scatter plot i regresione linije
  output$plot_gdp_life <- renderPlot({
    ggplot(eu_podacii, aes(x = GDP, y = broj_GODINA)) +
      geom_point(color = "blue", size = 3, alpha = 0.6) +
      geom_smooth(method = "lm", formula = y ~ log10(x), color = "red", se = FALSE) +
      geom_point(aes(x = input$gdp_input, y = predict(model_log, newdata = data.frame(GDP = input$gdp_input))),
                 color = "green", size = 4) +
      scale_x_log10() +
      labs(
        title = "Životni vijek u odnosu na GDP per capita (log skala)",
        x = "GDP per capita (log10 skala)",
        y = "Očekivani životni vijek (godine)"
      ) +
      theme_minimal()
  })
}

shinyApp(ui, server)
