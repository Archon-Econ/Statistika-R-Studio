library(shiny)          # Za pravljenje web aplikacije
library(ggplot2)        # Za crtanje grafova
library(dplyr)          # Za manipulaciju podacima
library(broom)          # Za formatiranje rezultata regresije

# UI (korisnički interfejs)
ui <- fluidPage(
  titlePanel("Višestruka linearna regresija: STR i PctEL vs TestScore"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("str", "Veličina razreda (STR):", min = 10, max = 30, value = 20),
      sliderInput("pctel", "Procenat učenika engleskog jezika (PctEL):", min = 0, max = 100, value = 20)
    ),
    
    mainPanel(
      plotOutput("scatterPlot"),
      verbatimTextOutput("regSummary"),
      verbatimTextOutput("prediction")
    )
  )
)

# SERVER logika
server <- function(input, output) {
  
  # Simulacija podataka (možeš zamijeniti sa stvarnim)
  set.seed(123)
  n <- 200
  STR <- runif(n, 15, 30)
  PctEL <- runif(n, 0, 60)
  TestScore <- 700 - 1.1 * STR - 0.65 * PctEL + rnorm(n, mean = 0, sd = 10)
  df <- data.frame(TestScore, STR, PctEL)
  
  # Fit modela: višestruka regresija
  model <- lm(TestScore ~ STR + PctEL, data = df)
  
  # Vizualizacija – scatter plot STR vs TestScore (boja = PctEL)
  output$scatterPlot <- renderPlot({
    ggplot(df, aes(x = STR, y = TestScore, color = PctEL)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
      labs(title = "Test Score vs STR & PctEL", x = "Veličina razreda (STR)", y = "Rezultat testa") +
      theme_minimal()
  })
  
  # Tekstualni prikaz regresione tabele
  output$regSummary <- renderPrint({
    summary(model)
  })
  
  # Prikaz predikcije za vrijednosti koje korisnik odabere
  output$prediction <- renderPrint({
    new_data <- data.frame(STR = input$str, PctEL = input$pctel)
    pred <- predict(model, newdata = new_data, interval = "confidence")
    cat("Predviđeni rezultat testa za STR =", input$str,
        "i PctEL =", input$pctel, "je:\n")
    print(pred)
  })
}

# Pokretanje aplikacije
shinyApp(ui = ui, server = server)
