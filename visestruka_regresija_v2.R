# Učitavanje potrebnih paketa
library(shiny)       # Za pravljenje interaktivne aplikacije
library(ggplot2)     # Za grafički prikaz
library(dplyr)       # Za manipulaciju podacima
library(broom)       # Za formatiranje rezultata regresije

# Učitavamo ugrađeni skup podataka mtcars
data("mtcars")

# UI dio aplikacije (korisnički interfejs)
ui <- fluidPage(
  titlePanel("Višestruka linearna regresija - mtcars dataset"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Odaberi nezavisne varijable za regresiju (zavisna: mpg)"),
      checkboxGroupInput("predictors", "Nezavisne varijable:",
                         choices = names(mtcars)[!names(mtcars) %in% "mpg"],
                         selected = c("wt", "hp"))
    ),
    
    mainPanel(
      h4("Sažetak modela:"),
      verbatimTextOutput("modelSummary"),
      
      h4("Tabela koeficijenata:"),
      tableOutput("coefTable"),
      
      h4("Rezidualni dijagram:"),
      plotOutput("residPlot")
    )
  )
)

# Server logika
server <- function(input, output) {
  
  # Reaktivni model koji se mijenja na osnovu izbora korisnika
  model <- reactive({
    formula <- as.formula(
      paste("mpg ~", paste(input$predictors, collapse = " + "))
    )
    lm(formula, data = mtcars)  # Primjenjujemo OLS regresiju
  })
  
  # Prikaz sažetka modela (kao što je summary(lm))
  output$modelSummary <- renderPrint({
    summary(model())
  })
  
  # Tabela koeficijenata korištenjem broom paketa (glance + tidy)
  output$coefTable <- renderTable({
    tidy(model(), conf.int = TRUE)  # Prikaz koeficijenata s intervalima povjerenja
  })
  
  # Dijagram reziduala
  output$residPlot <- renderPlot({
    ggplot(data = data.frame(
      fitted = fitted(model()),
      residuals = resid(model())
    ), aes(x = fitted, y = residuals)) +
      geom_point(color = "blue") +
      geom_hline(yintercept = 0, linetype = "dashed") +
      labs(x = "Predviđene vrijednosti", y = "Reziduali") +
      theme_minimal()
  })
}

# Pokretanje aplikacije
shinyApp(ui = ui, server = server)


