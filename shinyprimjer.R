library(shiny)

ui <- fluidPage(
  titlePanel("Cao, Ekonomski fakultete!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("broj", "Odaberi broj:", 1, 100, 50)
    ),
    mainPanel(
      textOutput("rezultat")
    )
  )
)

server <- function(input, output) {
  output$rezultat <- renderText({
    paste("Izabrao si broj:", input$broj)
  })
}

shinyApp(ui = ui, server = server)
