library(shiny)

ui <- fluidPage(
  titlePanel("Test SSP Cloud"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Nombre de points", min = 10, max = 1000, value = 200)
    ),
    mainPanel(
      plotOutput("hist"),
      textOutput("info")
    )
  )
)

server <- function(input, output, session) {
  output$hist <- renderPlot({
    hist(rnorm(input$n), col = "steelblue", border = "white",
         main = "Loi normale", xlab = "")
  })
  output$info <- renderText({
    paste("R", R.version.string, "|", Sys.info()[["sysname"]], "|", Sys.time())
  })
}

shinyApp(ui, server)