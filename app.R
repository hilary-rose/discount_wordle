library(shiny)
library(tidyverse)
library(patchwork)

ui <- fluidPage(
  #newgame button at top that sets turn to 1 and chooses target word and makes blank gameboard with alphabet
  
  #creation of new plot list is a reactive function
  plotOutput(outputId = "gameboard"),
  textInput(inputId = "guess", label = "Guess a word:"),
  actionButton(inputId = "enter_guess", label = "Guess!") # parameter to disable, reset by newgame click?
  
)
server <- function(input, output) {
  
  output$gameboard <- renderPlot({
    p1 <- iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
    p2 <- iris %>% ggplot(aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
    p1/p2
    
  })
  observeEvent(input$enter_guess, {
    print(input$guess) #run code for a turn
  })
  
}
shinyApp(ui = ui, server = server)