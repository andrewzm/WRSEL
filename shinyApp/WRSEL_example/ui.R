library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("WRSEL 3-variable toy study"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("c1",
                  "Weight on first rank",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("c2",
                  "Weight on second rank",
                  min = 0,
                  max = 1,
                  value = 0.5),
      sliderInput("c3",
                  "Weight on third rank",
                  min = 0,
                  max = 1,
                  value = 0.5)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))