library(shiny)
library(Matrix)
library(reshape2)
library(ggplot2)
library(WRSEL)


### Say we have three independent random variables, what is the WRSEL?
n <- 3
N <- 10000
X <- matrix(rnorm(n*N),n,N)
X <- X + c(-1,0,1)
df <- reshape2::melt(data.frame(t(X)))
WRSEL_fun <- WRSEL(X = X,num_nodes = NULL)

weights <- c(1,0,0)
lambda_hat <- WRSEL_fun(weights)
df2 <- melt(data.frame(t(lambda_hat)))
ggplot(df) + geom_density(aes(x=value,fill=variable),alpha=0.3) + 
  geom_vline(data=data.frame(df2),aes(xintercept=value,colour = variable))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    weights <- c(input$c1,input$c2,input$c3)
      lambda_hat <- WRSEL_fun(weights)
      df2 <- melt(data.frame(t(lambda_hat)))
      ggplot(df) + geom_density(aes(x=value,fill=variable),alpha=0.3) + 
        geom_vline(data=data.frame(df2),aes(xintercept=value,colour = variable))
  })
})                                                