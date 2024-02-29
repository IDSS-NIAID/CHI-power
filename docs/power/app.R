# simple power exploration app

library(shiny)
library(bslib)

ui <- page_sidebar(
  title = 'Power analysis',
  sidebar = sidebar(
    # parameters for the illustration
    title = 'Parameters',
    sliderInput('typeI_error',                     'Significance level',        0.05 , min = 0.001, max = 0.1), # significance level
    sliderInput(          'p', 'Cell death rates (unexposed / exposed)', c(0.2, 0.4 ), min = 0    , max = 1  ), # effect size
    sliderInput('variability',                            'Variability',        0.1  , min = 0    , max = 1  ), # variability
    numericInput(         'n',                            'Sample size',        5    , min = 2               ) # sample size
  ),

  card(
    plotOutput('cell_death_rates')#,
    #plotOutput('power')
  )
)

server <- function(input, output)
{
  calc_ab <- function(mean, variability = NULL, v = NULL)
  {
    if(is.null(v))
      v <- mean * (1 - mean) * variability

    # function to optimize
    fn <- function(x) # x = c(alpha, beta)
    {
      crossprod(c(mu_beta(x[1], x[2]), sigma2_beta(x[1], x[2])) - c(mean, v))
    }

    retval <- optim(c(1, 1), fn)$par
    names(retval) <- c('alpha', 'beta')

    return(retval)
  }

  mu_beta <- function(alpha, beta)
  {
    retval <- alpha / (alpha + beta)
    names(retval) <- 'mean'

    return(retval)
  }

  sigma2_beta <- function(alpha, beta)
  {
    retval <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
    names(retval) <- 'variance'

    return(retval)
  }

  # distribution of cell death rates
  params_u <- reactive({calc_ab(input$p[1], variability = input$variability)})
  params_e <- reactive({calc_ab(input$p[2], variability = input$variability)})

  output$cell_death_rates <- renderPlot(
    {
      curves <- data.frame(x = seq(0.01, 0.99, length.out = 200))
      curves$cntl <- dbeta(curves$x, params_u()[1], params_u()[2])
      curves$case <- dbeta(curves$x, params_e()[1], params_e()[2])

      plot(curves$x, curves$cntl, type = 'l', col = '#377EB8', lwd = 2, bty = 'l',
           xlab = 'Cell death rate', ylab = 'Density', main = 'Distribution of cell death rates (truth / hidden)')

      lines(curves$x, curves$case, col = '#E41A1C', lwd = 2)
    })

  # distribution of cell death rate statistics
  #output$power <- renderPlot({plot_beta_stats(input$p[1], params_u(), input$p[2], params_e(), input$n, input$typeI_error)})
}

shinyApp(ui = ui, server = server)
