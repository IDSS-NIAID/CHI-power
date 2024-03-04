# simple power exploration app

# build shiny app with shinylive (within the root directory)
# shinylive::export('docs/power', 'docs/power/site')

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
    plotOutput('cell_death_rates'),
    plotOutput('power')
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
  output$power <- renderPlot({
    # critical values
    delta <- input$p[2] - input$p[1]
    se <- sqrt(sigma2_beta(params_e()[1], params_e()[2]) / input$n +
               sigma2_beta(params_u()[1], params_u()[2]) / input$n)
    crit <- qnorm(1 - input$typeI_error / 2, sd = se)
    mode <- dnorm(0, sd = se)
    power <- pnorm(-crit, delta, se) + pnorm(crit, delta, se, lower.tail = FALSE)

    # colors (one pair with transparency)
    u_color <- '#377EB8'
    u_color_trans <- rgb(55, 126, 184, maxColorValue = 255, alpha = 128)
    e_color <- '#E41A1C'
    e_color_trans <- rgb(228, 26, 28, maxColorValue = 255, alpha = 128)

    # get plot limits
    xlims <- c(qnorm(0.001, mean =    0, sd = se),
               qnorm(0.001, mean = delta, sd = se),
               qnorm(0.999, mean =    0, sd = se),
               qnorm(0.999, mean = delta, sd = se)) |>
      range()


    # curves for null and alternate distributions
    curves <- data.frame(x = seq(xlims[1], xlims[2], length.out = 200))
    curves$cntl <- dnorm(curves$x, mean = 0, sd = se)
    curves$case <- dnorm(curves$x, mean = delta, sd = se)

    # curves for tails of each distribution
    tails  <- data.frame(x_left  = c(xlims[1], seq(xlims[1],   -crit, length.out = 200),   -crit),
                         x_right = c(   crit, seq(   crit, xlims[2], length.out = 200), xlims[2]))

    tails$null_left  <- c(0, dnorm(tails$x_left[ -c(1,202)], mean = 0, sd = se), 0)
    tails$null_right <- c(0, dnorm(tails$x_right[-c(1,202)], mean = 0, sd = se), 0)

    tails$alt_left  <- c(0, dnorm(tails$x_left[ -c(1,202)], mean = delta, sd = se), 0)
    tails$alt_right <- c(0, dnorm(tails$x_right[-c(1,202)], mean = delta, sd = se), 0)

    # plot!! :)
    plot(curves$x, curves$cntl, type = 'l', col = u_color, lwd = 2, bty = 'l',
         xlab = 'Statistic', ylab = '', main = 'Distribution of cell death rate statistics (ovserved)',
         ylim = c(0, 1.1*mode))
    lines(curves$x, curves$case, col = e_color, lwd = 2)

    polygon(tails$x_left,  tails$null_left,  col = u_color_trans, border = NA)
    polygon(tails$x_right, tails$null_right, col = u_color_trans, border = NA)

    polygon(tails$x_left,  tails$alt_left,   col = e_color_trans, border = NA)
    polygon(tails$x_right, tails$alt_right,  col = e_color_trans, border = NA)

    # add critical values
    abline(v = -crit, col = 'black', lty = 2)
    abline(v =  crit, col = 'black', lty = 2)

    # add labels
    reject_text_y <- 1.05*mode # y-value for placement of reject / fail to reject text and arrows

    text(-crit - 0.03*diff(xlims), reject_text_y, 'Reject Ho', pos = 2)
    arrows(-crit, reject_text_y, -crit - 0.03*diff(xlims), col = 'black', length = 0.1)

    text( crit + 0.03*diff(xlims), reject_text_y, 'Reject Ho', pos = 4)
    arrows( crit, reject_text_y,  crit + 0.03*diff(xlims), col = 'black', length = 0.1)

    text(0, reject_text_y, 'Fail to reject Ho', pos = 3)
    arrows(0, reject_text_y, -crit + 0.005*diff(xlims), reject_text_y, col = 'black', length = 0.08, angle = 90)
    arrows(0, reject_text_y,  crit - 0.005*diff(xlims), reject_text_y, col = 'black', length = 0.08, angle = 90)

    text(      - 0.5*se, reject_text_y - 0.1*mode,                       'Ho distribution', pos = 2, col = u_color)
    text(delta + 0.5*se, reject_text_y - 0.1*mode, 'Ha distribution\n("Truth / Expected")', pos = 4, col = e_color)

    text(  delta + 1.57*se, 0.55*mode,                  'Red area', pos = 2)
    text(  delta + 1.5 *se, 0.55*mode,                   '= Power', pos = 4)
    text(  delta + 1.5 *se, 0.5 *mode, paste('=', round(power, 2)), pos = 4)
    arrows(delta + 1.5 *se, 0.5 *mode, delta + se, 0.3*mode, length = 0.1)
  })
}

shinyApp(ui = ui, server = server)
