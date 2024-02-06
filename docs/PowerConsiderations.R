# functions used in PowerConsiderations.qmd

#' mu_beta
#' mu of beta distribution as a function of alpha and beta
#'
#' @param alpha The alpha parameter of the beta distribution
#' @param beta The beta parameter of the beta distribution
#'
#' @return The mean of the beta distribution
mu_beta <- function(alpha, beta)
{
  retval <- alpha / (alpha + beta)
  names(retval) <- 'mean'

  return(retval)
}

#' sigma2_beta
#' sigma^2 as a function of alpha and beta
#'
#' @param alpha The alpha parameter of the beta distribution
#' @param beta The beta parameter of the beta distribution
#'
#' @return The variance of the beta distribution
sigma2_beta <- function(alpha, beta)
{
  retval <- alpha * beta / ((alpha + beta)^2 * (alpha + beta + 1))
  names(retval) <- 'variance'

  return(retval)
}

#' calc_ab
#' Calculate alpha and beta given mean and variability measure in (0,1)
#'
#' @param mean The mean of the desired beta distribution
#' @param variability The variability measure of the desired beta distribution. The input is expected to be in (0,1) and will be mapped to a variance in (0, mean * (1 - mean)), which is the range of the variance of a beta distribution.
#' @param v The variance of the desired beta distribution. If this is provided, the variability parameter is ignored.
#'
#' @return A vector of length 2 containing alpha and beta
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

#' plot_beta_stats
#' Plot sample statistic distributions for beta distribution
#'
#' @param p_u The probability of success for the control (unexposed) group
#' @param params_u The shape parameters of the distribution of observations in the control (unexposed) group
#' @param p_e The probability of success for the experimental (exposed) group
#' @param params_e The shape parameters of the distribution of observations in the experimental (exposed) group
#' @param n The number of samples in each group
#' @param typeI_error The type I error rate
#'
#' @return A ggplot object
plot_beta_stats <- function(p_u, params_u, p_e, params_e, n, typeI_error)
{
  diff <- p_e - p_u
  se <- sqrt(sigma2_beta(params_e[1], params_e[2]) / n +
               sigma2_beta(params_u[1], params_u[2]) / n)
  crit <- qnorm(1 - typeI_error / 2, sd = se)
  mode <- dnorm(0, sd = se)
  power <- pnorm(crit, diff, se, lower.tail = FALSE) + pnorm(-crit, diff, se)

  u_color <- brewer.pal(3, 'Set1')[2]
  e_color <- brewer.pal(3, 'Set1')[1]

  lims <- c(qnorm(0.001, mean =    0, sd = se),
            qnorm(0.001, mean = diff, sd = se),
            qnorm(0.999, mean =    0, sd = se),
            qnorm(0.999, mean = diff, sd = se)) |>
    range()

  data.frame(x = lims) |>

    ggplot(aes(x)) +

    # distributions
    stat_function(fun = dnorm,
                  args = list(mean = 0,
                              sd = se),
                  color = u_color) +
    stat_function(fun = dnorm,
                  args = list(mean = diff,
                              sd = se),
                  color = e_color) +

    # Type I error under null hypothesis
    stat_function(fun = dnorm,
                  args = list(mean = 0,
                              sd = se),
                  xlim = c(lims[1], -crit),
                  fill = u_color,
                  alpha = 0.5,
                  geom = 'area') +
    stat_function(fun = dnorm,
                  args = list(mean = 0,
                              sd = se),
                  xlim = c(crit, lims[2]),
                  fill = u_color,
                  alpha = 0.5,
                  geom = 'area') +

    # Power under alternative hypothesis
    stat_function(fun = dnorm,
                  args = list(mean = diff,
                              sd = se),
                  xlim = c(lims[1], -crit),
                  fill = e_color,
                  alpha = 0.5,
                  geom = 'area') +
    stat_function(fun = dnorm,
                  args = list(mean = diff,
                              sd = se),
                  xlim = c(crit, lims[2]),
                  fill = e_color,
                  alpha = 0.5,
                  geom = 'area') +

    # add labels
    geom_errorbar(data = data.frame(x = 0, y = 1.05*mode, xmin = -crit, xmax = crit),
                  aes(x = x, y = y, xmin = xmin, xmax = xmax),
                  color = 'black', width = 0.1) +
    geom_segment(data = tibble(x = c(-crit, crit),
                               y = 1.05*mode,
                               xend = x + diff(lims)*c(-1, 1)*.03,
                               yend = 1.05*mode),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.3, 'cm'))) +
    geom_vline(xintercept = -crit, linetype = 'dashed') +
    geom_vline(xintercept = crit, linetype = 'dashed') +
    annotate(geom = 'label', x = 0, y = 1.05*mode, label = 'Fail to reject Ho', fill = 'white', label.size = NA) +
    annotate(geom = 'label', x =  crit + diff(lims)*.03, y = 1.05*mode, label = 'Reject Ho', hjust = 0, label.size = NA) +
    annotate(geom = 'label', x = -crit - diff(lims)*.03, y = 1.05*mode, label = 'Reject Ho', hjust = 1, label.size = NA) +

    annotate(geom = 'label', x =       -se/2, y = 0.95*mode, label =      'Null distribution', color = u_color, label.size = NA, hjust = 1) +
    annotate(geom = 'label', x = diff + se/2, y = 0.95*mode, label = 'Alternate distribution\n("Truth / Expected")', color = e_color, label.size = NA, hjust = 0) +

    geom_segment(data = tibble(x = lims[2] - diff(lims)*0.1, y = 0.5*mode, xend = lims[2] - diff(lims)*.2, yend = 0.3*mode),
                 aes(x = x, y = y, xend = xend, yend = yend),
                 arrow = arrow(length = unit(0.3, 'cm'))) +
    annotate(geom = 'label', x = lims[2] - .1*diff(lims), y = 0.5*mode, label = 'Red area', color = e_color, label.size = NA, hjust = 1) +
    annotate(geom = 'label', x = lims[2] - .1*diff(lims), y = 0.523*mode, label = paste('= Power\n=', round(power, 2)), color = e_color, label.size = NA, hjust = 0, vjust = 1) +

    labs(y = '', x = 'Difference in cell death rates', title = 'Distribution of difference of cell death rates (observed)')
}
