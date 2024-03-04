library(shiny)
library(bslib)

library(dplyr)

library(ggplot2)
library(RColorBrewer)
library(cowplot)
theme_set(theme_cowplot())

source('../PowerConsiderations.R')

# build shiny app with shinylive (within the root directory)
# shinylive::export('docs/multiple-comparisons', 'docs/multiple-comparisons/site')


ui <- page_sidebar(
  title = 'Multiple testing',
  sidebar = sidebar(
    # parameters for the illustration
    sliderInput('typeI_error',               'FW-α',        0.05, min = 0.001, max =    0.1             ), # significance level
    sliderInput('effect_size',        'Effect size', c(1.5, 4  ), min = 1    , max =    5   , step = 0.5), # effect size
    sliderInput(         'sd',                 'SD',        1.5 , min = 0.1  , max =    3               ), # variability
    sliderInput(    'n_range',  'Sample size range', c(3, 400  ), min = 3    , max =  500               ), # sample size
    sliderInput( 'n_analytes', 'Number of analytes',     7267   , min = 1    , max = 7267               ), # number of analytes
    sliderInput(      'power',              'Power',        0.8 , min = 0.5  , max =    1               )  # line for power
  ),

  card(
    plotOutput('power_multiple')
  )
)

server <- function(input, output)
{
  delta       <- reactive({input$effect_size})
  sd          <- reactive({input$sd         })
  alpha       <- reactive({input$typeI_error})
  power       <- reactive({input$power      })
  n_range     <- reactive({input$n_range    })
  n_analytes  <- reactive({input$n_analytes })

  ncurves <- reactive({ifelse(delta()[1] == delta()[2], 1, 3)})

  output$power_multiple <- renderPlot({
    tibble(n     = rep(seq(   n_range()[1] ,    n_range()[2] , 1), ncurves()),
           delta = rep(seq(log2(delta()[1]), log2(delta()[2]), length.out = ncurves()), each = length(n) / ncurves()),
           power = power.t.test(n,
                                delta,
                                sd(),
                                alpha() / n_analytes())$power) %>%

      mutate(delta = as.factor(round(2^delta, 1))) %>%

      ggplot(aes(n, power, color = delta, linetype = delta)) +
      geom_line() +
      geom_hline(yintercept = power(), linetype = 'dashed') +

      labs(x = 'Sample Size', y = 'Power') +
      theme_cowplot() +
      scale_color_brewer(palette = 'Set1')
  })
}

shinyApp(ui = ui, server = server)
