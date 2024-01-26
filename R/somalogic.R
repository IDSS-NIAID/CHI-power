# somalogic.R
# UI and server elements for SomaLogic tab/widget

#' somalogic_sidebarUI
#' UI element for the SomaLogic sidebar
#'
#' @name somalogic
#'
#' @param id Shiny namespace ID
#'
#' @return A modularized tagList
#' @export
#' @importFrom htmltools HTML
#' @importFrom shiny NS numericInput sliderInput tagList
somalogic_sidebarUI <- function(id)
{
  ns <- NS(id)

  tagList(
    numericInput(ns('delta0'), 'Min Effect Size',
                 value = 1,
                 min = 0),

    numericInput(ns('delta1'), 'Max Effect Size',
                 value = 1,
                 min = 0),

    numericInput(ns('sd'), 'SD',
                 value = 1,
                 min = 0.001),

    numericInput(ns('alpha'), HTML('Family-wise Type I Error (&alpha;)'),
                 value = 0.05,
                 min = 0,
                 max = 0.5),

    numericInput(ns('power'), 'Power',
                 value = 0.8,
                 min = 0,
                 max = 1),

    sliderInput(ns('n0'), 'Samples per group',
                value = c(3, 50),
                min = 3,
                max = 500,
                step = 1),

    # numericInput(ns('n1'), '#Cases',
    #              value = c(3, 50),
    #              min = 3,
    #              max = 500,
    #              step = 1),

    sliderInput(ns('analytes'), 'Number of Analytes',
                value = 7288, # number of analytes in the 7K panel
                min = 1,
                max = 10776, # number of analytes in the 11K panel
                step = 1)
  )
}


#' somalogic_cardsUI
#' UI element for the SomaLogic cards
#'
#' @rdname somalogic
#'
#' @return A modularized tagList of cards
#' @export
#'
#' @importFrom bslib card card_header card_body card_footer
#' @importFrom shiny downloadButton NS plotOutput tagList
somalogic_cardsUI <- function(id)
{
  ns <- NS(id)

  tagList(
    card(full_screen = TRUE,
         card_header("Power Curves"),
         card_body(plotOutput(ns("power_curves"))),
         card_footer(downloadButton(ns('power_curves_download'), 'Download figure')))
    # perhaps a visualization of Type I and II errors along with Power
  )
}


#' somalogic_server
#' Server element for SomaLogic tab
#'
#' @rdname somalogic
#'
#' @export
#' @importFrom cowplot theme_cowplot
#' @importFrom dplyr mutate tibble %>%
#' @importFrom ggplot2 aes ggplot ggsave geom_hline geom_line labs scale_color_brewer
#' @importFrom shiny downloadHandler moduleServer reactive reactiveValues reactiveValuesToList renderPlot
#' @importFrom stats power.t.test
somalogic_server <- function(id)
{
  # deal with pesky no visible binding for global variable warnings
  if(FALSE)
    delta <- n <- power <- NULL

  moduleServer(id, function(input, output, session) {

    # reactive values
    delta0 <- reactive({ input$delta0 })
    delta1 <- reactive({ input$delta1 })
    sd <- reactive({ input$sd })
    alpha <- reactive({ input$alpha })
    power <- reactive({ input$power })
    n0 <- reactive({ input$n0 })
    analytes <- reactive({ input$analytes })
    # n1 = reactive({ input$n1 })

    ncurves <- reactive({ifelse(delta0() == delta1(), 1, 3)})

    # power curves
    output$power_curves <- renderPlot({
      tibble(n = rep(seq(n0()[1], n0()[2], 1), ncurves()),
             delta = rep(seq(delta0(), delta1(), length.out = ncurves()), each = length(n) / ncurves()),
             power = power.t.test(n,
                                  delta,
                                  sd(),
                                  alpha() / analytes())$power) %>%

        mutate(delta = as.factor(delta)) %>%

        ggplot(aes(n, power, color = delta, linetype = delta)) +
        geom_line() +
        geom_hline(yintercept = power(), linetype = 'dashed') +

        labs(x = 'Sample Size', y = 'Power') +
        theme_cowplot() +
        scale_color_brewer(palette = 'Set1')
    })

    # download power curves
    output$power_curves_download <- downloadHandler(
      filename = function() {
        'power_curves.png'
      },
      content = function(file) {
        ggsave(file, width = 8, height = 6, dpi = 300)
      }
    )
  })
}
