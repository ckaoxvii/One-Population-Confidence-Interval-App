library(shiny)
library(bslib)
library(reactable)
library(tidyverse)

ui <- page_sidebar(
  title = "One-Proportion Confidence Interval Calculator",
  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),
  # Add custom CSS to ensure card headers have the correct styling
  tags$head(
    tags$style(HTML("
      .card-header {
        background-color: #A90533 !important;
        color: white !important;
        font-weight: bold;
      }
    "))
  ),
  sidebar = sidebar(
    radioButtons(
      'input_type',
      'Input Type',
      choices = c(
        'Sample Proportion' = 'prop',
        'Number of Successes' = 'count'
      ),
      selected = 'prop'
    ),
    uiOutput('sample_input'),
    numericInput(
      'sample_size',
      "Sample Size:",
      value = 100
    ),
    numericInput(
      'conf_level',
      'Confidence Level (%)',
      value = 95
    )
  ),

  layout_columns(
    col_widths = c(6, 6, 12),
    row_heights = c(1, 2),
    card(
      full_screen = TRUE,
      card_header('Estimate of Population Proportion'),
      reactableOutput('table_1')
    ),
    card(
      full_screen = TRUE,
      card_header('Confidence Interval'),
      reactableOutput('table_2')
    ),
    card(
      full_screen = TRUE,
      card_header('Confidence Interval Plot'),
      plotOutput('ci_curve_plot', height = '400px')
    )
  )
  
  # card(
  #   full_screen = TRUE,
  #   card_header("Results"),
  #   card_body(
  #     h4('Estimate of Population Proportion'),
  #     reactableOutput('table_1'),
  #     br(),
  #     h4('Confidence Interval'),
  #     reactableOutput('table_2'),
  #     br(),
  #     h4('Confidence Interval Plot'),
  #     plotOutput("ci_curve_plot", height = "400px")
  #   )
  # )

  
)

server <- function(input, output, session) {

  output$sample_input <- renderUI({
    if (input$input_type == "prop") {
      numericInput(
        "sample_prop",
        "Sample Proportion:",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.01
      )
    } else {
      numericInput(
        "successes",
        "Number of Successes:",
        value = 50,
        min = 0,
        step = 1
      )
    }
  })

  p_hat <- reactive({
    if (input$input_type == 'prop') {
      req(input$sample_prop)
      validate(
        need(
          input$sample_prop >= 0 && input$sample_prop <= 1,
          'Sample proportion must be between 0 and 1.'
        )
      )
      input$sample_prop
    } else {
      req(input$successes)
      validate(
        need(input$successes >= 0, 'Number of successes must be nonnegative.'),
        need(input$successes <= input$sample_size, 'Number of successes cannot exceed the sample size')
      )
      input$successes/input$sample_size
    }
  })
  
  # calculate standard error
  standard_error <- reactive({
    req(p_hat(), input$sample_size)
    sqrt(p_hat() * (1 - p_hat()) / input$sample_size)
  })

  # calculate margin of error
  margin_of_error <- reactive({
    req(input$conf_level)
    alpha = 1 - input$conf_level / 100
    z_critical = qnorm(1 - alpha / 2)
    z_critical * standard_error()
  })

  # calculate CI lower bound
  lower_bound <- reactive({
    max(0, p_hat() - margin_of_error())
  })

  # calculate CI upper bound
  upper_bound <- reactive({
    min(1, p_hat() + margin_of_error())
  })
  
  # calculate confidence interval
  confidence_interval <- reactive({
    c(lower_bound(), upper_bound())
  })
  
  output$table_1 <- renderReactable({
    reactable(
      tibble(
        point_estimate = sprintf('%.4f', p_hat()),
        standard_error = sprintf('%.4f', standard_error()),
        margin_of_error = sprintf('%.4f', margin_of_error())
      ),
      defaultColDef = colDef(
        align = 'right'
      ),
      columns = list(
        point_estimate = colDef(name = 'Point Estimate'),
        standard_error = colDef(name = 'Standard Error'),
        margin_of_error = colDef(name = 'Margin of Error')
      )
    )
  })

  output$table_2 <- renderReactable({
    reactable(
      tibble(
        conf_level = paste0(input$conf_level, '%'),
        lower_bd = sprintf('%.4f', lower_bound()),
        upper_bd = sprintf('%.4f', upper_bound()),
      ),
      defaultColDef = colDef(
        align = 'right'
      ),
      columns = list(
        conf_level = colDef(name = 'Confidence Level'),
        lower_bd = colDef(name = 'Lower Bound'),
        upper_bd = colDef(name = 'Upper Bound')
      )
    )
  })

   output$ci_curve_plot <- renderPlot({
    req(p_hat(), standard_error(), lower_bound(), upper_bound(), input$conf_level)

    mu <- p_hat()
    se <- standard_error()
    lb <- lower_bound()
    ub <- upper_bound()

    x_min <- max(0, mu - 4 * se)
    x_max <- min(1, mu + 4 * se)

    curve_df <- tibble(
      x = seq(x_min, x_max, length.out = 1000),
      y = dnorm(x, mean = mu, sd = se)
    )

    shade_df <- curve_df |> filter(x >= lb, x <= ub)

    y_peak <- max(curve_df$y)

    ggplot(curve_df, aes(x = x, y = y)) +
      geom_area(
        data = shade_df,
        fill = "#F04E2A",
        alpha = 0.25
      ) +
      geom_line(
        linewidth = 1.2,
        color = "#A90533"
      ) +
      geom_segment(
        aes(x = lb, xend = lb, y = 0, yend = y_peak * 0.95),
        linetype = "dashed",
        color = "#F04E2A",
      linewidth = 0.9
      ) +
      geom_segment(
        aes(x = ub, xend = ub, y = 0, yend = y_peak * 0.95),
        linetype = "dashed",
        color = "#F04E2A",
        linewidth = 0.9
      ) +
      annotate(
        "text",
        x = mu,
        y = y_peak * 1.25,
        label = paste0(input$conf_level, "% Confidence Interval"),
        size = 5
      ) +
      annotate(
        "text",
        x = mu,
        y = y_peak * 1.1,
        label = paste0(
          "[",
          sprintf("%.4f", lb),
          ", ",
          sprintf("%.4f", ub),
          "]"
        ),
        color = "#A90533",
        size = 5
      ) +
      annotate(
        "text",
        x = lb,
        y = 0.5 * y_peak,
        label = paste0("Lower\n", sprintf("%.4f", lb)),
        color = "#F04E2A",
        vjust = 1,
        hjust = 1.25,
        size = 5
      ) +
      annotate(
        "text",
        x = mu,
        y = -0.02 * y_peak,
        label = paste0("p̂\n", sprintf("%.4f", mu)),
        color = "#A90533",
        vjust = 1,
        size = 5
      ) +
      annotate(
        "text",
        x = ub,
        y = 0.5 * y_peak,
        label = paste0("Upper\n", sprintf("%.4f", ub)),
        color = "#F04E2A",
        vjust = 1,
        hjust = -0.25,
        size = 5
      ) +
      scale_x_continuous(
        limits = c(0, 1),
        breaks = seq(0, 1, by = 0.1)
      ) +
      labs(
        x = "Population Proportion p"
      ) +
      coord_cartesian(ylim = c(-0.08 * y_peak, 1.5 * y_peak)) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank()
      )
  })

  # output interpretation
  output$interpretation <- renderText({
    ci <- confidence_interval()
    paste0(
      "We are ", input$conf_level,
      "% confident that the true population proportion lies between ",
      round(ci[1] * 100, 2), "% and ",
      round(ci[2] * 100, 2), "%."
    )
  })

}

shinyApp(ui = ui, server = server)