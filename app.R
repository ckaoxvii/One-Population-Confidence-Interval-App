library(shiny)
library(bslib)
library(reactable)

ui <- page_sidebar(
  title = "One-Proportion Confidence Interval Calculator",
  window_title = "One-Proportion CI Calculator",

  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),

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
      "input_type",
      "Input Type",
      choices = c(
        "Sample Proportion" = "prop",
        "Number of Successes" = "count",
        "Upload CSV" = "csv"
      ),
      selected = "prop"
    ),

    conditionalPanel(
      condition = "input.input_type == 'csv'",
      fileInput(
        "csv_file",
        "Upload CSV File",
        accept = c(".csv")
      ),
      uiOutput("categorical_var_ui"),
      uiOutput("success_level_ui")
    ),

    conditionalPanel(
      condition = "input.input_type != 'csv'",
      uiOutput("sample_input"),
      numericInput(
        "sample_size",
        "Sample Size:",
        value = 100,
        min = 1,
        step = 1
      )
    ),

    numericInput(
      "conf_level",
      "Confidence Level (%)",
      value = 95,
      min = 1,
      max = 99.9,
      step = 1
    )
  ),

  layout_columns(
    col_widths = c(6, 6, 12),

    card(
      full_screen = TRUE,
      card_header("Estimate of Population Proportion"),
      reactableOutput("table_1")
    ),

    card(
      full_screen = TRUE,
      card_header("Confidence Interval"),
      reactableOutput("table_2")
    ),

    card(
      full_screen = TRUE,
      card_header("Confidence Interval Plot"),
      plotOutput("ci_curve_plot", height = "400px")
    )
  )
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

  uploaded_data <- reactive({
    req(input$csv_file)

    read.csv(
      input$csv_file$datapath,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  })

  categorical_vars <- reactive({
    df <- uploaded_data()

    names(df)[sapply(df, function(x) {
      is.character(x) || is.factor(x) || is.logical(x)
    })]
  })

  output$categorical_var_ui <- renderUI({
    req(uploaded_data())

    vars <- categorical_vars()

    validate(
      need(
        length(vars) > 0,
        "The uploaded CSV must contain at least one categorical variable."
      )
    )

    selectInput(
      "cat_var",
      "Choose Categorical Variable",
      choices = vars,
      selected = vars[1]
    )
  })

  output$success_level_ui <- renderUI({
    req(uploaded_data(), input$cat_var)

    values <- uploaded_data()[[input$cat_var]]
    values <- values[!is.na(values)]

    levels_available <- sort(unique(as.character(values)))

    validate(
      need(
        length(levels_available) > 0,
        "The selected categorical variable has no non-missing values."
      )
    )

    selectInput(
      "success_level",
      "Choose Success Category",
      choices = levels_available,
      selected = levels_available[1]
    )
  })

  sample_size_calc <- reactive({
    if (input$input_type == "csv") {
      req(uploaded_data(), input$cat_var)

      values <- uploaded_data()[[input$cat_var]]
      sum(!is.na(values))

    } else {
      req(input$sample_size)

      validate(
        need(input$sample_size > 0, "Sample size must be greater than 0.")
      )

      input$sample_size
    }
  })

  successes_calc <- reactive({
    if (input$input_type == "csv") {
      req(uploaded_data(), input$cat_var, input$success_level)

      values <- uploaded_data()[[input$cat_var]]
      values <- values[!is.na(values)]

      sum(as.character(values) == as.character(input$success_level))

    } else if (input$input_type == "count") {
      req(input$successes)

      validate(
        need(input$successes >= 0, "Number of successes must be nonnegative."),
        need(
          input$successes <= sample_size_calc(),
          "Number of successes cannot exceed the sample size."
        )
      )

      input$successes

    } else {
      req(input$sample_prop)

      validate(
        need(
          input$sample_prop >= 0 && input$sample_prop <= 1,
          "Sample proportion must be between 0 and 1."
        )
      )

      input$sample_prop * sample_size_calc()
    }
  })

  p_hat <- reactive({
    successes_calc() / sample_size_calc()
  })

  standard_error <- reactive({
    sqrt(p_hat() * (1 - p_hat()) / sample_size_calc())
  })

  margin_of_error <- reactive({
    req(input$conf_level)

    validate(
      need(
        input$conf_level > 0 && input$conf_level < 100,
        "Confidence level must be between 0 and 100."
      )
    )

    alpha <- 1 - input$conf_level / 100
    z_critical <- qnorm(1 - alpha / 2)

    z_critical * standard_error()
  })

  lower_bound <- reactive({
    max(0, p_hat() - margin_of_error())
  })

  upper_bound <- reactive({
    min(1, p_hat() + margin_of_error())
  })

  output$table_1 <- renderReactable({
    reactable(
      data.frame(
        Sample_Size = sample_size_calc(),
        Successes = round(successes_calc(), 0),
        Point_Estimate = sprintf("%.4f", p_hat()),
        Standard_Error = sprintf("%.4f", standard_error()),
        Margin_of_Error = sprintf("%.4f", margin_of_error()),
        check.names = FALSE
      ),
      defaultColDef = colDef(align = "right"),
      columns = list(
        Sample_Size = colDef(name = "Sample Size"),
        Successes = colDef(name = "Successes"),
        Point_Estimate = colDef(name = "Point Estimate"),
        Standard_Error = colDef(name = "Standard Error"),
        Margin_of_Error = colDef(name = "Margin of Error")
      )
    )
  })

  output$table_2 <- renderReactable({
    reactable(
      data.frame(
        Confidence_Level = paste0(input$conf_level, "%"),
        Lower_Bound = sprintf("%.4f", lower_bound()),
        Upper_Bound = sprintf("%.4f", upper_bound()),
        check.names = FALSE
      ),
      defaultColDef = colDef(align = "right"),
      columns = list(
        Confidence_Level = colDef(name = "Confidence Level"),
        Lower_Bound = colDef(name = "Lower Bound"),
        Upper_Bound = colDef(name = "Upper Bound")
      )
    )
  })

  output$ci_curve_plot <- renderPlot({
    req(p_hat(), standard_error(), lower_bound(), upper_bound())

    mu <- p_hat()
    se <- standard_error()
    lb <- lower_bound()
    ub <- upper_bound()

    validate(
      need(
        se > 0,
        "The standard error is 0, so the confidence interval plot cannot be drawn."
      )
    )

    x_min <- max(0, mu - 4 * se)
    x_max <- min(1, mu + 4 * se)

    x <- seq(x_min, x_max, length.out = 1000)
    y <- dnorm(x, mean = mu, sd = se)

    shade_x <- x[x >= lb & x <= ub]
    shade_y <- y[x >= lb & x <= ub]

    y_peak <- max(y)

    par(
      mar = c(5, 4, 4, 2),
      bty = "n"
    )

    plot(
      x,
      y,
      type = "n",
      xlim = c(0, 1),
      ylim = c(-0.08 * y_peak, 1.5 * y_peak),
      xlab = "Population Proportion p",
      ylab = "",
      yaxt = "n",
      main = paste0(
        input$conf_level,
        "% Confidence Interval: [",
        sprintf("%.4f", lb),
        ", ",
        sprintf("%.4f", ub),
        "]"
      ),
      axes = FALSE
    )

    axis(
      side = 1,
      at = seq(0, 1, by = 0.1)
    )

    polygon(
      c(shade_x, rev(shade_x)),
      c(shade_y, rep(0, length(shade_y))),
      col = adjustcolor("#F04E2A", alpha.f = 0.25),
      border = NA
    )

    lines(
      x,
      y,
      lwd = 3,
      col = "#A90533"
    )

    segments(
      x0 = lb,
      y0 = 0,
      x1 = lb,
      y1 = y_peak * 0.95,
      col = "#F04E2A",
      lwd = 2,
      lty = 2
    )

    segments(
      x0 = ub,
      y0 = 0,
      x1 = ub,
      y1 = y_peak * 0.95,
      col = "#F04E2A",
      lwd = 2,
      lty = 2
    )

    text(
      x = mu,
      y = y_peak * 1.18,
      labels = paste0("p-hat = ", sprintf("%.4f", mu)),
      col = "#A90533",
      cex = 1
    )

    text(
      x = lb,
      y = y_peak * 0.5,
      labels = paste0("Lower\n", sprintf("%.4f", lb)),
      col = "#F04E2A",
      pos = 2,
      cex = 0.9
    )

    text(
      x = ub,
      y = y_peak * 0.5,
      labels = paste0("Upper\n", sprintf("%.4f", ub)),
      col = "#F04E2A",
      pos = 4,
      cex = 0.9
    )
  })
}

shinyApp(ui = ui, server = server)