#------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
#------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(MASS)
library(bslib)
library(readr)

#------------------------------------------------------------------------------
# 2. DATA LOADING & PRE-PROCESSING
#------------------------------------------------------------------------------

generate_synthetic_raw <- function(n = 1000) {
  set.seed(123)
  data.frame(
    age = sample(18:85, n, replace = TRUE),
    gender = sample(0:1, n, replace = TRUE),
    bmi = rnorm(n, 28, 5),
    income = rlnorm(n, meanlog = 10, sdlog = 1),
    ndvisit = rpois(n, lambda = 2),
    dvisit = rnegbin(n, mu = 4, theta = 1),
    general = sample(1:5, n, replace = TRUE),
    mental = sample(1:5, n, replace = TRUE),
    region = sample(1:4, n, replace = TRUE),
    ethnicity = sample(1:4, n, replace = TRUE),
    education = sample(8:20, n, replace = TRUE),
    hypertension = sample(0:1, n, replace = TRUE),
    hyperlipidemia = sample(0:1, n, replace = TRUE),
    dvexpend = ifelse(
      rbinom(n, 1, 0.7) == 1,
      rgamma(n, shape = 2, scale = 500),
      0
    )
  )
}

# --- LOAD DATA ---
if (file.exists("meps.txt")) {
  message("Loading real data from meps.txt...")
  meps_raw <- readr::read_delim(
    "meps.txt",
    delim = "\t",
    show_col_types = FALSE
  )
} else {
  message("meps.txt not found. Generating synthetic data...")
  meps_raw <- generate_synthetic_raw(1000)
}

# --- CLEAN DATA ---
meps <- meps_raw %>%
  mutate(
    general = factor(
      general,
      levels = 1:5,
      labels = c("Excellent", "VGood", "Good", "Fair", "Poor")
    ),
    mental = factor(
      mental,
      levels = 1:5,
      labels = c("Excellent", "VGood", "Good", "Fair", "Poor")
    ),
    region = factor(
      region,
      levels = 1:4,
      labels = c("Northeast", "Midwest", "South", "West")
    ),
    ethnicity = factor(
      ethnicity,
      levels = 1:4,
      labels = c("White", "Black", "Native American", "Others")
    ),
    gender = factor(gender, levels = c(0, 1), labels = c("Female", "Male")),
    hypertension = factor(
      hypertension,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),
    hyperlipidemia = factor(
      hyperlipidemia,
      levels = c(0, 1),
      labels = c("No", "Yes")
    ),

    education_cat_detailed = case_when(
      education < 12 ~ "Less than High School",
      education >= 12 & education < 13 ~ "High School Graduate",
      education >= 13 & education < 16 ~ "Some College",
      education >= 16 & education < 17 ~ "College Graduate",
      education >= 17 ~ "Post-Graduate",
      TRUE ~ NA_character_
    ),
    education_cat_detailed = factor(
      education_cat_detailed,
      levels = c(
        "Less than High School",
        "High School Graduate",
        "Some College",
        "College Graduate",
        "Post-Graduate"
      )
    ),

    has_expense = ifelse(dvexpend > 0, 1, 0)
  )

meps_clean <- na.omit(meps)
meps_pos <- meps_clean %>% filter(dvexpend > 0)

# --- CALCULATE LIMITS ---
limits <- list(
  age = list(min = min(meps_clean$age), max = max(meps_clean$age)),
  bmi = list(
    min = floor(min(meps_clean$bmi)),
    max = ceiling(max(meps_clean$bmi))
  ),
  income = list(
    min = 0,
    max = ceiling(max(meps_clean$income)),
    mean = round(mean(meps_clean$income))
  ),
  ndvisit = list(min = 0, max = max(meps_clean$ndvisit))
)

#------------------------------------------------------------------------------
# 3. FIT MODELS
#------------------------------------------------------------------------------
m_part1 <- glm(
  has_expense ~ age +
    gender +
    bmi +
    general +
    mental +
    region +
    ethnicity +
    hypertension +
    hyperlipidemia +
    income +
    ndvisit +
    education_cat_detailed,
  family = binomial(link = "probit"),
  data = meps_clean
)
m_part2 <- glm(
  dvexpend ~ age +
    gender +
    general +
    mental +
    region +
    hypertension +
    hyperlipidemia +
    income +
    ndvisit +
    education_cat_detailed,
  family = Gamma(link = "log"),
  data = meps_pos
)
m_count <- glm.nb(
  dvisit ~ age +
    gender +
    bmi +
    general +
    mental +
    ethnicity +
    region +
    hypertension +
    hyperlipidemia +
    income +
    ndvisit +
    education_cat_detailed,
  data = meps_clean
)

#------------------------------------------------------------------------------
# 4. UI DEFINITION
#------------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("MEPS Healthcare Model Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Patient Configuration"),

      # --- BUTTON GROUP ---
      div(
        class = "d-grid gap-2",
        actionButton(
          "run_pred",
          "1. Estimate Outcomes",
          class = "btn-primary btn-lg"
        ),
        actionButton(
          "save_baseline",
          "2. Lock as Baseline",
          class = "btn-secondary",
          icon = icon("lock")
        )
      ),
      helpText(
        "Bonus (added after assessment deadline): Lock a baseline, change inputs, then Estimate again to compare."
      ),
      hr(),
      # --------------------

      sliderInput(
        "age",
        "Age (Years):",
        min = limits$age$min,
        max = limits$age$max,
        value = round(mean(c(limits$age$min, limits$age$max)))
      ),
      sliderInput(
        "bmi",
        "BMI:",
        min = limits$bmi$min,
        max = limits$bmi$max,
        value = 28,
        step = 0.1
      ),
      numericInput(
        "income",
        "Annual Income ($):",
        min = limits$income$min,
        max = limits$income$max,
        value = limits$income$mean,
        step = 1000
      ),
      numericInput(
        "ndvisit",
        "Non-Doctor Visits (Prior):",
        min = limits$ndvisit$min,
        max = limits$ndvisit$max,
        value = 0
      ),

      hr(),
      h5("Demographics"),
      selectInput("gender", "Gender:", choices = levels(meps_clean$gender)),
      selectInput(
        "ethnicity",
        "Ethnicity:",
        choices = levels(meps_clean$ethnicity)
      ),
      selectInput(
        "education",
        "Education Level:",
        choices = levels(meps_clean$education_cat_detailed)
      ),
      selectInput("region", "Region:", choices = levels(meps_clean$region)),

      hr(),
      h5("Clinical Factors"),
      selectInput(
        "general",
        "General Health:",
        choices = levels(meps_clean$general)
      ),
      selectInput(
        "mental",
        "Mental Health:",
        choices = levels(meps_clean$mental)
      ),
      checkboxInput("hypertension", "Has Hypertension?", value = FALSE),
      checkboxInput("hyperlipidemia", "Has Hyperlipidemia?", value = FALSE)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Predictions",
          br(),
          h3("Model Results"),
          p(
            "Estimates based on Two-Part Model (Probit/Gamma) and Negative Binomial regression."
          ),
          hr(),

          # We use uiOutput here to allow dynamic HTML (Green/Red colors)
          layout_column_wrap(
            width = 1 / 2,
            card(
              class = "bg-light",
              card_header("Expected Annual Expenditure"),
              card_body(uiOutput("total_cost_ui")),
              card_footer("Prob(Expense) × E[Cost|Expense]")
            ),
            card(
              class = "bg-light",
              card_header("Expected Doctor Visits"),
              card_body(uiOutput("visit_count_ui")),
              card_footer("Count per year")
            )
          ),

          br(),
          h4("Detailed Breakdown"),
          tableOutput("breakdown_table")
        ),
        tabPanel(
          "Population Context",
          br(),
          h4("Where does this patient fit?"),
          p(
            "Comparing the predicted expenditure against the distribution of observed positive expenditures."
          ),
          plotOutput("dist_plot", height = "500px")
        )
      )
    )
  )
)

#------------------------------------------------------------------------------
# 5. SERVER LOGIC
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive Value to store the baseline predictions
  baseline_data <- reactiveVal(NULL)

  # When user clicks "Lock as Baseline", save the current predictions
  observeEvent(input$save_baseline, {
    req(predictions()) # Ensure we have predictions to save
    baseline_data(predictions())
    showNotification(
      "Baseline Locked! Now change inputs and click 'Estimate' to compare.",
      type = "message"
    )
  })

  new_patient <- eventReactive(
    input$run_pred,
    {
      hyp_val <- if (input$hypertension) "Yes" else "No"
      lip_val <- if (input$hyperlipidemia) "Yes" else "No"

      data.frame(
        age = input$age,
        bmi = input$bmi,
        income = input$income,
        ndvisit = input$ndvisit,
        gender = factor(input$gender, levels = levels(meps_clean$gender)),
        general = factor(input$general, levels = levels(meps_clean$general)),
        mental = factor(input$mental, levels = levels(meps_clean$mental)),
        region = factor(input$region, levels = levels(meps_clean$region)),
        ethnicity = factor(
          input$ethnicity,
          levels = levels(meps_clean$ethnicity)
        ),
        education_cat_detailed = factor(
          input$education,
          levels = levels(meps_clean$education_cat_detailed)
        ),
        hypertension = factor(hyp_val, levels = c("No", "Yes")),
        hyperlipidemia = factor(lip_val, levels = c("No", "Yes"))
      )
    },
    ignoreNULL = FALSE
  )

  predictions <- reactive({
    pat <- new_patient()

    # --- VALIDATION ---
    is_income_valid <- pat$income >= limits$income$min &
      pat$income <= limits$income$max
    is_visit_valid <- pat$ndvisit >= limits$ndvisit$min &
      pat$ndvisit <= limits$ndvisit$max
    validate(
      need(
        is_income_valid,
        paste0(
          "Error: Income must be between $",
          limits$income$min,
          " and $",
          format(limits$income$max, big.mark = ",")
        )
      ),
      need(
        is_visit_valid,
        paste0(
          "Error: Visits must be between ",
          limits$ndvisit$min,
          " and ",
          limits$ndvisit$max
        )
      )
    )

    prob_any <- predict(m_part1, newdata = pat, type = "response")
    cond_cost <- predict(m_part2, newdata = pat, type = "response")
    expected_total <- prob_any * cond_cost
    visit_count <- predict(m_count, newdata = pat, type = "response")

    list(
      prob = prob_any,
      cond_cost = cond_cost,
      expected_total = expected_total,
      visits = visit_count
    )
  })

  # --- DYNAMIC UI OUTPUTS (Handles Comparison Logic) ---

  output$total_cost_ui <- renderUI({
    curr <- predictions()$expected_total
    base <- baseline_data()

    curr_txt <- paste0("$", format(round(curr, 2), big.mark = ","))

    if (is.null(base)) {
      # No baseline -> Show simple text
      return(h2(curr_txt, style = "color: #2c3e50; font-weight: bold;"))
    } else {
      # Baseline exists -> Show comparison
      diff <- curr - base$expected_total
      color <- if (diff > 0) {
        "#e74c3c"
      } else if (diff < 0) {
        "#27ae60"
      } else {
        "gray"
      } # Red if cost up, Green if cost down
      arrow <- if (diff > 0) {
        "▲"
      } else if (diff < 0) {
        "▼"
      } else {
        "-"
      }

      tagList(
        h2(
          curr_txt,
          style = "color: #2c3e50; font-weight: bold; margin-bottom: 0;"
        ),
        div(
          style = paste(
            "color:",
            color,
            "; font-size: 1.1rem; font-weight: bold;"
          ),
          paste0(
            arrow,
            " $",
            format(abs(round(diff, 2)), big.mark = ","),
            " vs Baseline"
          )
        )
      )
    }
  })

  output$visit_count_ui <- renderUI({
    curr <- predictions()$visits
    base <- baseline_data()

    curr_txt <- paste(round(curr, 1), "Visits")

    if (is.null(base)) {
      return(h2(curr_txt, style = "color: #2c3e50; font-weight: bold;"))
    } else {
      diff <- curr - base$visits
      color <- if (diff > 0) {
        "#e74c3c"
      } else if (diff < 0) {
        "#27ae60"
      } else {
        "gray"
      }
      arrow <- if (diff > 0) {
        "▲"
      } else if (diff < 0) {
        "▼"
      } else {
        "-"
      }

      tagList(
        h2(
          curr_txt,
          style = "color: #2c3e50; font-weight: bold; margin-bottom: 0;"
        ),
        div(
          style = paste(
            "color:",
            color,
            "; font-size: 1.1rem; font-weight: bold;"
          ),
          paste0(arrow, " ", round(abs(diff), 1), " vs Baseline")
        )
      )
    }
  })

  output$breakdown_table <- renderTable(
    {
      preds <- predictions()
      base <- baseline_data()

      df <- tibble(
        Metric = c(
          "Probability of Any Expense",
          "Estimated Cost if Seen",
          "Final Expected Cost",
          "Expected Visits"
        ),
        Current = c(
          paste0(round(preds$prob * 100, 1), "%"),
          paste0("$", format(round(preds$cond_cost, 2), big.mark = ",")),
          paste0("$", format(round(preds$expected_total, 2), big.mark = ",")),
          round(preds$visits, 2)
        )
      )

      if (!is.null(base)) {
        df$Baseline = c(
          paste0(round(base$prob * 100, 1), "%"),
          paste0("$", format(round(base$cond_cost, 2), big.mark = ",")),
          paste0("$", format(round(base$expected_total, 2), big.mark = ",")),
          round(base$visits, 2)
        )

        # Calculate raw numeric differences for the last column
        diffs <- c(
          (preds$prob - base$prob) * 100,
          preds$cond_cost - base$cond_cost,
          preds$expected_total - base$expected_total,
          preds$visits - base$visits
        )

        # Formatting differences
        df$Difference = sapply(1:4, function(i) {
          val <- diffs[i]
          prefix <- if (val > 0) "+" else ""
          if (i == 1) {
            return(paste0(prefix, round(val, 1), "%"))
          } # Prob
          if (i == 4) {
            return(paste0(prefix, round(val, 2)))
          } # Visits
          return(paste0(prefix, "$", format(round(val, 2), big.mark = ","))) # Costs
        })
      }

      df
    },
    bordered = TRUE,
    striped = TRUE,
    width = "100%"
  )

  output$dist_plot <- renderPlot({
    pat_cost <- predictions()$expected_total

    # Calculate Percentile
    fn_percentile <- ecdf(meps_pos$dvexpend)
    pct_val <- fn_percentile(pat_cost)
    pct_text <- paste0(round(pct_val * 100, 1), "th Percentile")
    label_text <- paste0("This Patient\n(", pct_text, ")")

    ggplot(meps_pos, aes(x = dvexpend)) +
      geom_density(fill = "#3498db", alpha = 0.4) +
      geom_vline(
        xintercept = pat_cost,
        color = "#e74c3c",
        linewidth = 1.5,
        linetype = "dashed"
      ) +
      annotate(
        "text",
        x = pat_cost,
        y = 0,
        label = label_text,
        vjust = -0.5,
        hjust = -0.1,
        color = "#e74c3c",
        angle = 90,
        fontface = "bold",
        size = 5
      ) +
      scale_x_log10(labels = scales::dollar) +
      labs(
        title = "Patient Expenditure vs. Population Distribution",
        subtitle = paste0(
          "Patient is in the ",
          pct_text,
          " of positive expenditures"
        ),
        x = "Expenditure (Log Scale)",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
