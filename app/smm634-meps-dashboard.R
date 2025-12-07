#------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
#------------------------------------------------------------------------------
print("STEP 1: Loading Libraries...")
library(shiny)
library(tidyverse)
library(MASS)
library(bslib)
library(readr)
library(GJRM)

#------------------------------------------------------------------------------
# 2. DATA LOADING
#------------------------------------------------------------------------------
print("STEP 2: Loading Data...")

generate_synthetic_raw <- function(n = 2000) {
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

if (file.exists("meps.txt")) {
  message("   -> Loading real data from meps.txt...")
  meps_raw <- readr::read_delim(
    "meps.txt",
    delim = "\t",
    show_col_types = FALSE
  )
} else {
  message("   -> meps.txt not found. Generating synthetic data...")
  meps_raw <- generate_synthetic_raw(2000)
}

#------------------------------------------------------------------------------
# 3. DATA CLEANING
#------------------------------------------------------------------------------
print("STEP 3: Cleaning Data...")

meps <- meps_raw %>%
  mutate(
    # Scaling income for the Copula to avoid numerical overflow
    income_cop = income / 1000,

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

print("   -> Creating meps_pos dataframe...")
meps_pos <- as.data.frame(dplyr::filter(meps_clean, dvexpend > 0))

if (nrow(meps_pos) == 0) {
  stop("ERROR: meps_pos has 0 rows!")
}

# --- LIMITS ---
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
# 4. MODEL fitting
#------------------------------------------------------------------------------
print("STEP 4: Fitting Models...")

print("   -> Fitting GLMs...")
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

print("   -> Fitting Copula (GJRM)...")
f1 <- dvisit ~ age +
  gender +
  bmi +
  general +
  mental +
  ethnicity +
  region +
  hypertension +
  hyperlipidemia +
  income_cop +
  ndvisit +
  education_cat_detailed
f2 <- dvexpend ~ age +
  gender +
  general +
  mental +
  region +
  hypertension +
  hyperlipidemia +
  income_cop +
  ndvisit +
  education_cat_detailed

args_list <- list(
  formula = list(f1, f2),
  data = meps_pos,
  margins = c("NBII", "GA"),
  copula = "N",
  model = "B"
)

fit_cop <- do.call(gjrm, args_list)

theta_val <- summary(fit_cop)$theta
tau_val <- 2 / pi * asin(theta_val)
print(paste("   -> Copula Fitted. Tau =", round(tau_val, 3)))

#------------------------------------------------------------------------------
# 5. UI
#------------------------------------------------------------------------------
print("STEP 5: Launching UI...")
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  titlePanel("MEPS Healthcare Model Dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Patient Configuration"),
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
        "(Bonus - added post assignment deadline): Lock a baseline, change inputs, then estimate again to compare."
      ),
      hr(),
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
        # --- TAB 1: PREDICTIONS ---
        tabPanel(
          "Predictions",
          br(),
          h3("Standard Model Results"),
          p(
            "Estimates based on separate Probit/Gamma and Negative Binomial regression."
          ),
          hr(),
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

        # --- TAB 2: POPULATION CONTEXT ---
        tabPanel(
          "Population Context",
          br(),
          h4("Where does this patient fit?"),
          p(
            "Comparing the predicted expenditure against the distribution of observed positive expenditures."
          ),
          plotOutput("dist_plot", height = "500px")
        ),

        # --- TAB 3: COPULA ANALYSIS ---
        tabPanel(
          "(Bonus - added post assignment deadline) Copula Analysis",
          br(),
          h3("Joint Modeling: Visits & Cost"),
          p(
            "This model connects Doctor Visits and Expenditure using a Gaussian Copula to account for hidden correlations."
          ),
          card(
            class = "border-primary mb-3",
            card_header("Model Dependence Parameter"),
            card_body(
              h4(paste0("Kendall's Tau: ", round(tau_val, 3))),
              p(
                "Correlation between Visits and Cost after accounting for patient characteristics."
              )
            )
          ),
          h4("Model Comparison"),
          tableOutput("copula_comparison_table"),
          br(),
          plotOutput("copula_plot", height = "400px")
        )
      )
    )
  )
)

#------------------------------------------------------------------------------
# 6. SERVER LOGIC
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  baseline_data <- reactiveVal(NULL)

  observeEvent(input$save_baseline, {
    req(predictions())
    baseline_data(predictions())
    showNotification("Baseline Locked!", type = "message")
  })

  new_patient <- eventReactive(
    input$run_pred,
    {
      hyp_val <- if (input$hypertension) "Yes" else "No"
      lip_val <- if (input$hyperlipidemia) "Yes" else "No"

      df <- data.frame(
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
      # Copula input must match training data (Scaled by 1000)
      df$fit_copop <- df$income / 1000

      df
    },
    ignoreNULL = FALSE
  )

  predictions <- reactive({
    pat <- new_patient()

    validate(
      need(
        pat$income >= limits$income$min & pat$income <= limits$income$max,
        "Income out of range"
      ),
      need(
        pat$ndvisit >= limits$ndvisit$min & pat$ndvisit <= limits$ndvisit$max,
        "Visits out of range"
      )
    )

    # GLM Predictions (Standard predict returns counts/dollars correctly)
    prob_any <- predict(m_part1, newdata = pat, type = "response")
    cond_cost_indep <- predict(m_part2, newdata = pat, type = "response")
    expected_total <- prob_any * cond_cost_indep
    visit_count <- predict(m_count, newdata = pat, type = "response")

    # --- COPULA PREDICTIONS (FIXED) ---

    # Equation 1: Visits (Nbin uses log-link -> apply exp)
    cop_visits_link <- predict(
      fit_cop,
      eq = 1,
      newdata = pat,
      type = "response"
    )
    cop_visits <- exp(cop_visits_link)

    # Equation 2: Cost (Gamma uses log-link -> apply exp)
    cop_cost_link <- predict(fit_cop, eq = 2, newdata = pat, type = "response")
    cop_cost <- exp(cop_cost_link)

    list(
      prob = prob_any,
      cond_cost = cond_cost_indep,
      cop_cost = cop_cost,
      expected_total = expected_total,
      visits = visit_count,
      cop_visits = cop_visits
    )
  })

  # --- OUTPUTS ---
  output$total_cost_ui <- renderUI({
    curr <- predictions()$expected_total
    base <- baseline_data()
    curr_txt <- paste0("$", format(round(curr, 2), big.mark = ","))
    if (is.null(base)) {
      h2(curr_txt, style = "color: #2c3e50; font-weight: bold;")
    } else {
      diff <- curr - base$expected_total
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
      h2(curr_txt, style = "color: #2c3e50; font-weight: bold;")
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
      tibble(
        Metric = c(
          "Probability of Any Expense",
          "Estimated Cost (Conditional)",
          "Final Expected Cost",
          "Expected Visits"
        ),
        Value = c(
          paste0(round(preds$prob * 100, 1), "%"),
          paste0("$", format(round(preds$cond_cost, 2), big.mark = ",")),
          paste0("$", format(round(preds$expected_total, 2), big.mark = ",")),
          round(preds$visits, 2)
        )
      )
    },
    bordered = TRUE,
    striped = TRUE,
    width = "100%"
  )

  output$dist_plot <- renderPlot({
    pat_cost <- predictions()$expected_total
    fn_percentile <- ecdf(meps_pos$dvexpend)
    pct_val <- fn_percentile(pat_cost)
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
        label = paste0(
          "This Patient\n(",
          round(pct_val * 100, 1),
          "th Percentile)"
        ),
        vjust = -0.5,
        hjust = -0.1,
        color = "#e74c3c",
        angle = 90,
        fontface = "bold"
      ) +
      scale_x_log10(labels = scales::dollar) +
      theme_minimal() +
      labs(
        title = "Expenditure Distribution",
        x = "Log Expenditure",
        y = "Density"
      )
  })

  output$copula_comparison_table <- renderTable(
    {
      preds <- predictions()
      tibble(
        Model = c(
          "Standard Independent Model (GLM)",
          "Joint Copula Model (GJRM)"
        ),
        `Expected Visits` = c(
          round(preds$visits, 2),
          round(preds$cop_visits, 2)
        ),
        `Expected Cost (if > 0)` = c(
          paste0("$", format(round(preds$cond_cost, 2), big.mark = ",")),
          paste0("$", format(round(preds$cop_cost, 2), big.mark = ","))
        ),
        `Correlation (Tau)` = c("Assumed 0", round(tau_val, 3))
      )
    },
    bordered = TRUE,
    striped = TRUE,
    width = "100%"
  )

  output$copula_plot <- renderPlot({
    preds <- predictions()
    df_plot <- data.frame(
      Model = c("Independent", "Copula"),
      Cost = c(preds$cond_cost, preds$cop_cost)
    )
    ggplot(df_plot, aes(x = Model, y = Cost, fill = Model)) +
      geom_col(width = 0.5) +
      geom_text(
        aes(label = paste0("$", round(Cost, 0))),
        vjust = -0.5,
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("#95a5a6", "#34495e")) +
      ylim(0, max(df_plot$Cost) * 1.2) +
      labs(
        title = "Impact of Joint Modeling on Cost Prediction",
        y = "Predicted Cost ($)"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  })
}

shinyApp(ui = ui, server = server)
