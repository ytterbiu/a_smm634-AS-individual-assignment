#------------------------------------------------------------------------------
# 1. SETUP & LIBRARIES
#------------------------------------------------------------------------------
library(shiny)
library(tidyverse)
library(MASS) # For glm.nb
library(bslib) # For modern UI themes
library(readr) # For read_delim

#------------------------------------------------------------------------------
# 2. DATA LOADING & PRE-PROCESSING
#------------------------------------------------------------------------------

# Function to generate synthetic RAW data (if meps.txt is missing)
generate_synthetic_raw <- function(n = 1000) {
  set.seed(123)
  data.frame(
    age = sample(18:85, n, replace = TRUE),
    gender = sample(0:1, n, replace = TRUE), # 0/1 Raw
    bmi = rnorm(n, 28, 5),
    income = rlnorm(n, meanlog = 10, sdlog = 1),
    ndvisit = rpois(n, lambda = 2),
    dvisit = rnegbin(n, mu = 4, theta = 1),

    # Raw ordinal/nominal codes matching your script
    general = sample(1:5, n, replace = TRUE),
    mental = sample(1:5, n, replace = TRUE),
    region = sample(1:4, n, replace = TRUE),
    ethnicity = sample(1:4, n, replace = TRUE),

    # Raw education years
    education = sample(8:20, n, replace = TRUE),

    hypertension = sample(0:1, n, replace = TRUE),
    hyperlipidemia = sample(0:1, n, replace = TRUE),

    # Generate expenditure correlated with visits
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
  meps_raw <- readr::read_delim("meps.txt", delim = "\t")
} else {
  message("meps.txt not found. Generating synthetic data for demonstration...")
  meps_raw <- generate_synthetic_raw(1000)
}

# --- CLEAN DATA (Logic from your script) ---
meps <- meps_raw %>%
  mutate(
    # 1. Standard ordinal / nominal factors
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

    # 2. Binary factors
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

    # 3. Education categories detailed
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

    # 4. Expense flag
    has_expense = ifelse(dvexpend > 0, 1, 0)
  )

meps_clean <- na.omit(meps)
meps_pos <- meps_clean %>% filter(dvexpend > 0)

#------------------------------------------------------------------------------
# 3. TRAIN MODELS
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

      # --- BUTTON MOVED HERE ---
      actionButton(
        "run_pred",
        "Estimate Outcomes",
        class = "btn-primary btn-lg",
        width = "100%"
      ),
      hr(),
      # -------------------------

      # Numeric Inputs
      sliderInput("age", "Age (Years):", min = 18, max = 90, value = 45),
      sliderInput("bmi", "BMI:", min = 15, max = 60, value = 28, step = 0.1),
      numericInput("income", "Annual Income ($):", value = 50000, step = 1000),
      numericInput("ndvisit", "Non-Doctor Visits (Prior):", value = 0, min = 0),

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
        # Results Tab
        tabPanel(
          "Predictions",
          br(),
          h3("Model Results"),
          p(
            "Estimates based on Two-Part Model (Probit/Gamma) and Negative Binomial regression."
          ),
          hr(),

          layout_column_wrap(
            width = 1 / 2,
            card(
              class = "bg-light",
              card_header("Expected Annual Expenditure"),
              card_body(h2(
                textOutput("total_cost"),
                style = "color: #2c3e50; font-weight: bold;"
              )),
              card_footer("Prob(Expense) × E[Cost|Expense]")
            ),
            card(
              class = "bg-light",
              card_header("Expected Doctor Visits"),
              card_body(h2(
                textOutput("visit_count"),
                style = "color: #2c3e50; font-weight: bold;"
              )),
              card_footer("Count per year")
            )
          ),
          br(),
          h4("Detailed Breakdown"),
          tableOutput("breakdown_table")
        ),

        # Distribution Tab
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

    # 1. Probit (Prob of any expense)
    prob_any <- predict(m_part1, newdata = pat, type = "response")

    # 2. Gamma (Cost if expense > 0)
    cond_cost <- predict(m_part2, newdata = pat, type = "response")

    # 3. Two-Part Model Result
    expected_total <- prob_any * cond_cost

    # 4. Count Model
    visit_count <- predict(m_count, newdata = pat, type = "response")

    list(
      prob = prob_any,
      cond_cost = cond_cost,
      expected_total = expected_total,
      visits = visit_count
    )
  })

  output$total_cost <- renderText({
    paste0("$", format(round(predictions()$expected_total, 2), big.mark = ","))
  })

  output$visit_count <- renderText({
    paste(round(predictions()$visits, 1), "Visits")
  })

  output$breakdown_table <- renderTable(
    {
      preds <- predictions()
      tibble(
        Metric = c(
          "Probability of Any Expense (Part 1)",
          "Estimated Cost if Seen (Part 2)",
          "Final Expected Cost (Combined)",
          "Expected Number of Doctor Visits"
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
        label = "This Patient",
        vjust = -1,
        hjust = -0.1,
        color = "#e74c3c",
        angle = 90,
        fontface = "bold"
      ) +
      scale_x_log10(labels = scales::dollar) +
      labs(
        title = "Patient Expenditure vs. Population Distribution",
        subtitle = "Comparing predicted cost against patients with positive expenditure",
        x = "Expenditure (Log Scale)",
        y = "Density"
      ) +
      theme_minimal(base_size = 14)
  })
}

shinyApp(ui = ui, server = server)
