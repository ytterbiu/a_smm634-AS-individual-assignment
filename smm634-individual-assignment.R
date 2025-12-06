# ==============================================================================
# SMM047 Probability and Mathematical Statistics (Subject CS1)
# Individual Coursework
# Author: Benjamin Evans
# Professor:    Professor Rosalba Radice
# Institution:  Bayes Business School - City St George's, University of London
# Date:         05/Dec/2025
# Description:  Term 1 individual project for Analytics Methods for Business.
# ==============================================================================
#
#
#

#----------------------- Initial setup (knitr settings) -----------------------#
dir.create("fig", showWarnings = FALSE)

# Defaults common to all outputs
knitr::opts_chunk$set(
  echo = TRUE,
  message = FALSE,
  warning = FALSE,
  fig.align = "center",
  out.width = "100%",
  fig.path = "fig/",
  dpi = 300
)

# Output-specific settings
if (knitr::is_latex_output()) {
  knitr::opts_chunk$set(
    fig.width = 6,
    fig.height = 4,
    dev = "pdf",
    fig.pos = "ht",
    out.extra = ""
  )
} else {
  knitr::opts_chunk$set(
    fig.width = 6,
    fig.height = 4,
    dev = "svglite" # or "png"
  )
}

if (knitr::is_latex_output()) {
  cat("\\renewcommand{\\arraystretch}{1.1}\n")
}

#----------------------------- Clean environment ------------------------------#
rm(list = ls()) # Remove all objects
graphics.off() # Close all graphical devices
cat("\014") # Clean console

#------------------- Load dependencies / external libraries -------------------#
library(tidyverse)
library(glmnet)
library(MASS)
library(ROCR)
library(GJRM) # Copula

library(broom)

library(ggplot2)
library(dplyr)
library(patchwork)
library(kableExtra)
library(stringr)

# for custom functions
library(clipr) # for banner_comment function qol to annotate code


#---------------------------- Custom QOL functions ----------------------------#
#####################################
# function: banner comments (used to to section up code)
# Usage: banner_comment("Element 1: data cleaning") -> then ctrl + v (or cmd+v)
#####################################
banner_comment <- function(text, width = 80, border = "#", fill = "-") {
  txt <- paste0(" ", text, " ")
  inner_width <- width - 2 * nchar(border)
  banner_string <- ""

  if (inner_width <= nchar(txt)) {
    banner_string <- paste0(border, txt, border)
  } else {
    pad_total <- inner_width - nchar(txt)
    pad_left <- pad_total %/% 2
    pad_right <- pad_total - pad_left

    banner_string <- paste0(
      border,
      strrep(fill, pad_left),
      txt,
      strrep(fill, pad_right),
      border
    )
  }

  cat(banner_string, "\n")
  # copy banner to allow direct pasting (requires clipr)
  clipr::write_clip(banner_string)
  # avoid [1] when printing if want to manually copy
  invisible(banner_string)
}
#####################################
# function: format p-values for text
# Usage (in-line): `r format_p_vals(ad_test_result$p.value)`
# Usage (console): format_p_vals(ad_test_result$p.value)
#####################################
format_p_vals <- function(p) {
  if (length(p) != 1L || is.na(p)) {
    stop("Error! p must be a single non-missing value")
  }
  if (p > 1) {
    stop("Error! Value greater than 1")
  }
  if (p < 0) {
    stop("Error! Value less than 0")
  }

  if (p >= 0.01) {
    paste0("= ", formatC(p, format = "f", digits = 2))
  } else if (p >= 0.001) {
    paste0("= ", formatC(p, format = "f", digits = 3))
  } else {
    "< 0.001"
  }
}
#####################################
# function: format confidence intervals for tables & text
# Usage (in-line): `r format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2])`
# Usage (console): format_interval(el2_ci_normal_95[1], el2_ci_normal_95[2])
#####################################
format_interval <- function(lower, upper, digits = 3) {
  paste0(
    "[",
    formatC(lower, format = "f", digits = digits),
    ", ",
    formatC(upper, format = "f", digits = digits),
    "]"
  )
}
#####################################
# function: format variable names in green in latex (& normal code in html)
# Usage (in-line): TBI `r format_var_name("dvisit")`
#####################################
format_var_name <- function(x) {
  if (knitr::is_latex_output()) {
    # replace all "_" with "\_" in va name
    paste0("\\greentt{", gsub("_", "\\\\_", x), "}")
  } else {
    # change to paste0("<code style='color:green'>", x, "</code>") for green
    # paste0("`", x, "`")
    paste0("<code style='color:green'>", x, "</code>")
  }
}
#####################################
# function: quick format (~legacy qol function)
#####################################
fmt <- function(n, digits = 0, big_mark = ",") {
  formatC(n, format = "f", digits = digits, big.mark = big_mark)
}

#---------------------------- Download / Load data ----------------------------#
meps <- meps_raw <- readr::read_delim("meps.txt", delim = "\t")

meps %>%
  summarise(across(
    where(is.numeric),
    ~ paste0(min(.x, na.rm = TRUE), " – ", max(.x, na.rm = TRUE))
  ))

edu_levels <- c(
  "Less than High School",
  "High School Graduate",
  "College+"
)

edu_levels_detailed <- c(
  "Less than High School",
  "High School Graduate",
  "Some College",
  "College Graduate",
  "Post-Graduate"
)

meps <- meps_raw %>%
  mutate(
    # --- 1. standard ordinal / nominal factors ---
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
    # --- 2. binary factors (0/1 mappings) ---
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
    # --- education categories
    # add 'education_cat' for categorical analysis.
    education_cat = case_when(
      education < 12 ~ "Less than High School",
      education >= 12 & education < 13 ~ "High School Graduate",
      education >= 13 ~ "College +",
      TRUE ~ NA_character_
    ),
    education_cat = factor(
      education_cat,
      levels = c("Less than High School", "High School Graduate", "College +")
    ),
    education_cat_detailed = case_when(
      education < 12 ~ "Less than High School",
      education >= 12 & education < 13 ~ "High School Graduate", # Catches 12 and 12.5
      education >= 13 & education < 16 ~ "Some College", # Catches 13 to 15.9
      education >= 16 & education < 17 ~ "College Graduate", # Catches 16 and 16.5
      education >= 17 ~ "Post-Graduate",
      TRUE ~ NA_character_
    ),
    # Ensure levels are ordered by SES for proper reference group in regression
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
    # adds has doctors expense check
    has_expense = ifelse(dvexpend > 0, 1, 0)
  )

check_levels <- list(
  Gen = levels(meps$general),
  Men = levels(meps$mental),
  Eth = levels(meps$ethnicity),
  Reg = levels(meps$region),
  Edu = levels(meps$education_cat_detailed)
)

print(check_levels)

meps_all <- meps
# exclude individuals with missing information
meps_clean <- na.omit(meps_all)

# filter out individuals with zero consultations and zero associated expenditures
# Note: Filtering dvexpend > 0 handles this and prepares for Gamma GLM
meps_pos <- meps_clean %>% filter(dvexpend > 0)

zero_visit_zero_cost <- sum(meps_clean$dvisit == 0 & meps_clean$dvexpend == 0)
pos_visit_zero_cost <- sum(meps_clean$dvisit > 0 & meps_clean$dvexpend == 0)
zero_visit_pos_cost <- sum(meps_clean$dvisit == 0 & meps_clean$dvexpend > 0)
# print(paste("Patients with 0 visits AND 0 cost:", zero_visit_zero_cost))
# print(paste("Patients with >0 visits but 0 cost:", pos_visit_zero_cost))
# print(paste("Patients with 0 visits but >0 cost:", zero_visit_pos_cost))

mean_visit <- mean(meps_clean$dvisit)
var_visit <- var(meps_clean$dvisit)

n_total_raw <- nrow(meps_raw)
n_full <- nrow(meps_clean)
n_pos <- nrow(meps_pos)
n_dropped <- n_total_raw - n_full
n_zeros <- n_full - n_pos

# calculate Zero-Cost stats based on the clean dataset
n_zero_cost <- sum(meps_clean$dvexpend == 0)
pct_zero <- (n_zero_cost / n_full) * 100

# stats for visits specifically
n_zero_visit <- sum(meps_clean$dvisit == 0)

# verify clean
glimpse(meps)
summary(meps)
glimpse(meps_clean)
summary(meps_clean)


get_cat_compare <- function(df_full, df_pos, var_name, display_name) {
  # Calculate proportions
  get_props <- function(d, v) {
    t <- table(d[[v]])
    formatC(as.numeric(prop.table(t)), format = "f", digits = 2)
  }
  # get labels and values
  clean_labels <- gsub("^[0-9]:\\s*", "", names(table(df_full[[var_name]])))
  # Tibble with one row per category level
  tibble(
    Variable = format_var_name(display_name),
    Description = clean_labels,
    `Full Sample` = get_props(df_full, var_name),
    `Positive Only` = get_props(df_pos, var_name)
  )
}
# var_summary <- tibble(
#   Variable = names(meps_raw),
#   Type = c("Ordinal Factor", "Ordinal Factor", "Continuous", "Continuous", "Continuous",
#            "Binary Factor", "Nominal Factor", "Categorical", "Nominal Factor",
#            "Binary Factor", "Binary Factor", "Count", "Count", "Continuous", "Continuous"),
#   `Raw Range/Levels` = c("1-5", "1-5", "9.4-68.2", "0-Max", "18-65", "0/1", "1-4", "0-17", "1-4", "0/1", "0/1", "0-29", "0-29", "0-Max", "0-Max"),
#   `Analysis Treatment` = c("Factor (Ref: Exc)", "Factor (Ref: Exc)", "Linear/Spline", "Linear (Log?)", "Linear",
#                            "Factor (Ref: Female)", "Factor (Ref: White)", "Factor (17 levels)", "Factor (Ref: NE)",
#                            "Factor (Ref: No)", "Factor (Ref: No)", "Response (Count)", "Response (Count)", "Response (Gamma)", "Response")
# )
#
# # Display Table
# kbl(
#   var_summary,
#   caption = "MEPS Variable Definitions and Analysis Treatment",
#   booktabs = TRUE
#   ) %>%
#   kable_styling(
#     bootstrap_options = c("striped", "hover")
# )

tab_cat_comp <- bind_rows(
  get_cat_compare(meps_clean, meps_pos, "gender", "gender"),
  get_cat_compare(meps_clean, meps_pos, "ethnicity", "ethnicity"),
  get_cat_compare(
    meps_clean,
    meps_pos,
    "education_cat_detailed",
    "education_cat_detailed"
  ),
  get_cat_compare(meps_clean, meps_pos, "region", "region"),
  get_cat_compare(meps_clean, meps_pos, "hypertension", "hypertension"),
  get_cat_compare(meps_clean, meps_pos, "hyperlipidemia", "hyperlipidemia")
)

header_full <- linebreak(
  paste0("Full Sample\n(N = ", format(n_full, big.mark = ","), ")"),
  align = "c"
)

header_pos <- linebreak(
  paste0("Positive expenditure\n(N = ", format(n_pos, big.mark = ","), ")"),
  align = "c"
)

# Render Categorical Table
# Note: We need to calculate linesep again because we have groups
cat_linesep <- rep("", nrow(tab_cat_comp))
group_ends <- cumsum(rle(as.character(tab_cat_comp$Variable))$lengths)
cat_linesep[group_ends] <- "\\addlinespace"
cat_linesep[nrow(tab_cat_comp)] <- ""

tab_cat_comp %>%
  kbl(
    caption = "Frequency of categorical variables in the full sample of patients and the subset of patients with positive healthcare expenditure.",
    col.names = c("Variable", "Category", header_full, header_pos),
    booktabs = TRUE,
    escape = FALSE,
    align = c("l", "l", "c", "c"),
    linesep = cat_linesep # use the vector you calculated
  ) %>%
  kable_styling(latex_options = c("hold_position", "repeat_header")) %>%
  collapse_rows(columns = 1, valign = "middle", latex_hline = "major")


get_num_compare <- function(
  df_full,
  df_pos,
  var_name,
  display_name,
  digits = 2,
  big_mark = "",
  show_mean_se = FALSE
) {
  # calculate stat based on Toggle
  calc_stat <- function(d, v) {
    x <- d[[v]]

    if (show_mean_se) {
      # --- Calculate Mean (SE) ---
      m <- mean(x, na.rm = TRUE)
      s <- sd(x, na.rm = TRUE)
      n_obs <- sum(!is.na(x))
      se <- s / sqrt(n_obs)

      return(paste0(
        fmt(m, digits = digits),
        " (",
        fmt(s, digits = digits),
        ")"
      ))
    } else {
      # calculate median & [IQR]
      med <- median(x, na.rm = TRUE)
      q1 <- quantile(x, 0.25, na.rm = TRUE)
      q3 <- quantile(x, 0.75, na.rm = TRUE)
      iqr <-
        paste0(
          " [",
          fmt(q1, digits = digits),
          " – ",
          fmt(q3, digits = digits),
          "]"
        )

      return(paste0(fmt(med, digits = digits), iqr))
    }
  }

  # for both groups
  val_full <- calc_stat(df_full, var_name)
  val_pos <- calc_stat(df_pos, var_name)

  # build tibble w. generic column names
  tibble(
    Variable = format_var_name(var_name),
    Description = display_name,
    `Full Sample` = val_full,
    `Positive Only` = val_pos
  )
}

tab_num_comp <- bind_rows(
  get_num_compare(
    meps_clean,
    meps_pos,
    "bmi",
    "Body mass index",
    digits = 2,
    show_mean_se = TRUE
  ),
  get_num_compare(
    meps_clean,
    meps_pos,
    "age",
    "Age (years)",
    digits = 2,
    show_mean_se = TRUE
  ),
  get_num_compare(
    meps_clean,
    meps_pos,
    "education",
    "Education (no. of years)",
    digits = 2,
    show_mean_se = TRUE
  ),
  get_num_compare(
    meps_clean,
    meps_pos,
    "income",
    "Income (USD)",
    digits = 0,
    big_mark = ",",
    show_mean_se = TRUE
  ),

  # utilisation
  get_num_compare(
    meps_clean,
    meps_pos,
    "dvisit",
    "Doctor visits",
    digits = 2,
    show_mean_se = TRUE
  ),
  get_num_compare(
    meps_clean,
    meps_pos,
    "ndvisit",
    "Non-doctor visits",
    digits = 2,
    show_mean_se = TRUE
  ),

  # expenditure
  get_num_compare(
    meps_clean,
    meps_pos,
    "dvexpend",
    "Doctor expenditure (USD)",
    digits = 2,
    big_mark = ",",
    show_mean_se = TRUE
  ),
  get_num_compare(
    meps_clean,
    meps_pos,
    "ndvexpend",
    "Non-doctor expenditure (USD)",
    digits = 2,
    big_mark = ",",
    show_mean_se = TRUE
  )
)

tab_num_comp %>%
  kbl(
    caption = "Summary statistics, mean (standard deviation), for continuous variables in the full sample and among individuals with positive healthcare expenditure.",
    col.names = c("Variable", "Description", header_full, header_pos),
    booktabs = TRUE,
    escape = FALSE,
    linesep = c("", "", "", "\\addlinespace"),
    align = c("l", "l", "l", "l")
  ) %>%
  kable_styling(latex_options = "hold_position") %>%
  column_spec(1, width = "3cm") %>%
  row_spec(0, bold = TRUE)

#----------------------------- Expenditure model ------------------------------#
# Y as the Binary Outcome (0 or 1)
y_binary <- meps_clean$has_expense

# X - full dataset, not just positive
X_full <- model.matrix(
  has_expense ~ . -
    dvisit -
    dvexpend -
    # ndvisit -
    ndvexpend -
    education -
    education_cat,
  data = meps_clean
)[, -1]
# X_full <- model.matrix(
#   has_expense ~ age +
#     gender +
#     general +
#     mental +
#     ethnicity +
#     region +
#     hypertension +
#     bmi,
#   data = meps_clean
# )[, -1]

# Lasso with family = "binomial" (Logistic)
set.seed(123)
cv_lasso_binary <- cv.glmnet(X_full, y_binary, alpha = 1, family = "binomial")

# plot and check
# plot(cv_lasso_binary)
# print(coef(cv_lasso_binary, s = "lambda.1se"))

## now for second part
##
##
y_pos <- log(meps_pos$dvexpend)

X_pos <- model.matrix(
  dvexpend ~ . -
    dvisit -
    dvexpend -
    ndvisit - # playing with toggling this on and off - opted to include
    ndvexpend -
    has_expense -
    education -
    education_cat,
  data = meps_pos
)[, -1]

cv_lasso_pos <- cv.glmnet(X_pos, y_pos, alpha = 1)

# 5. Plot to see the error curve
# plot(cv_lasso_pos)

# 6. Check Active Predictors
# coef(cv_lasso_pos, s = "lambda.1se")

##############################
##############################
##############################
# Actual models
# Part 1: Probit for probability of any expense
# Using full dataset (meps_clean) to predict yes/no for everyone

m_part1 <- glm(
  has_expense ~ age +
    gender +
    bmi + # Selected by Lasso (unlike Part 2)
    general +
    mental +
    region +
    ethnicity +
    hypertension +
    hyperlipidemia +
    income +
    ndvisit +
    education_cat_detailed,
  family = binomial(link = "probit"), # Probit link
  data = meps_clean
)

summary(m_part1)

# Part 2: Gamma GLM for amount of expense if visiting
m_part2 <- glm(
  dvexpend ~ age +
    gender +
    general +
    mental +
    region +
    hypertension +
    hyperlipidemia +
    income +
    ndvisit + # utilisation proxy
    education_cat_detailed,
  family = Gamma(link = "log"),
  data = meps_pos
)

summary(m_part2)

#----------------------------- Utilisation model ------------------------------#
# Check for Overdispersion
mean_visits <- mean(meps_clean$dvisit)
var_visits <- var(meps_clean$dvisit)

# print(paste("Mean:", round(mean_visits, 2)))
# print(paste("Variance:", round(var_visits, 2)))
# if var >> mean -> Poisson invalid -> use Negative Binomial.

# using meps_clean to model 0s and positive counts.
X_count <- model.matrix(
  dvisit ~ . -
    dvisit -
    dvexpend -
    ndvexpend -
    has_expense -
    education -
    education_cat,
  data = meps_clean
)[, -1]

y_count <- meps_clean$dvisit

# lasso (poisson)
cv_lasso_count <- cv.glmnet(X_count, y_count, family = "poisson", alpha = 1)

# check selected variables
coef(cv_lasso_count, s = "lambda.1se")

# pois
m_pois <- glm(
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
  family = poisson(link = "log"),
  data = meps_clean
)

# Negative Binomial Model for Utilization
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

summary(m_count)

# saving metrics to use in-line
# checking pois vs negative binomial through AIC
AIC(m_pois, m_count)
aic_pois <- AIC(m_pois)
aic_negbin <- AIC(m_count)

dispersion_pois <- sum(residuals(m_pois, type = "pearson")^2) /
  m_pois$df.residual
# dispersion_pois

p_overdisp <- pchisq(
  m_pois$deviance,
  df = m_pois$df.residual,
  lower.tail = FALSE
)
# p_overdisp

#### Predictions
####
####
# 1. predict probability
# type = "response" gives probability between 0 and 1
prob_any_expense <- predict(m_part1, newdata = meps_clean, type = "response")

# 2. conditional cost
# predict cost for everbody (assuming they do go to the doctor)
# type = "response" automatically undoes the Log-link (returns dollars, not log-dollars)
cond_cost <- predict(m_part2, newdata = meps_clean, type = "response")

# 3. combine
# Expected Expenditure = Probability * Cost
expected_expenditure <- prob_any_expense * cond_cost

# 4. add df & compare
meps_results <- meps_clean %>%
  mutate(
    pred_prob = prob_any_expense,
    pred_cost_if_seen = cond_cost,
    final_prediction = expected_expenditure,
    actual_cost = dvexpend
  )

# head(
#   meps_results %>% dplyr::select(has_expense, actual_cost, pred_prob, final_prediction)
# )

# evaluation of predictions
meps_results$error <- meps_results$actual_cost - meps_results$final_prediction

# 1. RMSE
rmse <- sqrt(mean(meps_results$error^2))

# 2. Mean Absolute Error
mae <- mean(abs(meps_results$error))

# Print results
# paste0("RMSE: $", round(rmse, 2))
# paste0("MAE:  $", round(mae, 2))

total_actual <- sum(meps_results$actual_cost)
total_pred <- sum(meps_results$final_prediction)
bias_percent <- ((total_pred - total_actual) / total_actual) * 100

# paste0("Total Actual Cost:    $", format(round(total_actual), big.mark = ","))
# paste0("Total Predicted Cost: $", format(round(total_pred), big.mark = ","))
# paste0("Bias: ", round(bias_percent, 2), "%")

# Calculate the errors -> residuals
residuals <- meps_results$actual_cost - meps_results$final_prediction

# IQR
iqr_metric <- IQR(residuals)

# percentiles
quantiles <- quantile(
  residuals,
  probs = c(0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99)
)

# print(paste0("IQR of Error: $", round(iqr_metric, 2)))
# print(quantiles)

eq_visits <- dvisit ~ age +
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
  education_cat_detailed

eq_cost <- dvexpend ~ age +
  gender +
  general +
  mental +
  region +
  hypertension +
  hyperlipidemia +
  income +
  ndvisit +
  education_cat_detailed

form_list <- list(eq_visits, eq_cost)

fit_cop <- gjrm(
  formula = form_list,
  data = meps_pos, # must have both dvisit and dvexpend > 0
  margins = c("NBII", "GA"), # NB2 for counts, Gamma for cost
  copula = "N", # Gaussian copula
  model = "B" # bivariate joint model
)

summary(fit_cop)
AIC(fit_cop)

theta_hat <- summary(fit_cop)$theta # copula parameter
tau_hat <- 2 / pi * asin(theta_hat) # Kendall's tau for Gaussian copula
tau_hat

new_patient <- meps_pos[1, , drop = FALSE]

copula.prob(
  x = fit_cop,
  y1 = 0,
  y2 = 200,
  newdata = new_patient,
  joint = TRUE # TRUE = use copula; FALSE = product of marginals
)

## Under the copula model, expected expenditure rises more steeply with visit
## counts than under an independence assumption, reflecting the positive
## residual dependence captured by the copula.

emp_cop_df <- meps_pos %>%
  mutate(
    u1 = rank(dvisit) / (n() + 1),
    u2 = rank(dvexpend) / (n() + 1)
  )

# # Looking at comparison to previous approach
# fit_ind <- gjrm(
#   formula = form_list,
#   data = meps_pos,
#   margins = c("NBII", "GA"),
#   copula = "I", # independence
#   model = "B"
# )
#
# AIC(fit_ind, fit_cop)
#

m_count_pos <- glm.nb(
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
  data = meps_pos
)

# Marginal cost model on positive spenders
m_part2_pos <- glm(
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

AIC_ind_proxy <- AIC(m_count_pos) + AIC(m_part2_pos)
AIC_cop <- AIC(fit_cop)

AIC_ind_proxy
AIC_cop
AIC_ind_proxy - AIC_cop

# boxplot
meps_results <- meps_results %>%
  mutate(
    cost_group = cut(
      actual_cost,
      breaks = c(-Inf, 0, 100, 1000, 5000, 10000, 20000, 40000, Inf),
      labels = c(
        "Zero",
        "$1–100",
        "$100–1k",
        "$1k–5k",
        "$5k–10k",
        "$10k–20k",
        "$20k–40k",
        ">$40k"
      )
    )
  )

# Plot the errors for each group
ggplot(meps_results, aes(x = cost_group, y = error)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    # title = "Distribution of health care expendature prediction errors",
    # subtitle = "Box shows the IQR (Middle 50% of errors)",
    x = "Expenditure Category",
    y = "Prediction Error ($)"
  ) +
  theme_minimal()
# +
# coord_cartesian(ylim = c(-2000, 2000)) # Zoom in to ignore extreme outliers

#---------------- Expendature two part table (semi-automated) -----------------#
# qol function for this automated table
format_p_vec <- function(x) {
  x_num <- as.numeric(x)
  out <- rep("", length(x_num))
  non_na <- !is.na(x_num)
  vals <- x_num[non_na]
  out[non_na] <- ifelse(vals < 0.001, "<0.001", sprintf("%.3f", vals))
  out
}

# tidy & join
pt1 <- broom::tidy(m_part1) %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  dplyr::rename(pt1_est = estimate, pt1_se = std.error, pt1_p = p.value)

pt2 <- broom::tidy(m_part2) %>%
  dplyr::select(term, estimate, std.error, p.value) %>%
  dplyr::rename(pt2_est = estimate, pt2_se = std.error, pt2_p = p.value)

raw_data <- full_join(pt1, pt2, by = "term")

#  create groups and clean labels

clean_data <- raw_data %>%
  mutate(
    # define gropu names
    var_group = case_when(
      term == "(Intercept)" ~ "Intercept",

      # --- Continuous / Non-Categorical Variables ---
      # grouped together to be sorted, but hide header
      str_starts(term, "age") ~ "Non_Categorical",
      str_starts(term, "bmi") ~ "Non_Categorical",
      str_starts(term, "income") ~ "Non_Categorical",
      str_starts(term, "ndvisit") ~ "Non_Categorical",

      # --- Categorical Variables (w. explicit reference headers) ---
      str_starts(term, "education") ~ "Education (ref: Less than High School)",
      str_starts(term, "ethnicity") ~ "Ethnicity (ref: White)",
      str_starts(term, "gender") ~ "Gender (ref: Female)",
      str_starts(term, "general") ~ "General Health (ref: Excellent)",
      str_starts(term, "hyperlipidemia") ~ "Hyperlipidemia",
      str_starts(term, "hypertension") ~ "Hypertension",
      str_starts(term, "mental") ~ "Mental Health (ref: Excellent)",
      str_starts(term, "region") ~ "Region (ref: Northeast)",
      TRUE ~ "Other"
    ),

    # define row labs
    label = case_when(
      term == "(Intercept)" ~ "(Intercept)",

      # continuous
      str_starts(term, "age") ~ "age",
      str_starts(term, "bmi") ~ "bmi",
      str_starts(term, "income") ~ "income",
      str_starts(term, "ndvisit") ~ "ndvisit",

      # categorical
      # striping 'education_cat_detailed' to reduce table width
      str_starts(term, "education_cat_detailed") ~ str_remove(
        term,
        "education_cat_detailed"
      ),
      str_starts(term, "ethnicity") ~ str_remove(term, "ethnicity"),
      str_starts(term, "gender") ~ str_remove(term, "gender"),
      str_starts(term, "general") ~ str_remove(term, "general"),
      str_starts(term, "hyperlipidemia") ~ str_remove(term, "hyperlipidemia"),
      str_starts(term, "hypertension") ~ str_remove(term, "hypertension"),
      str_starts(term, "mental") ~ str_remove(term, "mental"),
      str_starts(term, "region") ~ str_remove(term, "region"),
      TRUE ~ term
    ),

    # custom order for rows
    label_rank = case_when(
      # Education Order
      label == "High School Graduate" ~ 1,
      label == "Some College" ~ 2,
      label == "College Graduate" ~ 3,
      label == "Post-Graduate" ~ 4,

      # Health Order
      label == "Poor" ~ 1,
      label == "Fair" ~ 2,
      label == "Good" ~ 3,
      label == "VGood" ~ 4,

      TRUE ~ 10
    )
  )

# more group ordering tweaking

all_groups <- unique(clean_data$var_group)

# !note: must match the new text strings exactly for sorting to work
top_groups <- c(
  "Intercept",
  "Non_Categorical",
  "General Health (ref: Excellent)",
  "Mental Health (ref: Excellent)"
)

# combine top groups & add rest (alphabetical)
final_group_order <- c(
  top_groups,
  sort(setdiff(all_groups, top_groups))
)

sorted_data <- clean_data %>%
  # var_group into a factor with our custom order
  mutate(var_group = factor(var_group, levels = final_group_order)) %>%
  # arrange by group, then by custom rank, then alphabetically
  arrange(var_group, label_rank, label) %>%
  # format
  mutate(
    pt1_est = ifelse(is.na(pt1_est), "", sprintf("%.3f", pt1_est)),
    pt1_se = ifelse(is.na(pt1_se), "", sprintf("%.3f", pt1_se)),
    pt2_est = ifelse(is.na(pt2_est), "", sprintf("%.3f", pt2_est)),
    pt2_se = ifelse(is.na(pt2_se), "", sprintf("%.3f", pt2_se)),
    pt1_p = format_p_vec(pt1_p),
    pt2_p = format_p_vec(pt2_p)
  )

# Kable

group_indices <- sorted_data %>%
  mutate(row = row_number()) %>%
  group_by(var_group) %>%
  summarise(start = min(row), end = max(row)) %>%
  arrange(start)

kable_data <- sorted_data %>%
  dplyr::select(label, pt1_est, pt1_se, pt1_p, pt2_est, pt2_se, pt2_p)

colnames(kable_data) <- c(
  "Term",
  "Estimate",
  "Std. Error",
  "p-value",
  "Estimate ",
  "Std. Error ",
  "p-value "
)

header_labels <- c(
  " " = 1,
  "Pt 1: Prob. of any expense" = 3,
  "Pt 2: Expenditure amount" = 3
)

beta_symbol <- ifelse(knitr::is_latex_output(), "$\\hat{\\beta}$", "&beta;")

# caption_text <- paste0(
#   "Estimated coefficients (", beta_symbol, "), standard errors, and p-values. ",
#   format_var_name("dvexpend"),
#   "A Probit and Gamma GLM (log link) were used in combination to model the
#   probability of incurring any expense (Pt 1) and the expenditure among health
#   care users (Pt 2). ",
#   "Variable selection was performed independently for Pt 1 and Pt 2."
# )
caption_text <- paste0(
  "Estimated coefficients (",
  beta_symbol,
  "), standard errors, and p-values ",
  "from the two-part model for doctor-visit expenditure (",
  format_var_name("dvexpend"),
  "), including a Probit model for the probability ",
  "of any expenditure (Part 1) and a Gamma GLM with log link for positive ",
  "expenditure (Part 2)."
)

if (knitr::is_latex_output()) {
  cat("\\clearpage\n")
  cat("\\begingroup\n")
  cat("\\renewcommand{\\arraystretch}{1.2}\n")
}

coeffs_expend <- knitr::kable(
  kable_data,
  format = ifelse(knitr::is_latex_output(), "latex", "html"),
  booktabs = knitr::is_latex_output(),
  caption = caption_text,
  align = c("l", rep("r", ncol(kable_data) - 1)),
  linesep = "",
  escape = !knitr::is_latex_output(),
  table.placement = "H"
) %>%
  add_header_above(header_labels, escape = !knitr::is_latex_output()) %>%
  kable_styling(full_width = FALSE, position = "center")

for (i in 1:nrow(group_indices)) {
  group_name <- as.character(group_indices$var_group[i])
  if (group_name %in% c("Intercept", "Non_Categorical")) {
    next
  }
  coeffs_expend <- coeffs_expend %>%
    pack_rows(group_name, group_indices$start[i], group_indices$end[i])
}

coeffs_expend

if (knitr::is_latex_output()) {
  cat("\\endgroup\n")
  cat("\\clearpage\n")
}

# --- 1. Tidy Data (Single Model) ---------------------------------------------
# We only have one model, so no joining required.
raw_data <- broom::tidy(m_count) %>%
  dplyr::select(term, estimate, std.error, p.value)

# --- 2. Create Groups and Clean Labels ---------------------------------------
# (Logic identical to previous table for consistency)

clean_data <- raw_data %>%
  mutate(
    # A. Define Group Names
    var_group = case_when(
      term == "(Intercept)" ~ "Intercept",

      # --- Continuous / Non-Categorical Variables ---
      str_starts(term, "age") ~ "Non_Categorical",
      str_starts(term, "bmi") ~ "Non_Categorical",
      str_starts(term, "income") ~ "Non_Categorical",
      str_starts(term, "ndvisit") ~ "Non_Categorical",

      # --- Categorical Variables ---
      str_starts(term, "education") ~ "Education (ref: Less than High School)",
      str_starts(term, "ethnicity") ~ "Ethnicity (ref: White)",
      str_starts(term, "gender") ~ "Gender (ref: Female)",
      str_starts(term, "general") ~ "General Health (ref: Excellent)",
      str_starts(term, "hyperlipidemia") ~ "Hyperlipidemia",
      str_starts(term, "hypertension") ~ "Hypertension",
      str_starts(term, "mental") ~ "Mental Health (ref: Excellent)",
      str_starts(term, "region") ~ "Region (ref: Northeast)",
      TRUE ~ "Other"
    ),

    # B. Define Row Labels
    label = case_when(
      term == "(Intercept)" ~ "(Intercept)",

      # Continuous
      str_starts(term, "age") ~ "age",
      str_starts(term, "bmi") ~ "bmi",
      str_starts(term, "income") ~ "income",
      str_starts(term, "ndvisit") ~ "ndvisit",

      # Categorical (Strip prefixes)
      str_starts(term, "education_cat_detailed") ~ str_remove(
        term,
        "education_cat_detailed"
      ),
      str_starts(term, "ethnicity") ~ str_remove(term, "ethnicity"),
      str_starts(term, "gender") ~ str_remove(term, "gender"),
      str_starts(term, "general") ~ str_remove(term, "general"),
      str_starts(term, "hyperlipidemia") ~ str_remove(term, "hyperlipidemia"),
      str_starts(term, "hypertension") ~ str_remove(term, "hypertension"),
      str_starts(term, "mental") ~ str_remove(term, "mental"),
      str_starts(term, "region") ~ str_remove(term, "region"),
      TRUE ~ term
    ),

    # C. Custom Rank for Sorting
    label_rank = case_when(
      # Education
      label == "High School Graduate" ~ 1,
      label == "Some College" ~ 2,
      label == "College Graduate" ~ 3,
      label == "Post-Graduate" ~ 4,
      # Health
      label == "Poor" ~ 1,
      label == "Fair" ~ 2,
      label == "Good" ~ 3,
      label == "VGood" ~ 4,

      TRUE ~ 10
    )
  )

# --- 3. Sorting --------------------------------------------------------------

all_groups <- unique(clean_data$var_group)

# Top priority groups
top_groups <- c(
  "Intercept",
  "Non_Categorical",
  "General Health (ref: Excellent)",
  "Mental Health (ref: Excellent)"
)

final_group_order <- c(
  top_groups,
  sort(setdiff(all_groups, top_groups))
)

sorted_data <- clean_data %>%
  mutate(var_group = factor(var_group, levels = final_group_order)) %>%
  arrange(var_group, label_rank, label) %>%
  mutate(
    estimate = ifelse(is.na(estimate), "", sprintf("%.3f", estimate)),
    std.error = ifelse(is.na(std.error), "", sprintf("%.3f", std.error)),
    p.value = format_p_vec(p.value)
  )

# --- 4. Kable Prep -----------------------------------------------------------

group_indices <- sorted_data %>%
  mutate(row = row_number()) %>%
  group_by(var_group) %>%
  summarise(start = min(row), end = max(row)) %>%
  arrange(start)

kable_data <- sorted_data %>%
  dplyr::select(label, estimate, std.error, p.value)

colnames(kable_data) <- c(
  "Term",
  "Estimate",
  "Std. Error",
  "p-value"
)

# --- 5. Render ---------------------------------------------------------------

beta_symbol <- ifelse(knitr::is_latex_output(), "$\\hat{\\beta}$", "&beta;")

# caption_text <- paste0(
#   "Estimated coefficients (", beta_symbol, "), standard errors, and p-values ",
#   "for a Negative Binomial model for the number of doctor visits (",
#   format_var_name("dvexpend"),"). ",
#   "Negative Binomial Regression Results, including",
#   "The model predicts the number of visits based on health status, ",
#   "demographics, and socioeconomic factors."
# )
caption_text <- paste0(
  "Estimated coefficients (",
  beta_symbol,
  "), standard errors, and p-values ",
  "from a Negative Binomial regression model for the number of doctor visits (",
  format_var_name("dvisit"),
  ")."
)


if (knitr::is_latex_output()) {
  cat("\\clearpage\n")
  cat("\\begingroup\n")
  cat("\\renewcommand{\\arraystretch}{1.2}\n")
}

# Create Kable Object
kbl_count <- knitr::kable(
  kable_data,
  format = ifelse(knitr::is_latex_output(), "latex", "html"),
  booktabs = knitr::is_latex_output(),
  caption = caption_text,
  align = c("l", "r", "r", "r"), # 4 columns
  linesep = "",
  escape = !knitr::is_latex_output(),
  table.placement = "H"
) %>%
  kable_styling(full_width = FALSE, position = "center")

# gropuing
for (i in 1:nrow(group_indices)) {
  group_name <- as.character(group_indices$var_group[i])

  if (group_name %in% c("Intercept", "Non_Categorical")) {
    next
  }

  kbl_count <- kbl_count %>%
    pack_rows(group_name, group_indices$start[i], group_indices$end[i])
}

kbl_count

if (knitr::is_latex_output()) {
  cat("\\endgroup\n")
  cat("\\clearpage\n")
}

ggplot(meps_pos, aes(x = dvisit, y = dvexpend)) +
  geom_point(alpha = 0.2) +
  scale_y_log10() +
  # labs(
  #   title = "Doctor-visit expenditure versus utilisation (positive spenders)",
  #   subtitle = paste0(
  #     "Gaussian copula Kendall's tau \u2248 ",
  #     round(tau_hat, 2)
  #   ),
  labs(
    x = "Number of doctor visits",
    y = "Expenditure (log scale)"
  ) +
  theme_minimal()

# Figure Y. Doctor-visit expenditure versus utilisation among positive spenders. Expenditure is shown on a log scale to emphasise the wide dispersion in costs. The increasing trend with dvisit and substantial residual spread are consistent with moderate-to-strong frequency–severity dependence (Gaussian copula Kendall’s τ≈0.52).
# The scatterplot of dvexpend against dvisit on a log scale shows an overall positive association between utilisation and expenditure, with substantial heterogeneity in spending at each visit count. High-cost cases occurring at modest visit numbers indicate that cost intensity varies markedly across individuals, reinforcing the value of modelling frequency and severity jointly.

ggplot(meps_results, aes(x = final_prediction, y = actual_cost)) +
  geom_point(alpha = 0.3, color = "blue") + # Semi-transparent dots
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") + # Ideally where points should fall
  labs(
    title = "Actual vs. Predicted Expenditure",
    x = "Predicted Cost ($)",
    y = "Actual Cost ($)"
  ) +
  theme_minimal()
# +
#   coord_cartesian(xlim = c(0, 10000), ylim = c(0, 10000))

# Create a 2x2 plotting area
par(mfrow = c(2, 2))
plot(m_part2, which = 1:4) # Plot standard GLM diagnostics
par(mfrow = c(1, 1)) # Reset plotting area


gamma_diag <- data.frame(
  fitted = fitted(m_part2, type = "response"),
  resid = residuals(m_part2, type = "deviance")
)

ggplot(gamma_diag, aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  labs(
    x = "Fitted expenditure ($)",
    y = "Deviance residual"
    # title = "Gamma GLM: deviance residuals vs fitted"
  ) +
  theme_minimal()

par(mfrow = c(2, 2))
plot(m_count, which = 1:4)
par(mfrow = c(1, 1))

## Negative Binomial diagnostics data
nb_diag <- data.frame(
  fitted = fitted(m_count, type = "response"),
  resid = residuals(m_count, type = "deviance")
)

ggplot(nb_diag, aes(x = fitted, y = resid)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  labs(
    x = "Fitted number of visits",
    y = "Deviance residual"
    # title = "Negative Binomial: deviance residuals vs fitted"
  ) +
  theme_minimal()
