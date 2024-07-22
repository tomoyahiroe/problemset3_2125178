main <- function() {
  # packages
  # library(ggplot2)
  # library(dplyr)
  # library(tidyr)
  # library(purrr)
  # library(estimatr)
  # library(kableExtra)

  # set seed num (学籍番号)
  set.seed()

  df_simulation <- generate_simulate_df()

  regression_models <- write_regression_models()

  output_regs <- run_regression(df_simulation, regression_models)

  output_regs
}


generate_simulate_df <- function() {
  # constants
  household_n <- 500
  children_n <- 2 * household_n

  log_income_parent_mean <- 5
  log_income_parent_sd <- sqrt(2)

  alpha_0 <- 1
  alpha_1 <- 0.2
  effort_disturbance <- 1

  beta_0 <- 0.7
  beta_1 <- 1
  beta_2 <- 0.15

  income_disturbance_household <- 5
  income_disturbance_child <- 0.5

  income_parent_measurementerror <- 1
  income_child_measurementerror <- 1
  effort_measurementerror <- 1

  # parent data
  df_parent <- dplyr::tibble(
    # household_id
    household_id = 1:household_n,
    # income parents
    log_income_parent = rnorm(household_n, log_income_parent_mean, log_income_parent_sd),
    income_error_household = rnorm(household_n, 0, income_disturbance_household)
  ) |>
    mutate(
      log_income_parent_noisy = log_income_parent + rnorm(household_n, 0, income_parent_measurementerror)
    )

  df_parent <- df_parent |>
    dplyr::bind_rows(df_parent)

  # effort data
  list_effort_error <- rnorm(children_n, 0, effort_disturbance)
  list_effort_noisy <- rnorm(children_n, 0, effort_measurementerror)

  #  child data
  list_child_income_error <- rnorm(children_n, 0, income_disturbance_child)
  list_child_income_noisy <- rnorm(children_n, 0, income_child_measurementerror)

  # combine data
  df_simulation <- df_parent |>
    mutate(
      # effort
      effort = alpha_0 + alpha_1 * log_income_parent + list_effort_error,
      effort_noisy = effort + list_effort_noisy,

      # income child
      log_income_children = beta_0 + beta_1 * effort + beta_2 * log_income_parent + list_child_income_error + income_error_household,
      log_income_children_noisy = log_income_children + list_child_income_noisy,

      # calculate real income
      income_child = 10^log_income_children,
      income_child_noisy = 10^log_income_children_noisy,
      income_parent = 10^log_income_parent,
      income_parent_noisy = 10^log_income_parent_noisy,

      #  sibling_id
      sibling_id = rep(1:2, each = household_n)
    ) |>
    select(
      household_id,
      sibling_id,
      income_child,
      income_child_noisy,
      effort,
      effort_noisy,
      income_parent,
      income_parent_noisy
    )

  return(df_simulation)
}


write_regression_models <- function() {
  regression_models <- list(
    # correct model
    "(1)" = log10(income_child) ~ effort + log10(income_parent),

    # ommited variables models
    # model 2
    "(2)" = log10(income_child) ~ log10(income_parent),
    # model 3
    "(3)" = log10(income_child) ~ effort,

    # measurementerror models
    # model 4
    "(4)" = log10(income_child) ~ effort + log10(income_parent),
    # model 5
    "(5)" = log10(income_child) ~ effort + log10(income_parent),
    # model 6
    "(6)" = log10(income_child) ~ effort + log10(income_parent)
  )

  return(regression_models)
}


run_regression <- function(df_simulation, regression_models) {
  # run regressions
  full_regs <- map(
    regression_models,
    ~ lm_robust(.x,
      clusters = household_id,
      se_type = "stata",
      data = df_simulation
    )
  )

  # output regressions with a format from msummary
  measurementerror_varnames <- c(
    "effort" = "Own effort",
    "log10(income_parent)" = "Log Parents Income",
    "effort_noisy" = "Own effort with error",
    "log10(income_parent_noisy)" = "Log Parents Income with error",
    "(Intercept)" = "Constant"
  )

  table_output <- modelsummary::msummary(
    full_regs,
    gof_omit = "R2|AIC|BIC|se_type",
    fmt = "%.2f",
    output = "kableExtra",
    coef_map = measurementerror_varnames,
    title = "Regression Table",
    notes = "Heteroskedasticity robust standard errors clustered at household level"
  )

  return(table_output)
}


main()

