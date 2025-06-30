test_that("lavaan example works correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  '
  suppressWarnings({
    fit <- sem(model, data = PoliticalDemocracy)
  })

  # Standard test
  results <- sem_effects(fit, target = "dem65")
  expect_named(results, c("effect_table", "effect_long", "plot_object"))

  # Test invalid target (Line 150)
  expect_error(
    sem_effects(fit, target = "invalid_target"),
    "Target variable invalid_target not found"
  )

  # Test total_color as vector (Line 299)
  color_results <- sem_effects(
    fit,
    target = "dem65",
    total_only = TRUE,
    total_color = c("red", "blue", "green")
  )
  expect_s3_class(color_results$plot_object, "ggplot")

  # Test unsupported model class (Lines 256-258)
  expect_error(
    sem_effects(list(), target = "test"),
    "Unsupported object class"
  )
})

test_that("piecewiseSEM example works correctly", {
  skip_if_not_installed("piecewiseSEM")
  library(piecewiseSEM)
  data(keeley)

  pmod <- psem(
    lm(rich ~ cover, data = keeley),
    lm(cover ~ firesev, data = keeley),
    lm(firesev ~ age, data = keeley),
    data = keeley
  )

  # Standard test
  results <- sem_effects(pmod, target = "rich")
  expect_true("age" %in% results$effect_table$Variable)

  # Test invalid target (Line 197)
  expect_error(
    sem_effects(pmod, target = "invalid_target"),
    "Target variable invalid_target not found"
  )
})

test_that("plspm example works correctly", {
  skip_if_not_installed("plspm")
  library(plspm)
  data(satisfaction)

  IMAG = c(0,0,0,0,0,0)
  EXPE = c(1,0,0,0,0,0)
  QUAL = c(0,1,0,0,0,0)
  VAL = c(0,1,1,0,0,0)
  SAT = c(1,1,1,1,0,0)
  LOY = c(1,0,0,0,1,0)
  sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)

  sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
  sat_mod = rep("A", 6)

  plsmodel = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod)

  # Standard test
  results <- sem_effects(plsmodel, target = "LOY")
  expect_true("SAT" %in% results$effect_table$Variable)

  # Test invalid target (Line 234)
  expect_error(
    sem_effects(plsmodel, target = "invalid_target"),
    "Target variable invalid_target not found"
  )
})

test_that("zero-effect variables are handled correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  # Model with two final dependent variables
  modelyy <- '
    y7 + y8 ~ y1 + y2 + y3 + y4 + y5 + y6
    y6 ~ y1 + y2 + y3 + y4 + y5
    y5 ~ y1 + y2 + y3 + y4
  '
  fityy <- sem(modelyy, data = PoliticalDemocracy)

  # Test with zero effects kept (Lines 268-272, 279)
  results_keep <- sem_effects(fityy, target = "y8", delete_zero_effect = FALSE)
  expect_true("y7" %in% results_keep$effect_table$Variable)

  # Test warning message (Lines 268-272)
  expect_warning(
    sem_effects(fityy, target = "y8", delete_zero_effect = TRUE),
    "removed from the plot by default"
  )
})

test_that("plot customization works correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  model <- '
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3
    dem65 =~ y5 + y6 + y7
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  '
  suppressWarnings({
    fit <- sem(model, data = PoliticalDemocracy)
  })

  # Test total_only = TRUE with single color
  results_single <- sem_effects(
    fit,
    target = "dem65",
    total_only = TRUE,
    total_color = "red"
  )
  expect_s3_class(results_single$plot_object, "ggplot")

  # Test total_only = TRUE with multiple colors
  results_multi <- sem_effects(
    fit,
    target = "dem65",
    total_only = TRUE,
    total_color = c("red", "blue", "green")
  )
  expect_s3_class(results_multi$plot_object, "ggplot")
})
