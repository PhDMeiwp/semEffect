test_that("lavaan example works correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  # Setup model and data
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
  # Run sem_effects
  results <- sem_effects(fit, target = "dem65")

  # Test structure of results

  expect_named(results, c("effect_table", "effect_long", "plot_object"))

  # Test effect table
  expect_s3_class(results$effect_table, "data.frame")
  expect_equal(names(results$effect_table),
               c("Variable", "Direct_Effect", "Indirect_Effect", "Total_Effect"))
  expect_true(all(c("ind60", "dem60") %in% results$effect_table$Variable))

  # Test long format
  expect_s3_class(results$effect_long, "data.frame")
  expect_equal(names(results$effect_long),
               c("Variable", "Effect_Type", "Effect_Value"))

  # Test plot object
  expect_s3_class(results$plot_object, "ggplot")

  # Test effect values
  ind60_total <- results$effect_table$Total_Effect[results$effect_table$Variable == "ind60"]
  dem60_direct <- results$effect_table$Direct_Effect[results$effect_table$Variable == "dem60"]

})

test_that("piecewiseSEM example works correctly", {
  skip_if_not_installed("piecewiseSEM")
  library(piecewiseSEM)
  data(keeley)

  # Setup model
  pmod <- psem(
    lm(rich ~ cover, data = keeley),
    lm(cover ~ firesev, data = keeley),
    lm(firesev ~ age, data = keeley),
    data = keeley
  )

  # Run sem_effects
  results <- sem_effects(pmod, target = "rich",
                         color_palette = c("darkgreen", "grey80", "purple"))

  # Test structure of results

  expect_named(results, c("effect_table", "effect_long", "plot_object"))

  # Test effect table
  expect_true("age" %in% results$effect_table$Variable)
  expect_true("cover" %in% results$effect_table$Variable)

  # Test plot object
  expect_s3_class(results$plot_object, "ggplot")

  # Test effect values
  age_total <- results$effect_table$Total_Effect[results$effect_table$Variable == "age"]
  cover_direct <- results$effect_table$Direct_Effect[results$effect_table$Variable == "cover"]

})

test_that("plspm example works correctly", {
  skip_if_not_installed("plspm")
  library(plspm)
  data(satisfaction)

  # Setup path matrix
  IMAG = c(0,0,0,0,0,0)
  EXPE = c(1,0,0,0,0,0)
  QUAL = c(0,1,0,0,0,0)
  VAL = c(0,1,1,0,0,0)
  SAT = c(1,1,1,1,0,0)
  LOY = c(1,0,0,0,1,0)
  sat_path = rbind(IMAG, EXPE, QUAL, VAL, SAT, LOY)

  # Setup outer model blocks
  sat_blocks = list(1:5, 6:10, 11:15, 16:19, 20:23, 24:27)
  sat_mod = rep("A", 6)

  # Apply plspm
  plsmodel = plspm(satisfaction, sat_path, sat_blocks, modes = sat_mod)

  # Run sem_effects
  results <- sem_effects(
    plsmodel,
    target = "LOY",
    total_only = TRUE,
    total_color = RColorBrewer::brewer.pal(5, "Set3")
  )

  # Test structure of results

  expect_named(results, c("effect_table", "effect_long", "plot_object"))

  # Test effect table
  expect_true("IMAG" %in% results$effect_table$Variable)
  expect_true("SAT" %in% results$effect_table$Variable)

  # Test plot object
  expect_s3_class(results$plot_object, "ggplot")

  # Test effect values
  sat_total <- results$effect_table$Total_Effect[results$effect_table$Variable == "SAT"]
  imag_total <- results$effect_table$Total_Effect[results$effect_table$Variable == "IMAG"]

})

test_that("plot customization works correctly", {
  skip_if_not_installed("lavaan")
  library(lavaan)

  # Setup model and data
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
  # Run sem_effects and customize plot
  results <- sem_effects(fit, target = "dem65")
  customized_plot <- results$plot_object +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Customized effects plot")

  # Test customized plot
  expect_s3_class(customized_plot, "ggplot")

  # Extract plot data
  plot_data <- ggplot2::ggplot_build(customized_plot)

  # Verify customization

  expect_equal(customized_plot$labels$title, "Customized effects plot")
  expect_equal(plot_data$plot$theme$legend.position, "right")
})
