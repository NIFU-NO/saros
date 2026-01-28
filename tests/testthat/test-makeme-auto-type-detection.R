test_that("makeme auto-detects int_plot_html for numeric variables", {
  # Test with numeric dependent variables
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "auto",
    label_separator = NULL
  )

  expect_true(ggplot2::is_ggplot(result))
  # Should have violin/boxplot layers for interval plots
  layer_geoms <- vapply(
    result$layers,
    function(l) class(l$geom)[1],
    character(1)
  )
  expect_true(any(grepl("Violin|Boxplot", layer_geoms, ignore.case = TRUE)))
})

test_that("makeme auto-detection detects unsupported types", {
  # Test with Date variable
  date_data <- saros::ex_survey
  date_data$date_var <- as.Date("2026-01-15")

  expect_error(
    saros::makeme(
      data = date_data,
      dep = date_var,
      type = "auto",
      label_separator = NULL
    ),
    regexp = "Unsupported variables"
  )

  # Test with POSIXct variable
  posix_data <- saros::ex_survey
  posix_data$time_var <- as.POSIXct("2026-01-15 10:30:00")

  expect_error(
    saros::makeme(
      data = posix_data,
      dep = time_var,
      type = "auto",
      label_separator = NULL
    ),
    regexp = "Unsupported variables"
  )

  # Test with list variable
  list_data <- saros::ex_survey
  list_data$list_var <- list(1:10)

  expect_error(
    saros::makeme(
      data = list_data,
      dep = list_var,
      type = "auto",
      label_separator = NULL
    ),
    regexp = "Unsupported variables"
  )
})

test_that("makeme auto-detection handles mixed supported and unsupported types", {
  # Test with mix of numeric and Date variables
  mixed_data <- saros::ex_survey
  mixed_data$numeric_var <- mixed_data$c_1
  mixed_data$date_var <- as.Date("2026-01-15")

  # Capture error message once and check all patterns
  error_msg <- tryCatch(
    saros::makeme(
      data = mixed_data,
      dep = c(numeric_var, date_var),
      type = "auto",
      label_separator = NULL
    ),
    error = function(e) conditionMessage(e)
  )

  expect_match(error_msg, "mixed types")
  expect_match(error_msg, "Numeric variables")
  expect_match(error_msg, "Unsupported variables")
})

test_that("makeme auto-detects cat_plot_html for factor variables", {
  # Test with categorical dependent variables
  result <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_2,
    type = "auto",
    label_separator = NULL
  )

  expect_true(ggplot2::is_ggplot(result))
  # Should have col/bar layers for categorical plots
  layer_geoms <- vapply(
    result$layers,
    function(l) class(l$geom)[1],
    character(1)
  )
  expect_true(any(grepl("Col|Bar", layer_geoms, ignore.case = TRUE)))
})

test_that("makeme auto-detection works with type='auto' as default", {
  # Test that auto works as default (first option)
  result_numeric <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result_numeric))

  result_categorical <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_2,
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result_categorical))
})

test_that("makeme auto-detection provides informative error for mixed types", {
  # Create a temporary dataset with mixed types
  mixed_data <- saros::ex_survey
  mixed_data$mixed_numeric <- mixed_data$c_1
  mixed_data$mixed_factor <- mixed_data$b_1

  # Capture error message once and check all patterns
  error_msg <- tryCatch(
    saros::makeme(
      data = mixed_data,
      dep = c(mixed_numeric, mixed_factor),
      type = "auto",
      label_separator = NULL
    ),
    error = function(e) conditionMessage(e)
  )

  expect_match(error_msg, "mixed types")
  expect_match(error_msg, "Numeric variables")
  expect_match(error_msg, "Categorical variables")
})

test_that("makeme auto-detection handles single character variable as chr_table_html", {
  # Single character variable should use chr_table_html
  char_data <- saros::ex_survey
  char_data$char_var <- as.character(char_data$f_uni)

  result <- saros::makeme(
    data = char_data,
    dep = char_var,
    type = "auto",
    label_separator = NULL
  )

  # chr_table_html returns a data frame, not a ggplot
  expect_true(is.data.frame(result))
})

test_that("makeme auto-detection handles multiple character variables as categorical", {
  # Multiple character variables should use cat_plot_html
  char_data <- saros::ex_survey
  char_data$char_var1 <- as.character(char_data$b_1)
  char_data$char_var2 <- as.character(char_data$b_2)

  result <- saros::makeme(
    data = char_data,
    dep = c(char_var1, char_var2),
    type = "auto",
    label_separator = NULL
  )

  expect_true(ggplot2::is_ggplot(result))
  # Should detect as categorical plot
  layer_geoms <- vapply(
    result$layers,
    function(l) class(l$geom)[1],
    character(1)
  )
  expect_true(any(grepl("Col|Bar", layer_geoms, ignore.case = TRUE)))
})

test_that("makeme auto-detection works with independent variables", {
  # Test numeric with independent variable
  result_numeric <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    indep = x1_sex,
    type = "auto",
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result_numeric))

  # Test categorical with independent variable
  result_categorical <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_2,
    indep = x1_sex,
    type = "auto",
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result_categorical))
})

test_that("makeme explicit type still works (backward compatibility)", {
  # Ensure explicit type specification still works
  result_explicit_int <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    type = "int_plot_html",
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result_explicit_int))

  result_explicit_cat <- saros::makeme(
    data = saros::ex_survey,
    dep = b_1:b_2,
    type = "cat_plot_html",
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result_explicit_cat))
})

test_that("makeme auto-detection error message suggests correct types", {
  # Create mixed type data
  mixed_data <- saros::ex_survey
  mixed_data$numeric_var <- mixed_data$c_1
  mixed_data$factor_var <- mixed_data$b_1

  # Capture error message once and check all patterns
  error_msg <- tryCatch(
    saros::makeme(
      data = mixed_data,
      dep = c(numeric_var, factor_var),
      type = "auto",
      label_separator = NULL
    ),
    error = function(e) conditionMessage(e)
  )

  expect_match(error_msg, "int_plot_html.*for numeric")
  expect_match(error_msg, "cat_plot_html.*for categorical")
})

test_that("makeme auto-detection handles ordered factors as categorical", {
  # Test with ordered factor
  ordered_data <- saros::ex_survey
  ordered_data$b_1_ordered <- ordered(ordered_data$b_1)

  result <- saros::makeme(
    data = ordered_data,
    dep = b_1_ordered,
    type = "auto",
    label_separator = NULL
  )

  expect_true(ggplot2::is_ggplot(result))
  # Should detect as categorical
  layer_geoms <- vapply(
    result$layers,
    function(l) class(l$geom)[1],
    character(1)
  )
  expect_true(any(grepl("Col|Bar", layer_geoms, ignore.case = TRUE)))
})

test_that("makeme auto-detection issue #510 - numeric without type parameter", {
  # This is the specific issue reported in #510
  # Previously would give: Error in tapply(X = X, INDEX = x, FUN = FUN, ...) :
  #   arguments must have same length

  # Now it should auto-detect and work correctly
  expect_no_error({
    result <- saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      label_separator = NULL
    )
  })

  result <- saros::makeme(
    data = saros::ex_survey,
    dep = c_1:c_2,
    label_separator = NULL
  )
  expect_true(ggplot2::is_ggplot(result))

  # Verify it's an interval plot
  layer_geoms <- vapply(
    result$layers,
    function(l) class(l$geom)[1],
    character(1)
  )
  expect_true(any(grepl("Violin|Boxplot", layer_geoms, ignore.case = TRUE)))
})
