testthat::test_that("check_options evaluates in immediate parent environment", {
    x <- 10
    y <- "test"

    call <- quote(list(a = x, b = y))
    result <- saros:::check_options(call, default_values = list(a = NA, b = NA))

    testthat::expect_equal(result$a, 10)
    testthat::expect_equal(result$b, "test")
})

testthat::test_that("check_options finds variables in higher stack frames", {
    outer_var <- 100

    nested_fun <- function() {
        inner_var <- 200
        call <- quote(list(a = outer_var, b = inner_var))
        saros:::check_options(call, default_values = list(a = NA, b = NA))
    }

    result <- nested_fun()
    testthat::expect_equal(result$a, 100)
    testthat::expect_equal(result$b, 200)
})

testthat::test_that("check_options handles default values correctly", {
    default_vals <- list(
        x = 1,
        y = "default",
        z = TRUE
    )

    call <- quote(list(x = 5))
    result <- saros:::check_options(call, default_values = default_vals)

    testthat::expect_equal(result$x, 5)
    testthat::expect_equal(result$y, "default")
    testthat::expect_equal(result$z, TRUE)
})

testthat::test_that("check_options handles undefined variables gracefully", {
    fun <- function(a) {
        call <- match.call()
        saros:::check_options(call, default_values = list(a = 999))
    }
    result <- fun()

    testthat::expect_equal(result$a, 999)
})

testthat::test_that("check_options respects ignore_args", {
    x <- 10
    call <- quote(list(ignored = x, keep = x))

    result <- saros:::check_options(
        call,
        default_values = list(ignored = NA, keep = NA),
        ignore_args = "ignored"
    )

    testthat::expect_false("ignored" %in% names(result))
    testthat::expect_true("keep" %in% names(result))
    testthat::expect_equal(result$keep, 10)
})

testthat::test_that("check_options evaluates nested expressions", {
    x <- 5
    y <- 10

    call <- quote(list(a = x + y, b = paste(x, y)))
    result <- saros:::check_options(call, default_values = list(a = NA, b = NA))

    testthat::expect_equal(result$a, 15)
    testthat::expect_equal(result$b, "5 10")
})
