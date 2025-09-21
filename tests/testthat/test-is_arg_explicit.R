testthat::test_that("is_arg_explicit detects explicitly passed arguments", {
  test_func <- function(a = 1, b = 2, c = 3) {
    call <- match.call()
    list(
      a_explicit = saros:::is_arg_explicit("a", call),
      b_explicit = saros:::is_arg_explicit("b", call),
      c_explicit = saros:::is_arg_explicit("c", call)
    )
  }

  result <- test_func(a = 10, b = 20)
  testthat::expect_true(result$a_explicit)
  testthat::expect_true(result$b_explicit)
  testthat::expect_false(result$c_explicit)
})

testthat::test_that("is_arg_explicit handles missing arguments", {
  test_func <- function(a = 1, b = 2, c = 3) {
    call <- match.call()
    list(
      a_explicit = saros:::is_arg_explicit("a", call),
      b_explicit = saros:::is_arg_explicit("b", call),
      c_explicit = saros:::is_arg_explicit("c", call)
    )
  }

  result <- test_func()
  testthat::expect_false(result$a_explicit)
  testthat::expect_false(result$b_explicit)
  testthat::expect_false(result$c_explicit)
})

testthat::test_that("is_arg_explicit detects when arguments are missing", {
  test_func <- function(a, b, c) {
    call <- match.call()
    list(
      a_explicit = saros:::is_arg_explicit("a", call),
      b_explicit = saros:::is_arg_explicit("b", call),
      c_explicit = saros:::is_arg_explicit("c", call)
    )
  }

  result <- test_func(a = 10)
  testthat::expect_true(result$a_explicit)
  testthat::expect_false(result$b_explicit)
  testthat::expect_false(result$c_explicit)
})

testthat::test_that("is_arg_explicit works with different types of arguments", {
  test_func <- function(a = 1, b = "default", c = TRUE) {
    call <- match.call()
    list(
      a_explicit = saros:::is_arg_explicit("a", call),
      b_explicit = saros:::is_arg_explicit("b", call),
      c_explicit = saros:::is_arg_explicit("c", call)
    )
  }

  result <- test_func(b = "explicit")
  testthat::expect_false(result$a_explicit)
  testthat::expect_true(result$b_explicit)
  testthat::expect_false(result$c_explicit)
})

testthat::test_that("get_argument_value returns explicitly passed arguments", {
  test_func <- function(a = 1, b = 2, c = 3) {
    call <- match.call()
    defaults_env <- new.env()
    formals_list <- formals(test_func)
    list(
      a_value = saros:::get_argument_value(
        "a",
        call,
        defaults_env,
        formals_list
      ),
      b_value = saros:::get_argument_value(
        "b",
        call,
        defaults_env,
        formals_list
      ),
      c_value = saros:::get_argument_value(
        "c",
        call,
        defaults_env,
        formals_list
      )
    )
  }

  result <- test_func(a = 10, b = 20)
  testthat::expect_equal(result$a_value, 10)
  testthat::expect_equal(result$b_value, 20)
  testthat::expect_equal(result$c_value, 3)
})

testthat::test_that("get_argument_value returns defaults_env values if argument is not explicit", {
  test_func <- function(a = 1, b = 2, c = 3) {
    call <- match.call()
    defaults_env <- list(a = 100, b = 200)
    formals_list <- formals(test_func)
    list(
      a_value = saros:::get_argument_value(
        "a",
        call,
        defaults_env,
        formals_list
      ),
      b_value = saros:::get_argument_value(
        "b",
        call,
        defaults_env,
        formals_list
      ),
      c_value = saros:::get_argument_value(
        "c",
        call,
        defaults_env,
        formals_list
      )
    )
  }

  result <- test_func()
  testthat::expect_equal(result$a_value, 100)
  testthat::expect_equal(result$b_value, 200)
  testthat::expect_equal(result$c_value, 3)
})

testthat::test_that("get_argument_value returns formals_list values if neither explicit nor defaults_env", {
  test_func <- function(a = 1, b = 2, c = 3) {
    call <- match.call()
    defaults_env <- list()
    formals_list <- formals(test_func)
    list(
      a_value = saros:::get_argument_value(
        "a",
        call,
        defaults_env,
        formals_list
      ),
      b_value = saros:::get_argument_value(
        "b",
        call,
        defaults_env,
        formals_list
      ),
      c_value = saros:::get_argument_value(
        "c",
        call,
        defaults_env,
        formals_list
      )
    )
  }

  result <- test_func()
  testthat::expect_equal(result$a_value, 1)
  testthat::expect_equal(result$b_value, 2)
  testthat::expect_equal(result$c_value, 3)
})

testthat::test_that("get_argument_value prioritizes explicit over defaults_env and formals_list", {
  test_func <- function(a = 1, b = 2, c = 3) {
    call <- match.call()
    defaults_env <- list(a = 100, b = 200)
    formals_list <- formals(test_func)
    list(
      a_value = saros:::get_argument_value(
        "a",
        call,
        defaults_env,
        formals_list
      ),
      b_value = saros:::get_argument_value(
        "b",
        call,
        defaults_env,
        formals_list
      ),
      c_value = saros:::get_argument_value(
        "c",
        call,
        defaults_env,
        formals_list
      )
    )
  }

  result <- test_func(a = 10)
  testthat::expect_equal(result$a_value, 10)
  testthat::expect_equal(result$b_value, 200)
  testthat::expect_equal(result$c_value, 3)
})
