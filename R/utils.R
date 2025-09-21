#' Given Ordered Integer Vector, Return Requested Set.
#'
#' Useful for identifying which categories are to be collected.
#'
#' @param vec A vector of any type.
#' @param set A character string, one of c(".top", ".upper", ".mid_upper", ".lower",
#'   ".mid_lower", ".bottom")
#' @param spread_n The number of values to extract when set is "spread".
#' @param sort Whether to sort the output, defaults to FALSE.
#' @return Selected set of vector.
#' @keywords internal
# subset_vector(vec=1:7, set=".mid_lower")
subset_vector <-
  function(
    vec,
    set = c(
      ".top",
      ".upper",
      ".mid_upper",
      ".lower",
      ".mid_lower",
      ".bottom",
      ".spread"
    ),
    spread_n = NULL,
    sort = FALSE
  ) {
    set <- rlang::arg_match(set)
    n <- length(vec)
    if (sort) {
      vec <- sort(vec)
    }
    if (n %in% 0:1) {
      vec
    } else if (set == ".top") {
      vec[n]
    } else if (set == ".bottom") {
      vec[1]
    } else if (n %% 2 == 0L & set != ".spread") {
      if (set %in% c(".mid_upper", ".upper")) {
        vec[(n / 2 + 1):n]
      } else if (set %in% c(".mid_lower", ".lower")) {
        vec[1:(n / 2)]
      }
    } else {
      m <- stats::median(seq_len(n))
      if (set == ".upper") {
        vec[(m + 1):n]
      } else if (set == ".lower") {
        vec[1:(m - 1)]
      } else if (set == ".mid_upper") {
        vec[m:n]
      } else if (set == ".mid_lower") {
        vec[1:m]
      } else if (set == ".spread") {
        if (n == spread_n) {
          vec
        } else {
          max_set <- c()
          if (n %% 2 != 0L) {
            if (spread_n == 1L) {
              m
            }
            if (spread_n %% 2 != 0L) {
              max_set <- c(max_set, m)
            }
            if (spread_n > 1L) {
              max_set <- c(max_set, 1L, n)
            }
            if (spread_n > 4L) {
              max_set <- c(max_set, 2L, n - 1L)
            }
            if (spread_n > 5L | spread_n == 4) {
              max_set <- c(max_set, 3L, n - 2L)
            }
            if (spread_n > 6L) {
              max_set <- c(max_set, 4L, n - 3L)
            }
          } else if (n %% 2L == 0L) {
            if (spread_n > 1L) {
              max_set <- c(max_set, 1L, n)
            }
            if (spread_n > 3L & n <= 6) {
              max_set <- c(max_set, 2L, n - 1L)
            }
            if (spread_n > 4L | (spread_n > 3L & n > 6)) {
              max_set <- c(max_set, 3L, n - 2L)
            }
            if (spread_n %% 2 != 0L) {
              m <- round(stats::median(seq_len(n)))
              max_set <- c(max_set, m)
            }
          }
          unique(vec[sort(max_set)])
        }
      }
    }
  }


###  Check that all pairs of cols share at least one observed response category
check_category_pairs <-
  function(data, cols_pos, call = rlang::caller_env(), return_error = TRUE) {
    lapply(X = seq_along(cols_pos), FUN = function(i) {
      x <- unname(cols_pos)[[i]]
      y <- names(cols_pos)[[i]]

      cols_rest <-
        cols_pos[-c(1:match(y, names(cols_pos)))]
      lapply(X = seq_along(cols_rest), FUN = function(e) {
        x2 <- unname(cols_rest)[[e]]
        y2 <- names(cols_rest)[[e]]

        val_y <- if (is.factor(data[[y]])) {
          levels(data[[y]])
        } else {
          unique(data[[y]])
        }
        val_y2 <- if (is.factor(data[[y2]])) {
          levels(data[[y2]])
        } else {
          unique(data[[y2]])
        }
        common <- dplyr::intersect(val_y, val_y2)
        if (length(common) == 0L) {
          cli::cli_abort(
            c(
              "Unequal variables.",
              "!" = "All variables must share at least one common category.",
              "i" = "Column {.var {y}} and column {.var {y2}} lack common categories."
            ),
            call = call
          )
        }
      })
    })
    TRUE
  }
