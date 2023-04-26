#' Test Significance Based on Randomization Theory
#'
#' @inheritParams summarize_data
#' @param col_type,by_type Optional string specifying data type ("int", "cat", "character", "factor", "numeric", "integer"). Can be obtained from data lookup.
#' @param reps Integer, number of permutations.
#' @param hide_test_if_n_below Integer, if N is below this value, pvalue will not be shown.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' library(dplyr)
#' bind_rows(
#' sigtest(data=ex_survey1, cols = a_1) # t-test of proportions (.5): p=.98
#' sigtest(data=ex_survey1, cols = b_1), # Chi-square (all=.333): p=<.001
#' sigtest(data=ex_survey1, cols = c_1), # one-sample t-test p<.001
#' sigtest(data=ex_survey1, cols = b_1, by = x1_sex), # Chi-square: p = .548
#' sigtest(data=ex_survey1, cols = b_1, by = f_uni), # Chi-square: p = .102
#' sigtest(data=ex_survey1, cols = c_1, by = x1_sex), # two-sample t-test: p = .97
#' sigtest(data=ex_survey1, cols = c_1, by = f_uni), # ANOVA/F-test: p = .22
#' sigtest(data=ex_survey1, cols = c_1, by = c_2)) # correlation: p = .976
#'
#' sigtest(data=ex_survey1, cols = a_1, by = c_1) # NA
#' sigtest(data=ex_survey1, cols = b_1, by = c_1) # NA
#'}
#'
sigtest <-
  function(data,
           cols,
           ...,
           by = NULL,
           col_type = NULL,
           by_type = NULL,
           reps = 1000,
           digits = 1,
           hide_test_if_n_below = 10,
           label_separator = NULL,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)
    if(length(cols_pos) != 1L) cli::cli_abort("{.arg {cols_pos}} must be a single column.")
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data, error_call = call)
    if(length(by_pos) > 1L) cli::cli_abort("{.arg by} must be at most a single column.")

    data <-
      data %>%
      dplyr::filter(dplyr::if_all(.cols=c({{cols}}, {{by}}), .fns = ~!is.na(.x)))


    if(is.null(col_type)) {
      col_type <- class(data[[cols_pos]])
    } else if(col_type == "int" && class(data[[cols_pos]]) %in% c("factor")) {
      data[[cols_pos]] <- as.numeric(data[[cols_pos]])
    } else if(col_type == "cat" && class(data[[cols_pos]]) %in% c("integer", "numeric")) {
      data[[cols_pos]] <- as.factor(data[[cols_pos]])
    }
    col_n <- dplyr::n_distinct(data[[cols_pos]], na.rm = TRUE)

    if(length(by_pos) > 0) {
      if(is.null(by_type)) {
        by_type <- class(data[[by_pos]])
      }
      by_n <- dplyr::n_distinct(data[[by_pos]], na.rm = TRUE)
    }


    stat_test <-
      dplyr::case_when(
        col_type %in% c("numeric", "integer", "int") && col_n > 2 && length(by_pos) == 0 ~ "mean",

        col_n == 2 && length(by_pos) == 0 ~ "prop",

        col_type %in% c("cat", "factor", "character") && col_n > 2 && length(by_pos) == 0 ~ "chisq",

        col_type %in% c("numeric", "integer", "int") && length(by_pos) == 1 &&
          by_type %in% c("numeric", "integer", "int") ~ "correlation",

        col_type %in% c("numeric", "integer", "int") && length(by_pos) == 1 &&
          by_type %in% c("cat", "factor", "character") && by_n > 2 ~ "F",

        col_type %in% c("numeric", "integer", "int") && length(by_pos) == 1 &&
          by_type %in% c("cat", "factor", "character") && by_n == 2 ~ "t",

        col_type %in% c("cat", "factor", "character") && length(by_pos) == 1 &&
          by_type %in% c("cat", "factor", "character") ~ "chisq",

        .default = "NA"
      )
    if(stat_test == "NA") {
      cli::cli_abort("Statistical test not found for {.arg {col_type}} (n_unique={.var {col_n}}) and {.arg {by_type}}.")
    }

    lvls <- unique(data[[cols_pos]]) %>% as.character()
    success <- if(stat_test %in% c("prop", "chisq") && col_n == 2) lvls[length(lvls)]
    p_lvls <- if(stat_test %in% c("prop", "chisq") && length(by_pos) == 0) rlang::set_names(rep(1/length(lvls), length(lvls)), nm=lvls)
    m_lvls <- if(stat_test %in% c("mean")) 0
    order_lvls <- if(stat_test %in% c("prop", "chisq", "t") &&
                     # col_type %in% c("numeric", "integer", "int") &&
                     length(by_pos) == 1 &&
                     by_n == 2) unique(data[[by_pos]]) %>% as.character()
    generate_type <- if(length(by_pos) == 0 && stat_test %in% c("chisq", "prop")) "draw" else "bootstrap"

    estimate <-
      data %>%
      infer::specify(response = {{cols}},
                     explanatory = {{by}},
                     success = success) %>%
      infer::calculate(stat = stat_test, order = order_lvls) %>%
      suppressWarnings()

    null_dist <-
      data %>%
      infer::specify(response = {{cols}},
                     explanatory = {{by}},
                     success = success) %>%
      infer::hypothesize(null = if(length(by_pos)==0) "point" else "independence",
                         p = p_lvls,
                         mu = m_lvls) %>%
      infer::generate(reps = reps, type = generate_type) %>%
      infer::calculate(stat = stat_test, order = order_lvls) %>%
      suppressWarnings()

    pval <-
      null_dist %>%
      infer::get_p_value(obs_stat = estimate, direction = "two-sided") %>%
      suppressWarnings()
    pval <- pval$p_value
    N <- nrow(data)

    tibble::tibble(.variable_label =
                     get_raw_labels(data = data, cols_pos = cols_pos) %>%
                     keep_subitem(label_separator = label_separator) %>%
                     as.character(),
                   by_name = if(length(by_pos)>0) names(by_pos),
                   N = N,
                   test = stat_test,
                   stat = round(estimate[["stat"]], digits = digits),
                   p = dplyr::if_else(pval > 0, pval, 3/reps)) %>%
      dplyr::mutate(p = dplyr::if_else(.data$N >= hide_test_if_n_below, .data$p, NA_real_))
  }


#' Title
#'
#' @inheritParams sigtest
#' @inheritParams embed_cat_prop_plot_html
#'
#' @return Data frame
#' @export
#'
#' @examples
#' embed_uni_sigtest(data=ex_survey1, cols = a_1:a_9)
#' embed_uni_sigtest(data=ex_survey1, cols = b_1:b_3, label_separator=" - ")
#' embed_uni_sigtest(data=ex_survey1, cols = c_1:c_2)
embed_uni_sigtest <-
  function(data,
           cols,
           ...,
           by = NULL,
           col_type = NULL,
           by_type = NULL,
           reps = 1000,
           digits = 1,
           hide_test_if_n_below = 10,
           label_separator = NULL,
           return_raw = TRUE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    check_data_frame(data)

    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data, error_call = call)
    if(length(by_pos) > 1L) cli::cli_abort("{.arg by} must be at most a single column.")

    data <-
      data %>%
      dplyr::filter(dplyr::if_all(.cols=c(cols_pos, by_pos), ~!is.na(.x)))

    main_question <-
      get_raw_labels(data = data, cols_pos = cols_pos) %>%
      get_main_question2(label_separator = label_separator, warn_multiple = TRUE, call = call) %>%
      stringr::str_unique()
    main_question <-
      if(!is.null(label_separator) &&
         length(main_question)==1) main_question else ".variable_label"


    out <-
      cols_pos %>%
      names() %>%
      purrr::map(.f = ~{
        col_sym <- rlang::sym(.x)

        rlang::inject(
        sigtest(
          data = data,
          cols = !!col_sym,
          # by = !!by_pos,
          col_type = col_type,
          by_type = by_type,
          label_separator = label_separator,
          reps = reps,
          digits = digits,
          call = call,
          !!!dots))
      }) %>%
      dplyr::bind_rows()
    names(out)[names(out) == ".variable_label"] <- main_question

    if(return_raw) out else reactable::reactable(out, sortable = TRUE)

  }



#' Title
#'
#' @inheritParams sigtest
#' @inheritParams embed_cat_prop_plot_html
#'
#' @return Data frame
#' @export
#'
#' @examples
#' embed_bi_sigtest(data=ex_survey1, cols = a_1:a_9, by = x1_sex)
#' embed_bi_sigtest(data=ex_survey1, cols = b_1:b_3, by = x1_sex, label_separator=" - ")
#' embed_bi_sigtest(data=ex_survey1, cols = c_1:c_2, by = x1_sex)
embed_bi_sigtest <-
  function(data,
           cols,
           by,
           ...,
           col_type = NULL,
           by_type = NULL,
           reps = 1000,
           digits = 1,
           hide_test_if_n_below = 10,
           label_separator = NULL,
           return_raw = TRUE,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)
    check_data_frame(data)

    cols_enq <- rlang::enquo(arg = cols)
    cols_pos <- tidyselect::eval_select(cols_enq, data = data, error_call = call)
    by_enq <- rlang::enquo(arg = by)
    by_pos <- tidyselect::eval_select(by_enq, data = data, error_call = call)
    if(length(by_pos) > 1L) cli::cli_abort("{.arg by} must be at most a single column.")

    data <-
      data %>%
      dplyr::filter(dplyr::if_all(.cols=c(cols_pos, by_pos), ~!is.na(.x)))

    main_question <-
      get_raw_labels(data = data, cols_pos = cols_pos) %>%
      get_main_question2(label_separator = label_separator, warn_multiple = TRUE, call = call) %>%
      stringr::str_unique()
    main_question <-
      if(!is.null(label_separator) &&
         length(main_question)==1) main_question else ".variable_label"

    by_sym <- rlang::sym(names(by_pos))

    out <-
      cols_pos %>%
      names() %>%
      purrr::map(.f = ~{
        col_sym <- rlang::sym(.x)


          sigtest(
            data = data,
            cols = !!col_sym,
            by = !!by_sym,
            col_type = col_type,
            by_type = by_type,
            label_separator = label_separator,
            reps = reps,
            digits = digits,
            call = call,
            !!!dots)
      }) %>%
      dplyr::bind_rows()
    names(out)[names(out) == ".variable_label"] <- main_question

    if(return_raw) out else reactable::reactable(out, sortable = TRUE)

  }
