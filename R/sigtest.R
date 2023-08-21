
find_stat_config <-
  function(cols_pos,
           col_type,
           col_n,
           cols_unique,
           by_type,
           by_pos,
           by_n,
           by_unique,
           call = rlang::caller_env()) {

    stat_test <-
      dplyr::case_when(
        col_type %in% c("numeric", "integer", "int") && col_n > 2 && length(by_pos) == 0 ~ "mean",

        col_n == 2 && length(by_pos) == 0 ~ "prop",

        col_type %in% c("fct", "factor", "character") && col_n > 2 && length(by_pos) == 0 ~ "chisq",

        col_type %in% c("numeric", "integer", "int") && length(by_pos) == 1 &&
          by_type %in% c("numeric", "integer", "int") && by_n >= 5 ~ "correlation",

        col_type %in% c("numeric", "integer", "int") && length(by_pos) == 1 &&
          by_type %in% c("fct", "factor", "character") && by_n > 2 ~ "F",

        col_type %in% c("numeric", "integer", "int") && length(by_pos) == 1 &&
          by_type %in% c("fct", "factor", "character") && by_n == 2 ~ "t",

        col_type %in% c("fct", "factor", "character") && length(by_pos) == 1 &&
          by_type %in% c("fct", "factor", "character") && by_n >= 2 ~ "chisq",

        .default = "NA"
      )
    if(stat_test == "NA") {
      error_by_str <- if(length(by_pos) == 1) stringr::str_c(" and {.arg {by_type}}")
      cli::cli_warn(stringr::str_c("Statistical test not found for {.arg {col_type}} (n_unique={.var {col_n}})", error_by_str, "."), call = call)
    }

    lvls <- cols_unique %>% as.character()
    success <- if(stat_test %in% c("prop", "chisq") && col_n == 2) lvls[length(lvls)]
    p_lvls <- if(stat_test %in% c("prop", "chisq") && length(by_pos) == 0) stats::setNames(rep(1/length(lvls), length(lvls)), nm=lvls)
    m_lvls <- if(stat_test %in% c("mean")) 0
    order_lvls <- if(stat_test %in% c("prop", "chisq", "t") &&
                     # col_type %in% c("numeric", "integer", "int") &&
                     length(by_pos) == 1 &&
                     by_n == 2) by_unique %>% as.character()
    generate_type <- if(length(by_pos) == 0 && stat_test %in% c("chisq", "prop")) "draw" else "bootstrap"
    null_hypothesis_type <- if(length(by_pos) == 0) "point" else "independence"

    list(test = stat_test,
         lvls = lvls,
         success = success,
         p_lvls = p_lvls,
         m_lvls = m_lvls,
         order_lvls = order_lvls,
         null_hypothesis_type = null_hypothesis_type,
         generate_type = generate_type)
  }



#' Test Significance Based on Randomization Theory
#'
#' @inheritParams summarize_data
#' @param col_type,by_type Optional string specifying data type ("int", "fct", "character", "factor", "numeric", "integer"). Can be obtained from data lookup.
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
      dplyr::filter(dplyr::if_all(.cols=c({{cols}}, {{by}}), .fns = ~!is.na(.x))) %>%
      dplyr::mutate(dplyr::across(c({{cols}}, {{by}}) & tidyselect::where(~is.factor(.x)), ~forcats::fct_drop(.x)))

    number_rows <- nrow(data)

    var_labels <-
      get_raw_labels(data = data, cols_pos = cols_pos) %>%
      keep_subitem(label_separator = label_separator) %>%
      as.character()

    if(is.null(col_type)) {
      col_type <- class(data[[cols_pos]])
    } else if(col_type == "int" && class(data[[cols_pos]]) %in% c("factor")) {
      data[[cols_pos]] <- as.numeric(data[[cols_pos]])
    } else if(col_type == "fct" && class(data[[cols_pos]]) %in% c("integer", "numeric")) {
      data[[cols_pos]] <- as.factor(data[[cols_pos]])
    }
    col_n <- dplyr::n_distinct(data[[cols_pos]], na.rm = TRUE)

    by_label <-
      if(length(by_pos)>0) get_raw_labels(data, unname(by_pos))

    df_main <-
      vctrs::df_list(variable = names(cols_pos),
                     .variable_label = var_labels,
                     N = number_rows,
                     X = by_label)
    df_main <- vctrs::new_data_frame(df_main)


    if(length(by_pos) > 0) {
      if(is.null(by_type)) {
        by_type <- class(data[[by_pos]])
      }
      by_n <- dplyr::n_distinct(data[[by_pos]], na.rm = TRUE)
    }

    count_uniques <- dplyr::count(data,
                                  dplyr::pick(tidyselect::any_of(names(c(cols_pos, by_pos)))),
                                  name = ".n_count")

    if(col_n > 1 &&
       (length(by_pos) == 0 || by_n > 1) &&
       (col_type != "factor" || all(count_uniques$.n_count >= 10))) {


      stat_config <-
        find_stat_config(cols_pos = cols_pos,
                         col_type = col_type,
                         col_n = col_n,
                         cols_unique = unique(data[[cols_pos]]),
                         by_type = by_type,
                         by_pos = by_pos,
                         by_n = by_n,
                         by_unique = unique(data[[by_pos]]),
                         call = call)

      estimate <-
        data %>%
        infer::specify(response = {{cols}},
                       explanatory = {{by}},
                       success = stat_config$success) %>%
        infer::calculate(stat = stat_config$test, order = stat_config$order_lvls) %>%
        suppressWarnings()

      null_dist <-
        data %>%
        infer::specify(response = {{cols}},
                       explanatory = {{by}},
                       success = stat_config$success) %>%
        infer::hypothesize(null = stat_config$null_hypothesis_type,
                           p = stat_config$p_lvls,
                           mu = stat_config$m_lvls) %>%
        infer::generate(reps = dots$reps, type = stat_config$generate_type) %>%
        infer::calculate(stat = stat_config$test, order = stat_config$order_lvls) %>%
        suppressWarnings()

      pval <-
        null_dist %>%
        infer::get_p_value(obs_stat = estimate, direction = "two-sided") %>%
        suppressWarnings()
      pval <- pval$p_value

      df_main %>%
        dplyr::mutate(test = stat_config$test,
                      stat = round(estimate[["stat"]], digits = 1),
                      p = dplyr::if_else(pval > 0, pval, 3/dots$reps),
                      p = dplyr::if_else(.data$N >= dots$hide_test_if_n_below, .data$p, NA_real_))
    } else df_main
  }


#' Title
#'
#' @inheritParams sigtest
#' @inheritParams embed_cat_prop_plot
#'
#' @return Data frame
#' @export
#'
#' @examples
#' embed_uni_sigtest(data=ex_survey1,
#'                   cols = a_1:a_9, reps=10, hide_test_if_n_below=10)
#' embed_uni_sigtest(data=ex_survey1,
#'                   cols = b_1:b_3, reps=10, hide_test_if_n_below=10,
#'                   label_separator=" - ")
#' embed_uni_sigtest(data=ex_survey1,
#'                   cols = c_1:c_2, reps=10, hide_test_if_n_below=10)
embed_uni_sigtest <-
  function(data,
           cols,
           ...,
           by = NULL,
           col_type = NULL,
           by_type = NULL,
           label_separator = NULL,
           translations = .saros.env$defaults$translations,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

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
      unique()
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
            call = call,
            !!!dots))
      }) %>%
      dplyr::bind_rows()
    out$N <- NULL
    # names(out)[names(out) == ".variable_label"] <- main_question

    if(dplyr::n_distinct(out[[main_question]])==1) out[[main_question]] <- NULL

    attr(out, "saros_caption") <- stringr::str_c(translations$sigtest_prefix,
                                                 main_question,
                                                 translations$sigtest_suffix)

    out

  }



#' Title
#'
#' @inheritParams sigtest
#' @inheritParams embed_cat_prop_plot
#'
#' @return Data frame
#' @export
#'
#' @examples
#' embed_bi_sigtest(data=ex_survey1, cols = a_1:a_9, by = x1_sex,
#'                  reps=10, hide_test_if_n_below=1)
#' embed_bi_sigtest(data=ex_survey1, cols = b_1:b_3, by = x1_sex,
#'                  reps=10, hide_test_if_n_below=1, label_separator=" - ")
#' embed_bi_sigtest(data=ex_survey1, cols = c_1:c_2, by = x1_sex,
#'                  reps=10, hide_test_if_n_below=1)
embed_bi_sigtest <-
  function(data,
           cols,
           by,
           ...,
           col_type = NULL,
           by_type = NULL,
           translations = .saros.env$defaults$translations,
           label_separator = NULL,
           call = rlang::caller_env()) {

    dots <- rlang::list2(...)

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
      unique()

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
          call = call,
          !!!dots)
      }) %>%
      dplyr::bind_rows()
    names(out)[names(out) == ".variable_label"] <- main_question
    if(dplyr::n_distinct(out[[main_question]])==1) out[[main_question]] <- NULL

    attr(out, "saros_caption") <- stringr::str_c(translations$sigtest_prefix,
                                                 main_question,
                                                 translations$sigtest_suffix)

    out

  }
