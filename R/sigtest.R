
find_stat_config <-
  function(dep_pos,
           .variable_type,
           dep_n,
           dep_unique,
           indep_type,
           indep_pos,
           indep_n,
           indep_unique) {

    categorical_types <- c("fct", "factor", "ord", "ordered")
    continuous_types <- c("numeric", "dbl", "integer", "int")

    stat_test <-
      dplyr::case_when(
        .variable_type %in% continuous_types &&
          dep_n > 2 && length(indep_pos) == 0 ~ "mean",

        dep_n == 2 && length(indep_pos) == 0 ~ "prop",

        .variable_type %in% categorical_types &&
          dep_n > 2 && length(indep_pos) == 0 ~ "chisq",

        .variable_type %in% continuous_types &&
          length(indep_pos) == 1 &&
          indep_type %in% continuous_types &&
          indep_n >= 5 ~ "correlation",

        .variable_type %in% continuous_types &&
          length(indep_pos) == 1 &&
          indep_type %in% categorical_types &&
          indep_n > 2 ~ "F",

        .variable_type %in% continuous_types &&
          length(indep_pos) == 1 &&
          indep_type %in% categorical_types &&
          indep_n == 2 ~ "t",

        .variable_type %in% categorical_types &&
          dep_n >= 2 &&
          length(indep_pos) == 1 &&
          indep_type %in% categorical_types &&
          indep_n >= 2 ~ "chisq",

        .default = "NA"
      )
    if(stat_test == "NA" && .variable_type != "chr") {
      error_indep_str <- if(length(indep_pos) == 1) stringi::stri_c(ignore_null=TRUE, " and {.arg {indep_type}} ({.arg {indep_pos}})")
      cli::cli_warn("Statistical test not found for {.arg { .variable_type}} ({.arg {dep_pos}}, n_unique={.var {dep_n}}){error_indep_str}.")
    }

    lvls <- dep_unique %>% as.character()
    success <- if(stat_test %in% c("prop", "chisq") && dep_n == 2) lvls[length(lvls)]
    p_lvls <- if(stat_test %in% c("prop", "chisq") && length(indep_pos) == 0) stats::setNames(rep(1/length(lvls), length(lvls)), nm=lvls)
    m_lvls <- if(stat_test %in% c("mean")) 0
    order_lvls <- if(stat_test %in% c("prop", "chisq", "t") &&
                     # .variable_type %in% c("numeric", "integer", "int") &&
                     length(indep_pos) == 1 &&
                     indep_n == 2) as.character(indep_unique)
    generate_type <- if(length(indep_pos) == 0 && stat_test %in% c("chisq", "prop")) "draw" else "bootstrap"
    null_hypothesis_type <- if(length(indep_pos) == 0) "point" else "independence"

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
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#' @inheritParams summarize_data
#' @param .variable_type,indep_type Optional string specifying data type ("int", "fct", "character", "factor", "numeric", "integer"). Can be obtained from data lookup.
#'
#' @return Data frame
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' library(dplyr)
#' bind_rows(
#' sigtest(data=ex_survey1, dep = a_1) # t-test of proportions (.5): p=.98
#' sigtest(data=ex_survey1, dep = b_1), # Chi-square (all=.333): p=<.001
#' sigtest(data=ex_survey1, dep = c_1), # one-sample t-test p<.001
#' sigtest(data=ex_survey1, dep = b_1, indep = x1_sex), # Chi-square: p = .548
#' sigtest(data=ex_survey1, dep = b_1, indep = f_uni), # Chi-square: p = .102
#' sigtest(data=ex_survey1, dep = c_1, indep = x1_sex), # two-sample t-test: p = .97
#' sigtest(data=ex_survey1, dep = c_1, indep = f_uni), # ANOVA/F-test: p = .22
#' sigtest(data=ex_survey1, dep = c_1, indep = c_2)) # correlation: p = .976
#'
#' sigtest(data=ex_survey1, dep = a_1, indep = c_1) # NA
#' sigtest(data=ex_survey1, dep = b_1, indep = c_1) # NA
#'}
#'
sigtest <-
  function(data,
           dep,
           ...,
           indep = NULL,
           .variable_type = NULL,
           indep_type = NULL,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "sigtest")


    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
    if(length(dep_pos) != 1) cli::cli_abort("{.arg dep}: {dep_pos} must be a single column.")
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)
    if(length(indep_pos) >= 2) cli::cli_abort("{.arg indep}: {indep_pos} must be at most a single column.")

    data <-
      dplyr::filter(data, dplyr::if_all(.cols=c({{dep}}, {{indep}}), .fns = ~!is.na(.x))) %>%
      dplyr::mutate(dplyr::across(c({{dep}}, {{indep}}) & tidyselect::where(~is.factor(.x)), ~forcats::fct_drop(.x)))

    number_rows <- nrow(data)
    if(is.null(number_rows)) browser()
    number_rows_by_group <- min(table(data[,c(dep_pos, indep_pos)]))

    var_labels <-
      get_raw_labels(data = data, col_pos = dep_pos) %>%
      keep_subitem(label_separator = dots$label_separator) %>%
      as.character()

    if(is.null(.variable_type)) {
      .variable_type <- class(data[[dep_pos]])
    } else if(.variable_type == "int" && any(class(data[[dep_pos]]) %in% c("factor", "ordered"))) {
      data[[dep_pos]] <- as.numeric(data[[dep_pos]])
    } else if(.variable_type %in% c("fct", "ord") && any(class(data[[dep_pos]]) %in% c("integer", "numeric"))) {
      data[[dep_pos]] <- as.factor(data[[dep_pos]])
    }
    dep_n <- dplyr::n_distinct(data[[dep_pos]], na.rm = TRUE)

    indep_label <-
      if(length(indep_pos) >= 1) get_raw_labels(data = data, col_pos = unname(indep_pos))

    df_main <-
      vctrs::df_list(variable = names(dep_pos),
                     .variable_label = var_labels,
                     N = number_rows,
                     X = indep_label)
    df_main <- vctrs::new_data_frame(df_main)


    if(length(indep_pos) >= 1) {
      if(is.null(indep_type)) {
        indep_type <- class(data[[indep_pos]])
      }
      indep_n <- dplyr::n_distinct(data[[indep_pos]], na.rm = TRUE)
    }

    count_uniques <-
      dplyr::count(data,
                   dplyr::pick(tidyselect::any_of(names(c(dep_pos, indep_pos)))),
                   name = ".n_count")

    if(min(table(data[, c(dep_pos, indep_pos)]))==Inf) browser()
    if(dep_n >= 2 &&
       (length(indep_pos) == 0 || indep_n >= 2) &&
       (!.variable_type %in% c("fct", "factor", "ordered", "ord") ||
        all(count_uniques$.n_count >= 10)) &&
       df_main$N >= 1 &&
       min(table(data[, c(dep_pos, indep_pos)])) >= 1) {


      stat_config <-
        find_stat_config(dep_pos = dep_pos,
                         .variable_type = .variable_type,
                         dep_n = dep_n,
                         dep_unique = unique(data[[dep_pos]]),
                         indep_type = indep_type,
                         indep_pos = indep_pos,
                         indep_n = indep_n,
                         indep_unique = unique(data[[indep_pos]]))

      if(stat_config$test == "NA") return(NULL)
      estimate <-
        infer::specify(data,
                       response = {{dep}},
                       explanatory = {{indep}},
                       success = stat_config$success)

      tryCatch({
      estimate <-
        infer::calculate(estimate,
                         stat = stat_config$test,
                         order = stat_config$order_lvls) %>%
        suppressWarnings()
      }, error = function(e) {
        print(stat_config)
        browser()
        })

      null_dist <-
        infer::specify(data,
                       response = {{dep}},
                       explanatory = {{indep}},
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

        dplyr::mutate(df_main,
                      test = stat_config$test,
                      stat = round(estimate[["stat"]], digits = 1),
                      p = dplyr::if_else(pval > 0, pval, 3/dots$reps),
                      p = dplyr::if_else(.data$N >= dots$hide_test_if_n_below, .data$p, NA_real_))
    } else df_main
  }


#' Run univariate significance tests and collect them in a table
#'
#' @inheritParams sigtest
#' @inheritParams gen_qmd_chapters
#' @inheritParams embed_cat_prop_plot
#'
#' @return Data frame
#' @export
#'
#' @examples
#' embed_uni_sigtest(data=ex_survey1,
#'                   dep = a_1:a_9, reps=10, hide_test_if_n_below=10)
#' embed_uni_sigtest(data=ex_survey1,
#'                   dep = b_1:b_3, reps=10, hide_test_if_n_below=10,
#'                   label_separator=" - ")
#' embed_uni_sigtest(data=ex_survey1,
#'                   dep = c_1:c_2, reps=10, hide_test_if_n_below=10)
embed_uni_sigtest <-
  function(data,
           dep,
           ...,
           indep = NULL,
           .variable_type = NULL,
           indep_type = NULL,
           mesos_group = NULL,
           call = rlang::caller_env()) {


    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "uni_sigtest")

    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)
    if(length(indep_pos) > 1L) cli::cli_abort("{.arg indep} must be at most a single column.")

    data <-
      dplyr::filter(data, dplyr::if_all(.cols=c(dep_pos, indep_pos), ~!is.na(.x)))

    main_question <-
      get_raw_labels(data = data, col_pos = dep_pos) %>%
      get_main_question2(label_separator = dots$label_separator,
                         warn_multiple = TRUE) %>%
      unique()
    main_question <-
      if(!is.null(dots$label_separator) &&
         length(main_question)==1) main_question else ".variable_label"


    out <-
      lapply(names(dep_pos), FUN = function(.x) {
        col_sym <- rlang::sym(.x)

        rlang::inject(
          sigtest(
            data = data,
            dep = !!col_sym,
            # indep = !!indep_pos,
            .variable_type = .variable_type,
            indep_type = indep_type,
            !!!dots))
      })
    out <- dplyr::bind_rows(out)
    out$N <- NULL
    # names(out)[names(out) == ".variable_label"] <- main_question

    if(dplyr::n_distinct(out[[main_question]])==1) out[[main_question]] <- NULL

    attr(out, "saros_caption") <- stringi::stri_c(ignore_null=TRUE, dots$translations$sigtest_prefix,
                                                 main_question,
                                                 dots$translations$sigtest_suffix)

    out

  }



#' Bivariate significance tests
#'
#' @inheritParams draft_report
#' @inheritParams gen_qmd_chapters
#' @inheritParams summarize_data
#' @inheritParams sigtest
#'
#' @return Data frame
#' @export
#'
#' @examples
#' embed_bi_sigtest(data=ex_survey1, dep = a_1:a_9, indep = x1_sex,
#'                  reps=10, hide_test_if_n_below=1)
#' embed_bi_sigtest(data=ex_survey1, dep = b_1:b_3, indep = x1_sex,
#'                  reps=10, hide_test_if_n_below=1, label_separator=" - ")
#' embed_bi_sigtest(data=ex_survey1, dep = c_1:c_2, indep = x1_sex,
#'                  reps=10, hide_test_if_n_below=1)
embed_bi_sigtest <-
  function(data,
           dep,
           indep,
           ...,
           .variable_type = NULL,
           indep_type = NULL,
           call = rlang::caller_env()) {

    dots <- update_dots(dots = rlang::list2(...),
                        caller_function = "bi_sigtest")


    dep_enq <- rlang::enquo(arg = dep)
    dep_pos <- tidyselect::eval_select(dep_enq, data = data, error_call = call)
    indep_enq <- rlang::enquo(arg = indep)
    indep_pos <- tidyselect::eval_select(indep_enq, data = data, error_call = call)
    if(length(indep_pos) > 1L) cli::cli_abort("{.arg indep} must be at most a single column.")

    data <-
      dplyr::filter(data, dplyr::if_all(.cols = c(dep_pos, indep_pos), ~!is.na(.x)))

    main_question <-
      get_raw_labels(data = data, col_pos = dep_pos) %>%
      get_main_question2(label_separator = dots$label_separator,
                         warn_multiple = TRUE) %>%
      unique()

    main_question <-
      if(!is.null(dots$label_separator) &&
         length(main_question)==1) main_question else ".variable_label"

    indep_sym <- rlang::sym(names(indep_pos))

    out <-
      lapply(names(dep_pos), FUN = function(.x) {
        col_sym <- rlang::sym(.x)

        rlang::inject(
        sigtest(
          data = data,
          dep = !!col_sym,
          indep = !!indep_sym,
          .variable_type = .variable_type,
          indep_type = indep_type,
          !!!dots))
      })
    out <- dplyr::bind_rows(out)
    names(out)[names(out) == ".variable_label"] <- main_question
    if(dplyr::n_distinct(out[[main_question]])==1) out[[main_question]] <- NULL

    attr(out, "saros_caption") <- stringi::stri_c(ignore_null=TRUE, dots$translations$sigtest_prefix,
                                                 main_question,
                                                 dots$translations$sigtest_suffix)

    out

  }
