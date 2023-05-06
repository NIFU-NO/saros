response_rates_by_group <- function(data,
                                    sampled_var = NULL,
                                    invalid_var = NULL,
                                    partially_incompleted_var = NULL,
                                    partially_completed_var = NULL,
                                    completed_var = NULL,
                                    n_responses = NULL,
                                    by_var = NULL, # f.eks. "landsdel"
                                    total = NULL, # "Totalt"
                                    digits = 1) {

  if(!is.data.frame(data)) cli::cli_abort("data is not a data frame.")

  sampled_pos <- names(tidyselect::eval_select(rlang::enquo(arg = sampled_var), data = data))
  invalid_pos <- names(tidyselect::eval_select(rlang::enquo(arg = invalid_var), data = data))


  if(!is.null(total) && rlang::is_string(total)) {
    data <-
      dplyr::bind_rows(data,
                       dplyr::mutate(data, dplyr::across(tidyselect::all_of(by_var), .fns = function(x) total)))
  }
  data <- dplyr::group_by(data, dplyr::pick(tidyselect::all_of(by_var)))
  data <- dplyr::summarize(data,
                           n_sampling_frame = dplyr::n(),
                           n_sampled = if(!rlang::is_null(sampled_pos)) sum(as.integer({{ sampled_var }}), na.rm=FALSE),
                           n_invalid = sum(as.integer({{ invalid_var }}), na.rm=FALSE),
                           n_partially_incompleted = sum(as.integer({{ partially_incompleted_var }}), na.rm=FALSE),
                           n_partially_completed = sum(as.integer({{ partially_completed_var }}), na.rm=FALSE),
                           n_completed = sum(as.integer({{ partially_completed_var }}), na.rm=FALSE),

                           p_sampled_by_sampling_frame = round(.data$n_sampled/.data$n_sampling_frame, digits=digits),
                           p_invalid_by_sampled = round(.data$n_invalid/.data$n_sampled, digits=digits),
                           p_valid_by_sampled = round(.data$n_completed+.data$n_partially_completed/.data$n_sampled, digits=digits),
                           p_completed_by_sampled = round(.data$n_completed/.data$n_sampled, digits=digits),
                           p_partially_completed_by_sampled = round(.data$n_partially_completed/.data$n_sampled, digits=digits),
                           p_valid_by_sampling_frame = round(.data$n_completed+.data$n_partially_completed/.data$n_sampling_frame, digits=digits),

                           mean_responses = mean(as.integer({{ n_responses }}), na.rm=FALSE),
                           med_responses = stats::median(as.integer({{ n_responses }}), na.rm=FALSE),
                           min_responses = min(as.integer({{ n_responses }}), na.rm=FALSE),
                           max_responses = max(as.integer({{ n_responses }}), na.rm=FALSE))
  data
}
