keep_cols <- function(data,
                      dep,
                      indep = NULL,
                      mesos_var = NULL,
                      mesos_group = NULL,
                      crowd = "all",
                      hide_for_crowd_if_all_na = FALSE, # 1
                      hide_for_crowd_if_valid_n_below = 0, # 2
                      hide_for_crowd_if_category_k_below = 0, # 3
                      hide_for_crowd_if_category_n_below = 0, # 4
                      hide_for_crowd_if_cell_n_below = 0, # 5
                      hide_for_all_crowds_if_hidden_for_crowd_vars = NULL # 6
                      ) {

  out <- data[makeme_keep_rows(data = data,
                               crwd = crowd,
                               mesos_var = mesos_var,
                               mesos_group = mesos_group), , drop = FALSE]

  kept_vars <- c()


  for(d in dep) {
    keep_1 <-
      isFALSE(hide_for_crowd_if_all_na) ||
      !all(is.na(as.character(out[[d]])))
    keep_2 <-
      length(!is.na(as.character(out[[d]]))) >= hide_for_crowd_if_valid_n_below
    x <- as.character(unique(out[[d]]))
    keep_3 <-
      length(x[!is.na(x)]) >= hide_for_crowd_if_category_k_below
    x <- table(out[[d]], useNA = "no")
    x <- x[x>0]
    keep_4 <-
      length(x) > 0 &&
        min(x, na.rm = TRUE) >= hide_for_crowd_if_category_n_below

    keep_5 <- TRUE
    if(length(indep)>0) {
      keep_5 <-
        lapply(indep, function(i) {
          x <- table(out[[d]], out[[i]], useNA = "no")
          x <- x[x>0] # Only interested in cells with something in them
          length(x) > 0 &&
            min(x, na.rm = TRUE) >= hide_for_crowd_if_cell_n_below
        }) |> unlist() |> all()
    }

    keep_6 <- !d %in% hide_for_all_crowds_if_hidden_for_crowd_vars

    if(keep_1 && keep_2 && keep_3 && keep_4 && keep_5 && keep_6) {
      kept_vars <- c(kept_vars, d)
    }
  }

  list(data = out[, c(kept_vars, indep, mesos_var), drop=FALSE],
       kept_vars = kept_vars,
       omitted_vars = dep[!dep %in% kept_vars])

}
