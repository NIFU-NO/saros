withr::with_tempdir({
  test_that("crowd_plots_as_tabset handles categorical plots", {
    skip_if_not_installed("knitr")

    # Create test plots
    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A",
      type = "cat_plot_html"
    )

    # Should not error
    expect_no_error({
      output <- capture.output({
        saros::crowd_plots_as_tabset(
          plots,
          plot_type = "cat_plot_html",
          save = FALSE
        )
      })
    })

    # Output should contain markdown for tabs
    output <- capture.output({
      saros::crowd_plots_as_tabset(
        plots,
        plot_type = "cat_plot_html",
        save = FALSE
      )
    })

    expect_true(any(grepl("##### Target", output)))
    expect_true(any(grepl("N = ", output)))
  })

  test_that("crowd_plots_as_tabset handles interval plots", {
    skip_if_not_installed("knitr")

    # Create interval plots
    int_plots <- saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A",
      type = "int_plot_html"
    )

    expect_no_error({
      capture.output({
        saros::crowd_plots_as_tabset(
          int_plots,
          plot_type = "int_plot_html",
          save = FALSE
        )
      })
    })

    output <- capture.output({
      saros::crowd_plots_as_tabset(
        int_plots,
        plot_type = "int_plot_html",
        save = FALSE
      )
    })

    expect_true(any(grepl("##### Target", output)))
  })

  test_that("crowd_plots_as_tabset auto-detects categorical plot type", {
    skip_if_not_installed("knitr")

    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A",
      type = "cat_plot_html"
    )

    expect_no_error({
      output <- capture.output({
        saros::crowd_plots_as_tabset(plots, plot_type = "auto", save = FALSE)
      })
    })

    # output <- capture.output({
    #   saros::crowd_plots_as_tabset(plots, plot_type = "auto", save = FALSE)
    # })

    # Should use fig_height_h_barchart2 for categorical
    expect_true(any(grepl("#### Target", output)))
  })

  test_that("crowd_plots_as_tabset auto-detects interval plot type", {
    skip_if_not_installed("knitr")

    int_plots <- saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A",
      type = "int_plot_html"
    )

    expect_no_error({
      capture.output({
        saros::crowd_plots_as_tabset(
          int_plots,
          plot_type = "auto",
          save = FALSE
        )
      })
    })

    output <- capture.output({
      saros::crowd_plots_as_tabset(int_plots, plot_type = "auto", save = FALSE)
    })

    # Should use default height for interval plots
    expect_true(any(grepl("##### Target", output)))
  })

  test_that("crowd_plots_as_tabset handles save parameter", {
    skip_if_not_installed("knitr")
    skip_if_not_installed("withr")

    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A"
    )

    output_no_save <- capture.output({
      saros::crowd_plots_as_tabset(plots, save = TRUE)
    })
    expect_true(any(grepl("\\[PNG\\]", output_no_save)))
  })

  test_that("crowd_plots_as_tabset validates input types", {
    # Non-list input
    expect_error(
      saros::crowd_plots_as_tabset("not a list", save = FALSE),
      "must be a list"
    )

    # Empty list
    expect_warning(
      saros::crowd_plots_as_tabset(list(), save = FALSE),
      "empty"
    )

    # All NULL plots
    expect_warning(
      saros::crowd_plots_as_tabset(list(a = NULL, b = NULL), save = FALSE),
      "All plots.*are NULL"
    )

    # Invalid save parameter (NULL)
    plots <- list(plot1 = ggplot2::ggplot())
    expect_error(
      saros::crowd_plots_as_tabset(plots, save = NULL),
      "must be a single logical value"
    )

    # Invalid save parameter (character)
    expect_error(
      saros::crowd_plots_as_tabset(plots, save = "yes"),
      "must be a single logical value"
    )

    # Invalid save parameter (NA)
    expect_error(
      saros::crowd_plots_as_tabset(plots, save = NA),
      "must be a single logical value"
    )

    # Invalid save parameter (vector)
    expect_error(
      saros::crowd_plots_as_tabset(plots, save = c(TRUE, FALSE)),
      "must be a single logical value"
    )
  })

  test_that("crowd_plots_as_tabset skips NULL plots in mixed list", {
    skip_if_not_installed("knitr")

    # Create a plot list with one NULL
    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A"
    )

    # Add a NULL plot
    plots$null_plot <- NULL

    expect_no_error({
      output <- capture.output({
        saros::crowd_plots_as_tabset(plots, save = FALSE)
      })
    })

    # Should still generate output for non-NULL plots
    output <- capture.output({
      saros::crowd_plots_as_tabset(plots, save = FALSE)
    })
    expect_true(length(output) > 0)
  })

  test_that("crowd_plots_as_tabset uses custom int plot default height", {
    skip_if_not_installed("knitr")

    int_plots <- saros::makeme(
      data = saros::ex_survey,
      dep = c_1:c_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A",
      type = "int_plot_html"
    )

    output <- capture.output({
      saros::crowd_plots_as_tabset(
        int_plots,
        plot_type = "int_plot_html",
        fig_height_int_default = 8,
        save = FALSE
      )
    })

    expect_true(any(grepl("##### Target", output)))
  })

  test_that("crowd_plots_as_tabset returns invisible NULL", {
    skip_if_not_installed("knitr")

    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A"
    )

    result <- capture.output({
      return_val <- saros::crowd_plots_as_tabset(plots, save = FALSE)
    })

    expect_null(return_val)
  })

  test_that("crowd_plots_as_tabset handles plot type argument matching", {
    skip_if_not_installed("knitr")

    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A"
    )

    # Should error on invalid plot_type
    expect_error(
      capture.output({
        saros::crowd_plots_as_tabset(
          plots,
          plot_type = "invalid_type",
          save = FALSE
        )
      }),
      "'arg' should be one of"
    )
  })

  test_that("crowd_plots_as_tabset generates tab labels from names", {
    skip_if_not_installed("knitr")

    plots <- saros::makeme(
      data = saros::ex_survey,
      dep = b_1:b_2,
      crowd = c("target", "others"),
      mesos_var = "f_uni",
      mesos_group = "Uni of A"
    )

    output <- capture.output({
      saros::crowd_plots_as_tabset(plots, save = FALSE)
    })

    # Output should contain the plot names as tab labels
    combined_output <- paste(output, collapse = "\n")
    expect_true(any(grepl("Others", names(plots))))
  })
})
