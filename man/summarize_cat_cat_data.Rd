% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_cat_cat_data.R
\name{summarize_cat_cat_data}
\alias{summarize_cat_cat_data}
\title{Summarize a survey dataset for use in tables and graphs}
\usage{
summarize_cat_cat_data(
  data,
  dep = colnames(data),
  indep = NULL,
  ...,
  showNA = c("ifany", "always", "never"),
  totals = FALSE,
  sort_by = ".upper",
  data_label = c("percentage_bare", "percentage", "proportion", "count"),
  digits = 0,
  add_n_to_dep_label = FALSE,
  add_n_to_indep_label = FALSE,
  add_n_to_label = FALSE,
  add_n_to_category = FALSE,
  hide_label_if_prop_below = 0.01,
  data_label_decimal_symbol = ".",
  categories_treated_as_na = NULL,
  label_separator = NULL,
  descend = FALSE,
  labels_always_at_bottom = NULL,
  labels_always_at_top = NULL,
  translations = list(),
  call = rlang::caller_env()
)
}
\arguments{
\item{data}{\emph{Your data.frame/tibble or srvyr-object (experimental)}

\code{data.frame} // \emph{required}

The data to be used for plotting.}

\item{dep, indep}{\emph{Variable selections}

<\code{tidyselect}> // \emph{Default:} \code{NULL}, meaning everything for dep, nothing for indep.

Columns in \code{data}. \code{dep} is compulsory.}

\item{...}{\emph{Dynamic dots}

<\href{https://rlang.r-lib.org/reference/dyn-dots.html}{\code{dynamic-dots}}>

Arguments forwarded to the corresponding functions that create the elements.}

\item{showNA}{\emph{Show NA categories}

\verb{vector<character>} // \emph{default:} \code{c("ifany", "always", "never")} (\code{optional})

Choose whether to show NA categories in the results.}

\item{totals}{\emph{Include totals}

\verb{scalar<logical>} // \emph{default:} \code{FALSE} (\code{optional})

Whether to include totals in the output.}

\item{sort_by}{\emph{What to sort output by}

\verb{vector<character>} // \emph{default:} \code{NULL} (\code{optional})

Sort output (and collapse if requested). When using \code{indep}-argument,
sorting differs between ordered factors and unordered factors: Ordering
of ordered factors is always respected in output. Unordered factors will be
reordered by \code{sort_by}. Currently, this works best for a single \code{dep}.

\describe{
\item{NULL}{No sorting.}
\item{".top"}{The proportion for the highest category available in the variable.}
\item{".upper"}{The sum of the proportions for the categories above the middle category.}
\item{".mid_upper"}{The sum of the proportions for the categories including and above the middle category.}
\item{".mid_lower"}{The sum of the proportions for the categories including and below the middle category.}
\item{".lower"}{The sum of the proportions for the categories below the middle category.}
\item{".bottom"}{The proportions for the lowest category available in the variable.}
\item{".variable_label"}{Sort by the variable labels.}
\item{".variable_name"}{Sort by the variable names.}
\item{".variable_position"}{Sort by the variable position in the supplied data frame.}
\item{".by_group"}{The groups of the by argument.}
\item{character()}{Character vector of category labels to sum together.}
}}

\item{data_label}{\emph{Data label}

\verb{scalar<character>} // \emph{default:} \code{"proportion"} (\code{optional})

One of "proportion", "percentage", "percentage_bare", "count", "mean", or "median".}

\item{digits}{\emph{Decimal places}

\verb{scalar<integer>} // \emph{default:} \code{0L} (\code{optional})

Number of decimal places.}

\item{add_n_to_dep_label, add_n_to_indep_label}{\emph{Add N= to the variable label}

\verb{scalar<logical>} // \emph{default:} \code{FALSE} (\code{optional})

For some plots and tables it is useful to attach the \code{"N="} to the end of the label of
the dependent and/or independent variable.
Whether it is \code{N} or \code{N_valid} depends on your \code{showNA}-setting. See also
\code{translations$add_n_to_dep_label_prefix},
\code{translations$add_n_to_dep_label_suffix},
\code{translations$add_n_to_indep_label_prefix},
\code{translations$add_n_to_indep_label_suffix}.}

\item{add_n_to_label}{\emph{Add N= to the variable label of both dep and indep}

\verb{scalar<logical>} // \emph{default:} \code{FALSE} (\code{optional})

For some plots and tables it is useful to attach the \code{"N="} to the end of the label.
Whether it is \code{N} or \code{N_valid} depends on your \code{showNA}-setting. See also
\code{translations$add_n_to_label_prefix} and
\code{translations$add_n_to_label_suffix}.}

\item{add_n_to_category}{\emph{Add N= to the category}

\verb{scalar<logical>} // \emph{default:} \code{FALSE} (\code{optional})

For some plots and tables it is useful to attach the \code{"N="} to the end of the category.
This will likely produce a range across the variables, hence an infix (comma)
between the minimum and maximum can be specified.
Whether it is \code{N} or \code{N_valid} depends on your \code{showNA}-setting. See also
\code{translations$add_n_to_category_prefix},
\code{translations$add_n_to_category_infix}, and
\code{translations$add_n_to_category_suffix}.}

\item{hide_label_if_prop_below}{\emph{Hide label threshold}

\verb{scalar<numeric>} // \emph{default:} \code{NULL} (\code{optional})

Whether to hide label if below this value.}

\item{data_label_decimal_symbol}{\emph{Decimal symbol}

\verb{scalar<character>} // \emph{default:} \code{"."} (\code{optional})

Decimal marker, some might prefer a comma ',' or something else entirely.}

\item{categories_treated_as_na}{\emph{NA categories}

\verb{vector<character>} // \emph{default:} \code{NULL} (\code{optional})

Categories that should be treated as NA.}

\item{label_separator}{\emph{How to separate main question from sub-question}

\verb{scalar<character>} // \emph{default:} \code{NULL} (\code{optional})

Separator for main question from sub-question.}

\item{descend}{\emph{Sorting order}

\verb{scalar<logical>} // \emph{default:} \code{FALSE} (\code{optional})

Reverse sorting of \code{sort_by} in figures and tables. See \code{arrange_section_by}
for sorting of report sections.}

\item{labels_always_at_top, labels_always_at_bottom}{\emph{Top/bottom variables}

\verb{vector<character>} // \emph{default:} \code{NULL} (\code{optional})

Column names in \code{data} that should always be placed at the top or bottom of figures/tables.}

\item{translations}{\emph{Localize your output}

\verb{list<character>}

A list of translations where the name is the code and the value is the translation. See the examples.}

\item{call}{\emph{Internal call}

\verb{obj:<call>} // \emph{Default:} \code{rlang::caller_env()} (\code{optional})

Both the absolute and relative folderpaths are required, as strings.}
}
\value{
Dataset with the columns: \code{.variable_name}, \code{.variable_label}, \code{.category},
\code{.count}, \code{.count_se}, \code{.count_per_dep}, \code{.count_per_indep_group}, \code{.proportion}, \code{.proportion_se},
\code{.mean}, \code{.mean_se}, indep-variable(s), \code{.data_label}, \code{.comb_categories}, \code{.sum_value},
\code{.variable_label_prefix}
}
\description{
Summarize a survey dataset for use in tables and graphs
}
\keyword{internal}
