% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/n_rng.R
\name{n_range}
\alias{n_range}
\title{Provides a range (or single value) for N in data, given dep and indep}
\usage{
n_range(
  data,
  dep,
  indep = NULL,
  mesos_var = NULL,
  mesos_group = NULL,
  glue_template_1 = "{n}",
  glue_template_2 = "[{n[1]}-{n[2]}]"
)
}
\arguments{
\item{data}{Dataset}

\item{dep, indep}{Tidyselect syntax}

\item{mesos_var}{Optional, NULL or string specifying name of variable used to
split dataset.}

\item{mesos_group}{Optional, NULL or string specifying value in \code{mesos_var}
indicating the target group.}

\item{glue_template_1, glue_template_2}{String, for the case of a single
value (1) or a range with minimum-maximum of values (2).}
}
\value{
String.
}
\description{
Provides a range (or single value) for N in data, given dep and indep
}
\examples{
n_range(data = ex_survey, dep = b_1:b_3, indep = x1_sex)
}
