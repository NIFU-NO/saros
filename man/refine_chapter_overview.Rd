% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_overview.R
\name{refine_chapter_overview}
\alias{refine_chapter_overview}
\title{Processes A 'chapter_overview' Data Frame}
\usage{
refine_chapter_overview(
  chapter_overview = NULL,
  data = NULL,
  ...,
  progress = TRUE,
  variable_group_dep = ".variable_group_dep",
  call = rlang::caller_env()
)
}
\arguments{
\item{chapter_overview}{\emph{What goes in each chapter}

\verb{obj:<data.frame>|obj:<tbl_df>} // Required

Data frame (or tibble, possibly grouped). One row per chapter. Should
contain the columns 'chapter' and 'dep', Optionally 'indep' (independent
variables) and other informative columns as needed.}

\item{data}{\emph{Survey data}

\verb{obj:<data.frame>|obj:<tbl_df>} // Required

A data frame (or a srvyr-object) with the columns specified in the
chapter_overview 'dep_cat', etc columns.}

\item{...}{\emph{Dynamic dots}

<\href{https://rlang.r-lib.org/reference/dyn-dots.html}{\code{dynamic-dots}}>

Arguments forwarded to the corresponding functions that create the elements.}

\item{progress}{\emph{Whether to display progress message}

\verb{scalar<logical>} // \emph{default:} \code{TRUE}}

\item{variable_group_dep}{\emph{Name for the variable_group_dep column}

\verb{scalar<string>} // \emph{default:} \code{".variable_group_dep"}

This column is used to group variables that are part of the same bivariate analysis.}

\item{call}{\emph{Internal call}

\verb{obj:<call>} // \emph{Default:} \code{rlang::caller_env()} (\code{optional})

Both the absolute and relative folderpaths are required, as strings.}
}
\value{
Grouped tibble.
}
\description{
Processes A 'chapter_overview' Data Frame
}
\examples{
ref_df <- refine_chapter_overview(chapter_overview = ex_survey_ch_overview)
\donttest{
ref_df2 <- refine_chapter_overview(chapter_overview = ex_survey_ch_overview,
                     data = ex_survey, hide_bi_entry_if_sig_above=.05)
}
}
