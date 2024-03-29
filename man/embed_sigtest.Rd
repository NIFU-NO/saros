% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sigtest2.R
\name{embed_sigtest}
\alias{embed_sigtest}
\title{Test Significance Based on Randomization Theory}
\usage{
embed_sigtest(data, chapter_overview, ..., call = rlang::caller_env())
}
\arguments{
\item{data}{\emph{Survey data}

\verb{obj:<data.frame>|obj:<tbl_df>} // Required

A data frame (or a srvyr-object) with the columns specified in the
chapter_overview 'dep_cat', etc columns.}

\item{chapter_overview}{\emph{What goes in each chapter}

\verb{obj:<data.frame>|obj:<tbl_df>} // Required

Data frame (or tibble, possibly grouped). One row per chapter. Should
contain the columns 'chapter' and 'dep', Optionally 'indep' (independent
variables) and other informative columns as needed.}

\item{...}{\emph{Dynamic dots}

<\href{https://rlang.r-lib.org/reference/dyn-dots.html}{\code{dynamic-dots}}>

Arguments forwarded to the corresponding functions that create the elements.}

\item{call}{\emph{Internal call}

\verb{obj:<call>} // \emph{Default:} \code{rlang::caller_env()} (\code{optional})

Both the absolute and relative folderpaths are required, as strings.}
}
\value{
Data frame
}
\description{
Test Significance Based on Randomization Theory
}
