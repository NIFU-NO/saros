% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/global_settings.R
\name{global_settings_reset}
\alias{global_settings_reset}
\title{Reset Global Options for saros-functions}
\usage{
global_settings_reset(fn_name = "makeme")
}
\arguments{
\item{fn_name}{String, one of \code{"make_link"}, \code{"fig_height_h_barchart"} and \code{"makeme"}.}
}
\value{
Invisibly returned list of old and new values.
}
\description{
Reset Global Options for saros-functions
}
\examples{
global_settings_reset()
}
