% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exploratory.R
\name{exploratory}
\alias{exploratory}
\title{Launch the exploratory analysis tool}
\usage{
exploratory(
  data = datasets::mtcars,
  sigfig = 3,
  select_list_max = 1e+05,
  saved_analyses_file_name = "exploratory_analyses_saved.csv",
  run_analysis_file_name = "exploratory_analyses_run.csv"
)
}
\arguments{
\item{data}{a data object (a data frame or a data.table)}

\item{sigfig}{number of significant digits to round to}

\item{select_list_max}{maximum number of variable names to display
for dropdown menus}

\item{saved_analyses_file_name}{name of the .csv file in which
saved analyses will be recorded
(default = "exploratory_analyses_saved.csv")}

\item{run_analysis_file_name}{name of the .csv file in which
all conducted analyses will be recorded
(default = "exploratory_analyses_run.csv")}
}
\value{
There will be no output from this function. Rather, the
exploratory analysis tool (a Shiny App) will open in a browser
on the local machine.
}
\description{
Launches the exploratory analysis tool in a browser on the local machine
}
\examples{
if (interactive()) {exploratory(data = mtcars)}
}
