% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table1.R
\name{table1}
\alias{table1}
\title{Table 1}
\usage{
table1(
  DF,
  y,
  paired = FALSE,
  strata = NULL,
  ynames = NULL,
  make.names = TRUE,
  overall = TRUE,
  overall_name = "Overall",
  tests = TRUE,
  norm_test = TRUE,
  Khi2 = TRUE,
  mutation = 40,
  legend = TRUE,
  title = TRUE,
  round = 2,
  exit = "html"
)
}
\arguments{
\item{DF}{dataframe : datas to be analysed}

\item{y}{character : column that can separate the dataframe}

\item{ynames}{vector of characters (optionnal) : names to be put in table one columns instead of y levels.}

\item{make.names}{(optionnal) : TRUE so that the colnames provided are formated}

\item{overall}{booleen (optionnal) : TRUE if an "overall" column is wanted}

\item{overall_name}{(optionnal) : name of the 'overall' column}

\item{tests}{booleen (optionnal) : TRUE if you want tests to be executed}

\item{norm_test}{booleen (optionnal) : TRUE if you want tests on normal distribution to be performed has condition to student test}

\item{Khi2}{booleen (optionnal) : if TRUE, khi2 is prefered to fisher}

\item{mutation}{numeric : number of modalities to display for one variable. If there is more than "mutation" categories, the modalities after this threeshold are wrapped into an "others" categorie.}

\item{legend}{booleen (optionnal) : TRUE if a legend is wanted under the table}

\item{title}{booleen (optionnal) : TRUE if a title is wanted}

\item{round}{(optionnal) : the number of digits to be display in the results}

\item{exit}{character : 'html', 'console' or 'excel'}
}
\value{
dataframe, excel file or html table depending on the exit parameter
}
\description{
Table 1
}
