% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{rect_around}
\alias{rect_around}
\alias{hull_around}
\title{Helper functions to find rectangles or convex hulls of data}
\usage{
rect_around(x, y, data = NULL, expand = 0)

hull_around(x, y, data = NULL, expand = 0)
}
\arguments{
\item{x, y}{Unquoted names or expressions}

\item{data}{A data frame}

\item{expand}{Amount to expand the data around its midpoint. Default is 10
per cent.}
}
\value{
\code{rect_around()} returns a list with names \code{xmin}, \code{xmax}, \code{ymin},
and \code{ymax}. \code{hull_around()} returns a data frame with columns \code{x} and \code{y}.
}
\description{
Helper functions to find rectangles or convex hulls of data
}
\examples{
library(ggplot2)
to <- c(2, 4.5, 6, 8)
setosas <- iris[iris$Species == "setosa", ]
rect_around(Sepal.Width, Sepal.Length, data = setosas)
hull_around(Sepal.Width, Sepal.Length, data = setosas)
}
