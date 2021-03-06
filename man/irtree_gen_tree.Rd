% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate-data.R
\name{irtree_gen_tree}
\alias{irtree_gen_tree}
\title{Generate data from an IR-tree model}
\usage{
irtree_gen_tree(
  object = NULL,
  N = NULL,
  sigma = NULL,
  theta = NULL,
  itempar = NULL,
  link = c("logit", "probit"),
  na_okay = TRUE
)
}
\arguments{
\item{object}{Object of class \code{irtree_model}. See \link{irtree_model} for more
information.}

\item{N}{Integer, the number of persons.}

\item{sigma}{Either a matrix or a function that returns a matrix. This matrix
is the variance-covariance matrix of the person parameters that is passed
to \code{\link[MASS:mvrnorm]{MASS::mvrnorm()}}. Note that the order of the person
parameters is taken from the section Processes in the model \code{object} (see
\link{irtree_model}).}

\item{theta}{Optional numeric matrix of person parameters with one row per person and
one column per dimension (i.e., \code{object$S}). If provided, this overrides
\code{N} and \code{sigma}.}

\item{itempar}{Either a list or a function that returns a list. The list has
an element \code{beta} and an element \code{alpha}. Each of these is a
matrix of item parameters. Note that the order of items (rows) is taken from the
section Items and the order of processes (columns) is taken from the
section Processes in the \code{model} (see \link{irtree_model}).}

\item{link}{Character. Link function to use.}

\item{na_okay}{Logical indicating whether variables with unobserved response
categories are permitted. If \code{FALSE}, rejection sampling
is used to ensure that all categories are observed.}
}
\value{
A list with element \code{data} containing the data and an
element \code{spec} containing the true parameter values etc.
}
\description{
This function generates data from an IR-tree model.
}
\keyword{internal}
