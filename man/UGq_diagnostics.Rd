\name{UGq_diagnostics}
\alias{UGq_diagnostics}
\title{Convergence diagnostics for UGq models}
\description{Computes R-hat, effective sample size and diagnostic plots.}
\usage{
UGq_diagnostics(object, pars = c("beta", "gamma"), plot = TRUE)
}
\arguments{
\item{object}{An object of class \code{UGq_fit}.}
\item{pars}{Parameters for diagnostics.}
\item{plot}{Logical; whether to show trace and density plots.}
}
\value{An object of class \code{UGq_diagnostics}.}
\author{Éric O. Rocha}
