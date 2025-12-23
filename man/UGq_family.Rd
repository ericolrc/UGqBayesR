\name{UGq_family}
\alias{UGq_family}
\title{Unified UGq family object}
\description{Creates a family object for the Unified UGq regression model.}
\usage{
UGq_family(tau = 0.9, link.mu = "logit", link.sigma = "log")
}
\arguments{
\item{tau}{Quantile level defining the UGq distribution.}
\item{link.mu}{Link function for the mean parameter.}
\item{link.sigma}{Link function for the dispersion parameter.}
}
\value{An object of class \code{UGq_family}.}
\author{Éric O. Rocha}
