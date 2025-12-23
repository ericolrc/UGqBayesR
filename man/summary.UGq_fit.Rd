\name{summary.UGq_fit}
\alias{summary.UGq_fit}
\title{Summary of a UGq fitted model}
\description{Provides posterior summaries for UGq regression parameters.}
\usage{
\method{summary}{UGq_fit}(object, pars = c("beta", "gamma"),
prob = c(0.025, 0.975), use.chains = "all", ...)
}
\arguments{
\item{object}{An object of class \code{UGq_fit}.}
\item{pars}{Parameters to summarize.}
\item{prob}{Credible interval probabilities.}
\item{use.chains}{Combine or separate MCMC chains.}
}
\value{An object of class \code{summary.UGq_fit}.}
\author{Éric O. Rocha}
