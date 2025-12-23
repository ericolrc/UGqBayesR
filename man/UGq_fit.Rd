\name{UGq_fit}
\alias{UGq_fit}
\title{Bayesian UGq regression via NIMBLE}
\description{Fits a Bayesian Unified UGq regression model for bounded data using NIMBLE.}
\usage{
UGq_fit(formula.mu, formula.sigma = ~1, data, family,
niter = 16000, nburnin = 1000, thin = 10, nchains = 2)
}
\arguments{
\item{formula.mu}{Formula for the mean submodel.}
\item{formula.sigma}{Formula for the dispersion submodel.}
\item{data}{Data frame containing variables in the model.}
\item{family}{An object of class \code{UGq_family}.}
\item{niter}{Total number of MCMC iterations.}
\item{nburnin}{Burn-in iterations.}
\item{thin}{Thinning interval.}
\item{nchains}{Number of MCMC chains.}
}
\value{An object of class \code{UGq_fit}.}
\author{Éric O. Rocha}
