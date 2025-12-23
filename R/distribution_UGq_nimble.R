dUGq_nimble <- nimbleFunction(
  run = function(x = double(0),
                 mu = double(0),
                 sigma = double(0),
                 tau = double(0),
                 log = integer(0, default = 0)) {
    returnType(double(0))


    q <- qgamma(1 - tau, shape = sigma)
    Hinv <- q / (-log(mu))


    logdens <- sigma * log(Hinv) - lgamma(sigma) +
      (Hinv - 1) * log(x) +
      (sigma - 1) * log(-log(x))


    if (log) return(logdens)
    exp(logdens)
  }
)


registerDistributions(list(
  dUGq_nimble = list(
    BUGSdist = "dUGq_nimble(mu, sigma, tau)",
    types = c(
      "value = double(0)",
      "mu = double(0)",
      "sigma = double(0)",
      "tau = double(0)"
    )
  )
))
