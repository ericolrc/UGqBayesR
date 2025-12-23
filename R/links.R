links <- list(
  mu = list(
    logit = list(
      link = function(x) log(x / (1 - x)),
      inv = function(eta) 1 / (1 + exp(-eta))
    )
  ),
  sigma = list(
    log = list(
      link = function(x) log(x),
      inv = function(eta) exp(eta)
    )
  )
)
