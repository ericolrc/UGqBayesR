UGq_family <- function(tau = 0.9,
                       link.mu = "logit",
                       link.sigma = "log") {
  structure(
    list(
      family = "UGq",
      parameters = c("mu", "sigma"),
      tau = tau,
      link = list(mu = link.mu, sigma = link.sigma),
      support = c(0, 1)
    ),
    class = "UGq_family"
  )
}
