library(UGqBayesR)


data(example_data)


fam <- UGq_family(tau = 0.1)


fit <- UGq_fit(
  formula.mu = y ~ x1 + x2,
  formula.sigma = ~ x1 + x2,
  data = example_data,
  family = fam,
  niter = 4000,
  nburnin = 1000,
  thin = 5
)


summary(fit)
UGq_diagnostics(fit)
