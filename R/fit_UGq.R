UGq_fit <- function(formula.mu,
                    formula.sigma = ~ 1,
                    data,
                    family,
                    niter = 16000,
                    nburnin = 1000,
                    thin = 10,
                    nchains = 2) {

  # --- mu ---
  mf_mu <- model.frame(formula.mu, data)
  y <- model.response(mf_mu)
  W <- model.matrix(formula.mu, mf_mu)

  # --- sigma ---
  mf_sigma <- model.frame(formula.sigma, data)
  V <- model.matrix(formula.sigma, mf_sigma)

  N <- length(y)
  p <- ncol(W)
  q <- ncol(V)

  code <- nimbleCode({

    for (i in 1:N) {
      logit(mu[i]) <- inprod(W[i, 1:p], beta[1:p])
      log(sigma[i]) <- inprod(V[i, 1:q], gamma[1:q])
      y[i] ~ dUGq_nimble(mu[i], sigma[i], tau)
    }

    for (j in 1:p) {
      beta[j] ~ dt(0, 100, 2)
    }

    for (j in 1:q) {
      gamma[j] ~ dt(0, 100, 4)
    }
  })

  constants <- list(N = N, p = p, q = q, tau = family$tau)
  data_nim <- list(y = y, W = W, V = V)
  inits <- list(beta = rep(0, p), gamma = rep(0, q))

  fit <- nimbleMCMC(
    code = code,
    data = data_nim,
    constants = constants,
    inits = inits,
    monitors = c("beta", "gamma", "mu", "sigma"),
    nchains = nchains,
    niter = niter,
    nburnin = nburnin,
    thin = thin,
    samplesAsCodaMCMC = TRUE,
    summary = TRUE,
    WAIC = TRUE,
    progressBar = FALSE
  )

  structure(
    list(
      fit = fit,
      family = family,
      formula = list(mu = formula.mu, sigma = formula.sigma),
      data = data
    ),
    class = "UGq_fit"
  )
}

