UGq_diagnostics <- function(object,
                            pars = c("beta", "gamma"),
                            burnin = 0,
                            thin = 1,
                            plot = TRUE) {

  samples <- object$fit$samples

  # --------------------------------------------------
  # Extrair cadeias como mcmc.list
  # --------------------------------------------------
  mcmc_list <- mcmc.list(
    lapply(samples, function(chain) {
      mat <- as.matrix(
        chain[, grep(paste(pars, collapse = "|"),
                     colnames(chain)), drop = FALSE]
      )
      mcmc(mat)
    })
  )

  # --------------------------------------------------
  # Diagnósticos numéricos
  # --------------------------------------------------
  rhat <- gelman.diag(mcmc_list, autoburnin = FALSE)$psrf[, 1]
  ess  <- effectiveSize(mcmc_list)

  diagnostics <- list(
    Rhat = round(rhat, 4),
    ESS  = round(ess, 1)
  )

  # --------------------------------------------------
  # Gráficos
  # --------------------------------------------------
  plots <- NULL

  if (plot) {

    plots <- list(
      trace = mcmc_trace(mcmc_list),
      density = mcmc_dens(mcmc_list)
    )

    print(plots$trace)
    print(plots$density)
  }

  structure(
    list(
      diagnostics = diagnostics,
      plots = plots,
      pars = pars
    ),
    class = "UGq_diagnostics"
  )
}

print.UGq_diagnostics <- function(x, ...) {

  cat("\nConvergence diagnostics (UGq model)\n\n")

  cat("R-hat (Gelman–Rubin):\n")
  print(x$diagnostics$Rhat)

  cat("\nEffective Sample Size (ESS):\n")
  print(x$diagnostics$ESS)

  invisible(x)
}


