summary.UGq_fit <- function(object,
                            pars = c("beta", "gamma"),
                            prob = c(0.025, 0.975),
                            use.chains = c("all", "separate"),
                            digits = 4,
                            ...) {

  use.chains <- match.arg(use.chains)
  samples <- object$fit$samples

  summarize_matrix <- function(mat) {
    out <- cbind(
      Mean   = colMeans(mat),
      Median = apply(mat, 2, median),
      SD     = apply(mat, 2, sd),
      Qlow   = apply(mat, 2, quantile, prob[1]),
      Qhigh  = apply(mat, 2, quantile, prob[2])
    )
    round(out, digits)
  }

  # --------------------------------------------------
  # Cadeias combinadas
  # --------------------------------------------------
  if (use.chains == "all") {

    mat <- do.call(
      rbind,
      lapply(samples, function(x) {
        as.matrix(
          x[, grep(paste(pars, collapse = "|"),
                   colnames(x)), drop = FALSE]
        )
      })
    )

    res <- summarize_matrix(mat)

    structure(
      list(
        call = object$formula,
        family = object$family,
        estimates = res,
        prob = prob,
        chains = "combined"
      ),
      class = "summary.UGq_fit"
    )
  } else {

    # --------------------------------------------------
    # Cadeias separadas
    # --------------------------------------------------
    res <- lapply(samples, function(x) {
      mat <- as.matrix(
        x[, grep(paste(pars, collapse = "|"),
                 colnames(x)), drop = FALSE]
      )
      summarize_matrix(mat)
    })

    names(res) <- paste0("chain_", seq_along(res))

    structure(
      list(
        call = object$formula,
        family = object$family,
        estimates = res,
        prob = prob,
        chains = "separate"
      ),
      class = "summary.UGq_fit"
    )
  }
}

print.summary.UGq_fit <- function(x, ...) {

  cat("\nUnified UGq Regression Model\n")
  cat("Family:", x$family$family, "\n")
  cat("Tau:", x$family$tau, "\n\n")

  cat("Formulas:\n")
  cat("  mu    :", deparse(x$call$mu), "\n")
  cat("  sigma :", deparse(x$call$sigma), "\n\n")

  cat("Posterior estimates",
      if (x$chains == "combined")
        "(chains combined)\n\n"
      else
        "(chains shown separately)\n\n")

  if (x$chains == "combined") {
    print(x$estimates)
  } else {
    for (nm in names(x$estimates)) {
      cat("\n", nm, "\n", sep = "")
      print(x$estimates[[nm]])
    }
  }

  invisible(x)
}

