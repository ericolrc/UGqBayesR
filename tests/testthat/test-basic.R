test_that("UGq_family works", {
  fam <- UGq_family(tau = 0.3)
  expect_s3_class(fam, "UGq_family")
})


test_that("UGq_fit returns UGq_fit object", {
  data(example_data)
  fam <- UGq_family(tau = 0.1)


  fit <- UGq_fit(
    formula.mu = y ~ x1,
    data = example_data,
    family = fam,
    niter = 1000,
    nburnin = 200,
    thin = 2,
    nchains = 1
  )


  expect_s3_class(fit, "UGq_fit")
})
