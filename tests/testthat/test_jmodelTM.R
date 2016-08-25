cat("\nTests for 'jmodelTM'")

myEps <- .Machine$double.eps

fitLME <- lme(sqrt(CD4) ~ drug + obstime + I(obstime ^ 2) + drug : obstime + drug : I(obstime ^2), random = ~ 1 | ID, data = aids)
fitCOX <- coxph(Surv(start, stop, event) ~ drug, data = aids, x = TRUE)
control <- list(max.iter = 50, nknot = 3, tol.L = 1e-08, tol.P = 1e-04)

test_that(" basic jmodelTM test with for aids data model = 1, rho = 1 ", { 
  m_TM <- jmodelTM(fitLME, fitCOX, aids, model = 1, rho = 1, timeVarY = 'obstime', control = control)
  expect_equal(  m_TM$coefficients$lgLik,-2523.329037070506274, tolerance = (10^14)*myEps, scale = 1)
  expect_equal( mean (m_TM$coefficients$lamb), 4.379207628026059, tolerance = (10^13)*myEps, scale = 1)
})

test_that(" basic jmodelTM test with for aids data model = 2, rho = 1 ", { 
  m_TM <- jmodelTM(fitLME, fitCOX, aids, model = 2, rho = 1, timeVarY = 'obstime', control = control)
  expect_equal(  m_TM$coefficients$lgLik, -2522.506475889341800, tolerance = (10^14)*myEps, scale = 1)
  expect_equal( mean (m_TM$coefficients$lamb), 4.341256927560805, tolerance = (10^9)*myEps, scale = 1)
})

