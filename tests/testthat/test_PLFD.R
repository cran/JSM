 cat("\nTests for 'PLFD (jmodelMult)'")

 myEps <- .Machine$double.eps
 
fitLME <- lme(sqrt(CD4) ~ bs(obstime, 4), random =~ 1 | ID, data = aids)
fitCOX <- coxph(Surv(start, stop, event) ~ drug, data = aids, x = TRUE)
control <- list( max.iter = 50, nknot = 5, SE.method ='PLFD', tol.L = 1e-08, tol.P = 1e-05)

# We use increased tolerances as this procedure is very susceptible to numeric noise. 

test_that(" basic PLFD jmodelMult test with for aids data model = 2, rho = 1 ", { 
  m_MULT <- jmodelMult(fitLME, fitCOX, aids, model = 2, rho = 1, control = control)
  expect_equal( mean (m_MULT$est.bi), 0.99979161336117106, tolerance = (10^13)*myEps, scale = 1)
  expect_equal( mean (m_MULT$Vcov), 0.00198699228755987, tolerance = (10^13)*myEps, scale = 1)
})

 cat("\nTests for 'PLFD (jmodelTM)'")

fitLME <- lme(sqrt(CD4) ~ drug + obstime + I(obstime^2) + drug:obstime + drug:I(obstime^2), random = ~ 1 | ID, data = aids)
fitCOX <- coxph(Surv(start, stop, event) ~ drug, data = aids, x = TRUE)
control <- list(nknot = 5, SE.method = 'PLFD', tol.L = 1e-08, tol.P = 1e-05)

test_that(" basic PLFD jmodelTM test with for aids data model = 2, rho = 1 ", {
  m_TM <- jmodelTM(fitLME, fitCOX, aids, timeVarY = 'obstime', control= control, model=2, rho=1)
  expect_equal( mean (m_TM$est.bi), -0.000700231727253, tolerance = (10^9)*myEps, scale = 1)
  expect_equal( mean (m_TM$Vcov), 0.000683913958988, tolerance = (10^9)*myEps, scale = 1)
})


