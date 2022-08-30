# Create a sample covariance matrix with known properties

GetSampleCov <- function(BPop, cnames){# Beta Matrix needs to be in regression format
  # rows are causes and columns are effects

  Sigma1 <- diag(ncol(BPop))
  mat_res <- list(NULL)

  for (j in 1:1000){
    psi <- diag(1 - diag(t(BPop) %*% Sigma1 %*% BPop), ncol(BPop)) # Creates a psi matrix
    # so the variance = 1
    if( sum(psi < 0) == 0){ # Only does the following steps if the psi values are
      # positive
      Sigma2 <- t(BPop) %*% Sigma1 %*% BPop + psi
      Sigma1 <- Sigma2
      mat_res[[j]] <- Sigma2
      #  #print(Sigma2)
    }
    else{}
  }
  if(length(mat_res) == 1000){ # Checks if the previous for loop returned 1000
    # matrices which is expected if we have a stable
    # process
    rownames(Sigma2) <- colnames(Sigma2) <- cnames
    return(Sigma2)}
  else{print("Non-stationary")}
}

# 2 variable set up
BPop2 <- matrix(c(.3,.3,
                  0,.3), ncol = 2, nrow = 2, byrow = TRUE)
colnames(BPop2 ) <- c("X", "Y")
rownames(BPop2 ) <- c("X_0", "Y_0")
sampleCov2 <- GetSampleCov(BPop2, c("X", "Y"))

model2 <-  'Y ~ X'
stability <- data.frame(X = .3, Y = 0.33)


# 3 variable set up
BPop3 <- matrix(c(.3,.3, .3,
                  0,.3, .3,
                  0, 0, .3), ncol = 3, nrow = 3, byrow = TRUE)
colnames(BPop3) <- c("X", "Y", "Z")
rownames(BPop3) <- c("X_0", "Y_0", "Z_0")
sampleCov3 <- GetSampleCov(BPop3, c("X", "Y", "Z"))


model3 <-  'Y ~ X
            Z ~ X + Y'
stability3 <- data.frame(X = .3, Y = 0.33, Z = .4)



# data setup
dat <- data.frame(Y = rnorm(500, 0, 1), X = rnorm(500, 0, 1), Z = rnorm(500, 0, 1))


# tests
test_that("Not specifying proper data inputs throws an error", {

  expect_error(stim(S = sampleCov2, model = model2 , stability = stability2))
  expect_error(stim(n = 1000, effects = model2, stability = stability2))
  expect_error(stim(S = sampleCov2, n = "one thousand", effects = model2, stability = stability2))

})


test_that("stim function returns correct solution with 2 variables", {

  ModelFit2 <- stim(S = sampleCov2, n = 1000, model = model2, stability = stability)
  lavaanSolution2 <- t(round(lavaan::inspect(ModelFit2$lavaanObjects[[1]], what = "std")$lambda, 1))
  expect_equal(unclass(lavaanSolution2), BPop2)
})


test_that("stim function returns correct solution with 2 variables", {

  ModelFit2 <- stim(S = sampleCov2, n = 1000, model = model2, stability = stability)
  lavaanSolution2 <- t(round(lavaan::inspect(ModelFit2$lavaanObjects[[1]], what = "std")$lambda, 1))
  expect_equal(unclass(lavaanSolution2), BPop2)
})


test_that("stim function returns correct solution with 3 variables", {

  ModelFit3 <- stim(S = sampleCov3, n = 1000, model = model3, stability = stability3)
  lavaanSolution3 <- t(round(lavaan::inspect(ModelFit3$lavaanObjects[[1]], what = "std")$lambda, 1))
  expect_equal(unclass(lavaanSolution3), BPop3)
})


test_that("If the number of estimated parameters exceeds degrees of freedom, function will produce an error ", {

  tooManyEffects <-  data.frame(predictor = c("X", "Y"), outcome = c("Y", "X"), name = c("CLxy", "CLyx"))
  expect_error(stim(S = sampleCov2, n = 1000, effects = tooManyEffects, stability = stability2))

})
