# Function for creating a sample covariance matrix with known properties
GetSampleCov <- function(B, ResidualCov){# Beta Matrix needs to be in regression format
  # rows are causes and columns are effects

  Sigma1 <- diag(ncol(B))
  mat_res <- list(NULL)

  for (j in 1:1000){
    psi <- diag(1 - diag(t(B) %*% Sigma1 %*% B), ncol(B)) # Creates a psi matrix
    # so the variance = 1

    colnames(psi) <- rownames(psi) <- colnames(B)


    for(i in 1:nrow(ResidualCov)){
      psi[ResidualCov[i, 1],ResidualCov[i, 2]] <- ResidualCov[i, 3]
      psi[ResidualCov[i, 2],ResidualCov[i, 1]] <- ResidualCov[i, 3]
    }

    if(sum(diag(psi) < 0) == 0){ # Only does the following steps if the psi values are
      # positive
      Sigma2 <- t(B) %*% Sigma1 %*% B + psi
      Sigma1 <- Sigma2
      mat_res[[j]] <- Sigma2
      #  #print(Sigma2)
    }else{}
  }
  if(length(mat_res) == 1000){ # Checks if the previous for loop returned 1000
    # matrices which is expected if we have a stable
    # process
    rownames(Sigma2) <- colnames(Sigma2) <- colnames(B)

    return(Sigma2)} else{print("Non-stationary")}
} # B: True Beta Matrix
                                              # ResidualCov: True Psi Matrix


########################################
#  Tests Without Residual Covariances  #
########################################

# 2 variable set up
BPop2 <- matrix(c( .3,.3,
                   0,.3 ),
                ncol = 2, nrow = 2, byrow = TRUE,
                dimnames = list( c("X_0", "Y_0"),
                                  c("X", "Y")     ))

sampleCov2 <- GetSampleCov(BPop2, data.frame("X", "Y", 0))

model2 <-  'Y ~ X'
stability2 <- data.frame(X = .3, Y = 0.33)


# 3 variable set up
BPop3 <- matrix(c( .3,.3, .3,
                    0,.3, .3,
                    0, 0, .3 ),
                ncol = 3, nrow = 3, byrow = TRUE,
                dimnames = list( c("X_0", "Y_0", "Z_0"),
                                 c("X", "Y", "Z")       ))

sampleCov3 <- GetSampleCov(BPop3, data.frame("X", "Y", 0))


model3 <-  'Y ~ X
            Z ~ X + Y'
stability3 <- data.frame(X = .3, Y = 0.33, Z = .4)



# data setup
dat <- data.frame(Y = rnorm(500, 0, 1),
                  X = rnorm(500, 0, 1),
                  Z = rnorm(500, 0, 1))


# start tests
test_that("Not specifying proper data inputs throws an error", {

  expect_error(stim(S = sampleCov2, model = model2 , stability = stability2))
  expect_error(stim(n = 1000, model = model2, stability = stability2))
  expect_error(stim(S = sampleCov2, n = "one thousand", model = model2,
                    stability = stability2))

})


test_that("stim function returns correct solution with 2 variables", {

  ModelFit2 <- stim(S = sampleCov2, n = 1000, model = model2, stability = stability2)
  lavaanSolution2 <- t(round(lavaan::inspect(ModelFit2$lavaanObjects[[1]], what = "std")$lambda, 2))
  expect_equal(unclass(lavaanSolution2), BPop2)
})

test_that("fixing values rather than estimating works with 2 variables", {
  model2fix <- c('Y ~ .3 * X
                 Y ~~ X')

  ModelFit2Fix <- stim(S = sampleCov2, n = 1000, model = model2fix, stability = stability2)
  lavaanSolution2Fix <- t(round(lavaan::inspect(ModelFit2Fix$lavaanObjects[[1]], what = "std")$lambda, 2))
  expect_equal(unclass(lavaanSolution2Fix), BPop2)
})


test_that("stim function returns correct solution with 2 variables & multiple
          stability conditions", {
  stability2df <- data.frame(X = c(.3, .34), Y = c(0.33, .36))
  ModelFit2 <- stim(S = sampleCov2, n = 1000, model = model2, stability = stability2df)
  lavaanSolution2 <- t(round(lavaan::inspect(ModelFit2$lavaanObjects[[1]], what = "std")$lambda, 2))
  lavaanSolution2Wrong <- t(round(lavaan::inspect(ModelFit2$lavaanObjects[[2]], what = "std")$lambda, 2))
  expect_equal(unclass(lavaanSolution2), BPop2)
  expect_error(expect_equal(unclass(lavaanSolution2Wrong), BPop2))
})


test_that("stim function returns correct solution with 3 variables", {

  ModelFit3 <- stim(S = sampleCov3, n = 1000, model = model3, stability = stability3)
  lavaanSolution3 <- t(round(lavaan::inspect(ModelFit3$lavaanObjects[[1]], what = "std")$lambda, 2))
  expect_equal(unclass(lavaanSolution3), BPop3)
})

test_that("stim function returns correct solution with 3 variables and multiple
          stability conditions", {
  stability3df <- data.frame(X = c(.3, .4), Y = c(0.33, .43), Z = c(.4, .45))

  ModelFit3 <- stim(S = sampleCov3, n = 1000, model = model3, stability = stability3df)
  lavaanSolution3 <- t(round(lavaan::inspect(ModelFit3$lavaanObjects[[1]], what = "std")$lambda, 2))
  lavaanSolution3Wrong <- t(round(lavaan::inspect(ModelFit3$lavaanObjects[[2]], what = "std")$lambda, 2))
  expect_equal(unclass(lavaanSolution3), BPop3)
  expect_error(expect_equal(unclass(lavaanSolution3Wrong), BPop3))
})

test_that("fixing values rather than estimating works with 3 variables", {
  model3fix <- 'Y ~ X
            Z ~ .3 * X + Y'

  ModelFit3Fix <- stim(S = sampleCov3, n = 1000, model = model3fix, stability = stability3)
  lavaanSolution3Fix <- t(round(lavaan::inspect(ModelFit3Fix$lavaanObjects[[1]], what = "std")$lambda, 2))
  expect_equal(unclass(lavaanSolution3Fix), BPop3)
})


test_that("If the number of estimated parameters exceeds degrees of freedom, function will produce an error ", {

  tooManyEffects <-  data.frame(predictor = c("X", "Y"), outcome = c("Y", "X"), name = c("CLxy", "CLyx"))
  expect_error(stim(S = sampleCov2, n = 1000, effects = tooManyEffects, stability = stability2))

})



#######################################
#   Tests With Residual Covariance    #
#######################################

# 2 variable set up

sampleCov2Rcov <- GetSampleCov(BPop2, data.frame("X", "Y", .1))

model2Rcov <-  'Y ~ .3 * X
                X ~~  Y'
stability2Rcov <- data.frame(X = .3, Y = 0.363)


# 3 variable set up
sampleCov3Rcov <- GetSampleCov(BPop3, data.frame("X", "Y", .05))


model3Rcov <-  'Y ~ .3 *X
                Z ~ X + Y
                X ~~ Y'
stability3Rcov <- data.frame(X = .3, Y = 0.346, Z = .406)




# start tests

test_that("stim function returns correct solution with 2 variables & covarying residual", {

  ModelFit2Rcov <- stim(S = sampleCov2Rcov, n = 1000, model = model2Rcov, stability = stability2Rcov)
  lavaanSolution2Rcov <- t(round(lavaan::inspect(ModelFit2Rcov$lavaanObjects[[1]], what = "std")$lambda, 1))
  expect_equal(unclass(lavaanSolution2Rcov), BPop2)
})


test_that("stim function returns correct solution with 3 variables", {

  ModelFit3Rcov <- stim(S = sampleCov3Rcov, n = 1000, model = model3Rcov, stability = stability3Rcov)
  lavaanSolution3Rcov <- t(round(lavaan::inspect(ModelFit3Rcov$lavaanObjects[[1]], what = "std")$lambda, 1))
  expect_equal(unclass(lavaanSolution3Rcov), BPop3)

})






