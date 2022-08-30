model1 <- c('Y ~  X
            X ~ .3 * Y')
effects1 <- effectTable(model1)$CLEffectTable
effects1$predictor <- paste0(effects1$predictor, "_0")
use1 <- c("X", "Y")
bp1 <- matrix(c("ARX", "0.3", "CLXY", "ARY"), ncol = 2)
colnames(bp1) <- c("X", "Y")
rownames(bp1) <- c("X_0", "Y_0")


##
model3 <- c('Y ~  X
             X ~  Y
             X ~~ Y')
effects3 <- effectTable(model3)$CLEffectTable
effects3$predictor <- paste0(effects3$predictor, "_0")


bp3 <- matrix(c("ARX", "CLYX", "CLXY", "ARY"), ncol = 2)
colnames(bp3) <- c("X", "Y")
rownames(bp3) <- c("X_0", "Y_0")


##
model5 <- c(' Y ~ t*X')
effects5 <- effectTable(model5)$CLEffectTable
effects5$predictor <- paste0(effects5$predictor, "_0")
bp5 <- matrix(c("ARX", "0", "t", "ARY"), ncol = 2)
colnames(bp5) <- c("X", "Y")
rownames(bp5) <- c("X_0", "Y_0")



test_that("blueprint function exports a matrix", {
  expect_equal(is.matrix(blueprint(effects = effects1, use = use1)), TRUE)
  expect_equal(is.matrix(blueprint(effects = effects3, use = use1)), TRUE)
  expect_equal(is.matrix(blueprint(effects = effects5, use = use1)), TRUE)
})


test_that("blueprint has right number of variables", {
  expect_equal(ncol(blueprint(effects = effects1, use = use1)), length(use1))
  expect_equal(ncol(blueprint(effects = effects3, use = use1)), length(use1))
  expect_equal(ncol(blueprint(effects = effects5, use = use1)), length(use1))
})

test_that("blueprint is correct", {
  expect_equal(blueprint(effects = effects1, use = use1), bp1)
  expect_equal(blueprint(effects = effects3, use = use1), bp3)
})

test_that("you can name CL effects", {
  expect_equal(blueprint(effects = effects5, use = use1), bp5)
})

