model1 <- c('Y ~  X
            X ~ .3 * Y')

model2 <- c('Y ~  X
            X ~ .3 * Y
            X ~~ Y')

model3 <- c('Y ~  X
             X ~   Y
             X ~~ Y')

model4 <- c('Y ~  X
             X ~  Y + Z
             Z ~ L + M
             X ~~ Y
             L ~~ M
             M ~~ X')

model5 <- c(' Y ~ t*X')

test_that("lavaan input is needed", {

  expect_error(effectTable("x+y"))
  expect_error(effectTable("potato"))

})

test_that("residual covariance information is correct", {

  expect_null(effectTable(model1)$ResidualCovariance$Syntax)
  expect_null(effectTable(model1)$ResidualCovariance$Variables)

  expect_equal(effectTable(model2)$ResidualCovariance$Syntax, 'Y~~RCovYX*X')
  expect_equal(effectTable(model2)$ResidualCovariance$Variables, data.frame(V1 = "Y", V2 = "X", name = "RCovYX"))

  expect_equal(effectTable(model3)$ResidualCovariance$Syntax, effectTable(model2)$ResidualCovariance$Syntax)
  expect_equal(effectTable(model3)$ResidualCovariance$Variables, effectTable(model2)$ResidualCovariance$Variables)



  expect_equal(effectTable(model4)$ResidualCovariance$Syntax, c('Y~~RCovYX*X',
                                                                'L~~RCovLM*M',
                                                                'X~~RCovXM*M'))
  expect_equal(effectTable(model4)$ResidualCovariance$Variables,
               data.frame(V1 = c("Y", "L", "X"), V2 = c("X", "M", "M"),
                          name = c("RCovYX", "RCovLM", "RCovXM")))
  })

test_that("effect table is correct", {

 expect_equal(effectTable(model1)$CLEffectTable, data.frame(predictor = c("X", "Y"),
                                               outcome = c("Y", "X"),
                                               name = c("CLXY", "0.3"),
                                               estimate = c("Yes", "No")))

 expect_equal(effectTable(model1)$CLEffectTable, effectTable(model2)$CLEffectTable)

 expect_equal(effectTable(model3)$CLEffectTable, data.frame(predictor = c("X", "Y"),
                                                            outcome = c("Y", "X"),
                                                            name = c("CLXY", "CLYX"),
                                                            estimate = c("Yes", "Yes")))

 expect_equal(effectTable(model4)$CLEffectTable, data.frame(predictor = c("X", "Y", "Z", "L", "M"),
                                                            outcome = c("Y", "X","X", "Z", "Z"),
                                                            name = c("CLXY", "CLYX", "CLZX", "CLLZ", "CLMZ"),
                                                            estimate = c("Yes", "Yes", "Yes", "Yes", "Yes")))

})

test_that("you can name estimated CL paths", {
  expect_equal(effectTable(model5)$CLEffectTable , data.frame(predictor = "X",
                                                              outcome = "Y",
                                                              name =  "t",
                                                              estimate = "Yes"))
})




