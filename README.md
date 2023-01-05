
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/AnnaWysocki/stim/actions/workflows/r.yml/badge.svg)](https://github.com/AnnaWysocki/stim/actions/workflows/r.yml)
<!-- badges: end --> \# stim

The **stim** package fits the Stability Informed Model which
incorporates variable stability–how a variable correlates with future
versions of itself–into cross-sectional estimates. Assuming the process
is stationary, the model is specified correctly, and the stability
values are correct, the Stability Informed Model can estimate parameters
that are unbiased for cross-lagged (longitudinal) effects when only
cross-sectional data are available.

For more information on the Stability Informed Model see
<https://psyarxiv.com/vg5as>

This tutorial outlines how to estimate a Stability Informed Model using
the **stim** package within an SEM framework.

## Installation

You can install the development version of **stim** from GitHub with

``` r
devtools::install_github("https://github.com/AnnaWysocki/stim")
```

## Example

Let’s create some data to use in our example.

``` r
library(stim)
S <-  matrix(c(1, .3, .3,
              .3,  1, .3,
              .3, .3,  1), nrow = 3, ncol = 3)
example_data <- as.data.frame(MASS::mvrnorm(n = 300, mu = rep(0, 3), Sigma =  S,
                              empirical = TRUE))
# Add column names to dataset
colnames(example_data) <- c("X", "Y", "Z")
```

## stim Function Overview

Estimate a single or a set of Stability Informed Models using the
`stim()` function.

`stim()` has five arguments

- data : A data frame with the measured variables. Not needed if S is
  provided.
- S: A covariance matrix for the measured variables. Not needed if data
  is provided.
- n: Number of observations in data set. Not needed if data is provided.
- model: An object with the cross-sectional model specified in lavaan
  syntax.
- stability: an object with the stability information for each variable
  in the model

More details on the `model` and `stability` arguments can be found
below.

### The `model` Argument

Input an object with the cross-sectional model specified in lavaan
syntax. The model syntax should be specified as a cross-sectional path
model in lavaan (See <https://lavaan.ugent.be/tutorial/tutorial.pdf> for
information on lavaan syntax).

This input determines what parameters/effects are estimated. Note, the
Stability Informed Model can estimate a maximum of (p \* (p -1))/2
parameters (where p is the number of measured variables). These
parameters can be, for example, cross-lagged effects or residual
covariances.

To estimate the effect of X on Y, I could create the following object

``` r
model <- 'Y ~ X' # outcome ~ predictor
```

More complex models can be specified as well.

``` r
model2 <- 'Y ~ X
           Z ~ X + Y'
```

The default is to constrain all residual covariances to 0. But this
constraint can be relaxed by specifying a residual covariance in the
model syntax.

``` r
model2 <- 'Y ~ X
           Z ~ X + Y
          
           X ~~ Y' # Allows X and Y to have covarying residuals
```

The above model object specifies 4 estimated parameters, but, with 3
measured variables, the Stability Informed Model can only estimate 3
parameters. The remaining effects can either be fixed to 0 or fixed to a
non-zero value.

``` r
model2 <- 'Y ~ .6 * X  # fix effect of X on Y to .6
           Z ~ X + Y
          
           X ~~  Y' 
```

Labels can be specified for the estimated parameters.

``` r
model2 <- 'Y ~ .6 * X 
           Z ~ Effect1 * X + Y # label the estimated effect of X on Z
          
           X ~~ Y'
```

If no label is specified for a cross-lagged parameter, the default label
is ‘CL’ and a subscript with the predictor name and the outcome name.

Residual covariances are labeled ‘RCov’ and a subscript with the names
of the two variable whose residuals are covarying.

### Inputs for the `stability` Argument

Input a object with the stability information for each variable in the
model.

To fit `model2`, the stability input should have a stability value for
X, Y, and Z.

``` r
stability <- c(X = .5, Y = .1, Z = .1)
```

The elements or columns in the stability object need to be named, and
the names must match the variable names in the `data` or `S` input.

Multiple stability values can be specified for each variable. This
results in multiple Stability Informed Models being estimated (one for
each stability condition).

``` r
stability <- data.frame(X = c(.5, .55), Y = c(.1, .15), Z = c(.1, .2))
rownames(stability) <- c("Model 1", "Model 2")
stability
#>            X    Y   Z
#> Model 1 0.50 0.10 0.1
#> Model 2 0.55 0.15 0.2
```

If this is the `stability` input, two models will be estimated. One
model where the stability values for X, Y, and Z are .5, .1, and .1,
respectively, and one where the stability values for X, Y, and Z are
.55, .15, and .2, respectively.

### Estimate the Stability Informed Model

``` r
modelFit <- stim(data = example_data, model = model2, stability = stability) 
#> StIM: Stability Informed Models 
#> -------------------------------------
#> -------------------------------------
#> 
#> Variables (p): 3 
#> Sample Size (n): 300 
#> Estimated Parameters (q): 3 
#> Degrees of Freedom: 0 
#> Number of Models Estimated: 2 
#> 
#> -------------------------------------
```

Instead of the data input, I could also use the covariance matrix and
sample size inputs.

``` r
modelFit <- stim(S = cov(example_data), n = nrow(example_data), model = model2, stability = stability) 
```

Some information about the model(s) is automatically printed out when
the `stim()` function is run. The output from this function is an object
of type `stim`. When the `summary()` function is used on a `stim`
object, a summary of the estimated Stability Informed Models will be
printed.

``` r
summary(modelFit)
#> StIM: Stability Informed Models 
#> -------------------------------------
#> -------------------------------------
#> 
#> Variables (p): 3 
#> Sample Size (n): 300 
#> Estimated Parameters (q): 3 
#> Degrees of Freedom: 0 
#> 
#> -------------------------------------
#> Model 1 
#> 
#> Stability:
#>    X   Y   Z
#>  0.5 0.1 0.1
#> 
#>  Autoregressive Effects:
#>         ARX         ARY         ARZ 
#>  0.50083542 -0.07953265 -0.24509220 
#> 
#>  Cross Lagged Effects:
#>   Effect Estimate Standard.Error P.Value
#>  Effect1    0.466          0.213   0.029
#>     CLYZ    0.687          1.127   0.542
#> 
#>  Residual Covariances:
#>  Effect Estimate Standard.Error P.Value
#>  RCovYX    0.011          0.068   0.873
#> 
#> -------------------------------------
#>  Model 2 
#> 
#> Stability:
#>     X    Y   Z
#>  0.55 0.15 0.2
#> 
#>  Autoregressive Effects:
#>         ARX         ARY         ARZ 
#>  0.55091896 -0.02944912 -0.16022887 
#> 
#>  Cross Lagged Effects:
#>   Effect Estimate Standard.Error P.Value
#>  Effect1    0.330          0.643   0.608
#>     CLYZ    0.874          3.049   0.774
#> 
#>  Residual Covariances:
#>  Effect Estimate Standard.Error P.Value
#>  RCovYX   -0.026          0.067   0.696
#> 
#> -------------------------------------
#> 
```

### Eploring the `stim` Output Object

The object `modelFit` contains a list with information for the Stability
Informed Model

- n: Sample size
- p: Number of measured variables used in the Stability Informed Model
- q: Number of estimated parameters
- df: Degrees of freedom
- stability : A table of the stability conditions
- CLEffectTable : A table with information on which cross-lagged effects
  are estimated
- CLMatrices: A list of matrices (1 for each Stability Informed Model
  that was estimated) with the estimated cross-lagged effects and their
  associated standard errors and p-values.
- RCovMatrices: A list of matrices (1 for each Stability Informed Model
  that was estimated) with the estimated residual covariances and their
  associated standard errors and p-values.
- ARVector: A list of vectors (1 for each Stability Informed Model that
  was estimated) with the values for the auto-regressive effects.
- lavaanObjects : A list of lavaan objects (1 for each Stability
  Informed Model that was estimated)
- NoWarnings : A vector with information on whether there were any
  errors or warnings for each of the estimated models
- CSModelSyntax : The user-specified model syntax (input for model
  argument)
- SIMSyntax : The syntax for the Stability Informed Model–model syntax
  for the lavaan function
- modelImpliedEquations : Model implied equations for the latent
  covariances and auto-regressive effects

#### stability

A table of the stability conditions. Each row contains the stability
information for one Stability Informed Model.

``` r
modelFit$stability 
#>            X    Y   Z
#> Model 1 0.50 0.10 0.1
#> Model 2 0.55 0.15 0.2
```

#### CLEffectTable

A table with information on the cross-lagged paths. It has the predictor
and outcome names, cross-lagged effect labels, and whether the
cross-lagged path is estimated or constrained.

``` r
modelFit$CLEffectTable 
#>   predictor outcome    name estimate
#> 1       X_0       Y     0.6       No
#> 2       X_0       Z Effect1      Yes
#> 3       Y_0       Z    CLYZ      Yes
```

#### CLMatrices

A list of matrices with the estimated cross-lagged effects and standard
errors and p-values for each of the estimated cross-lagged effects. Each
matrix corresponds to one of the estimated Stability Informed Models.

``` r
modelFit$CLMatrices 
#> $Model1
#>    Effect Estimate Standard.Error P.Value
#> 3 Effect1    0.466          0.213   0.029
#> 6    CLYZ    0.687          1.127   0.542
#> 
#> $Model2
#>    Effect Estimate Standard.Error P.Value
#> 3 Effect1    0.330          0.643   0.608
#> 6    CLYZ    0.874          3.049   0.774
```

#### RCovMatrices

A list of matrices with the estimated residual covariances and their
standard errors and p-values. Each matrix corresponds to one of the
estimated Stability Informed Models.

``` r
modelFit$RCovMatrices 
#> $Model1
#>    Effect Estimate Standard.Error P.Value
#> 16 RCovYX    0.011          0.068   0.873
#> 
#> $Model2
#>    Effect Estimate Standard.Error P.Value
#> 16 RCovYX   -0.026          0.067   0.696
```

#### ARVector

A list of vectors with the values for each auto-regressive effect. Each
vector corresponds to one of the estimated Stability Informed Models.

``` r
modelFit$ARVector
#> $Model1
#>         ARX         ARY         ARZ 
#>  0.50083542 -0.07953265 -0.24509220 
#> 
#> $Model2
#>         ARX         ARY         ARZ 
#>  0.55091896 -0.02944912 -0.16022887
```

#### lavaanObjects

A list of lavaan objects (1 for each Stability Informed Model)

To output the lavaan object easily, you can use the
`lavaanSummary() function`

``` r
lavaanSummary(modelFit)
#> Model 1 
#> lavaan 0.6-12 ended normally after 168 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        12
#> 
#>   Number of observations                           300
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   X_0 =~                                              
#>     X        (ARX)    0.500       NA                  
#>     Y                 0.600                           
#>     Z       (Eff1)    0.466    0.213    2.184    0.029
#>   Y_0 =~                                              
#>     X                 0.000                           
#>     Y        (ARY)   -0.079    0.036   -2.203    0.028
#>     Z       (CLYZ)    0.687    1.127    0.610    0.542
#>   Z_0 =~                                              
#>     X                 0.000                           
#>     Y                 0.000                           
#>     Z        (ARZ)   -0.245    0.292   -0.838    0.402
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   X_0 ~~                                              
#>     Y_0     (CvXY)    0.299    0.060    4.977    0.000
#>     Z_0     (CvXZ)    0.299    0.060    4.977    0.000
#>   Y_0 ~~                                              
#>     Z_0     (CvYZ)    0.299    0.060    4.977    0.000
#>  .X ~~                                                
#>    .Y       (RCYX)    0.011    0.068    0.160    0.873
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     X_0     (VarX)    1.000                           
#>     Y_0     (VarY)    1.000                           
#>     Z_0     (VarZ)    1.000                           
#>    .X                 0.747    0.081    9.175    0.000
#>    .Y                 0.659    0.087    7.535    0.000
#>    .Z                 0.225    1.430    0.157    0.875
#> 
#> Constraints:
#>                                                |Slack|
#>     ARX - (0.5/VarX)                             0.000
#>     ARY - ((0.1-0.6*CovXY)/VarY)                 0.000
#>     ARZ-((0.1-(Effect1*CovXZ+CLYZ*CvYZ))/VrZ)    0.000
#>     CovXY - ((0.6*VarX+ARY*CovXY)*ARX+RCovYX)    0.000
#>     CvXZ-((Effct1*VrX+CLYZ*CvXY+ARZ*CXZ)*ARX)    0.000
#>     CYZ-(0.6*(E1*VX+CLYZ*CXY+ARZ*CXZ)+(E1*CXY    0.000
#> 
#> Model 2 
#> lavaan 0.6-12 ended normally after 188 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        12
#> 
#>   Number of observations                           300
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   X_0 =~                                              
#>     X        (ARX)    0.550       NA                  
#>     Y                 0.600                           
#>     Z       (Eff1)    0.330    0.643    0.513    0.608
#>   Y_0 =~                                              
#>     X                 0.000                           
#>     Y        (ARY)   -0.029    0.036   -0.816    0.415
#>     Z       (CLYZ)    0.874    3.049    0.286    0.774
#>   Z_0 =~                                              
#>     X                 0.000                           
#>     Y                 0.000                           
#>     Z        (ARZ)   -0.160    0.704   -0.227    0.820
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   X_0 ~~                                              
#>     Y_0     (CvXY)    0.299    0.060    4.977    0.000
#>     Z_0     (CvXZ)    0.299    0.060    4.977    0.000
#>   Y_0 ~~                                              
#>     Z_0     (CvYZ)    0.299    0.060    4.977    0.000
#>  .X ~~                                                
#>    .Y       (RCYX)   -0.026    0.067   -0.391    0.696
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     X_0     (VarX)    1.000                           
#>     Y_0     (VarY)    1.000                           
#>     Z_0     (VarZ)    1.000                           
#>    .X                 0.694    0.081    8.530    0.000
#>    .Y                 0.646    0.087    7.392    0.000
#>    .Z                 0.041    4.668    0.009    0.993
#> 
#> Constraints:
#>                                                |Slack|
#>     ARX - (0.55/VarX)                            0.000
#>     ARY - ((0.15-0.6*CovXY)/VarY)                0.000
#>     ARZ-((0.2-(Effect1*CovXZ+CLYZ*CvYZ))/VrZ)    0.000
#>     CovXY - ((0.6*VarX+ARY*CovXY)*ARX+RCovYX)    0.000
#>     CvXZ-((Effct1*VrX+CLYZ*CvXY+ARZ*CXZ)*ARX)    0.000
#>     CYZ-(0.6*(E1*VX+CLYZ*CXY+ARZ*CXZ)+(E1*CXY    0.000
```

You can also print a subset of the lavaan objects by using the `subset`
argument.

``` r
lavaanSummary(modelFit, subset = 1)
#> lavaan 0.6-12 ended normally after 168 iterations
#> 
#>   Estimator                                         ML
#>   Optimization method                           NLMINB
#>   Number of model parameters                        12
#> 
#>   Number of observations                           300
#> 
#> Model Test User Model:
#>                                                       
#>   Test statistic                                 0.000
#>   Degrees of freedom                                 0
#> 
#> Parameter Estimates:
#> 
#>   Standard errors                             Standard
#>   Information                                 Expected
#>   Information saturated (h1) model          Structured
#> 
#> Latent Variables:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   X_0 =~                                              
#>     X        (ARX)    0.500       NA                  
#>     Y                 0.600                           
#>     Z       (Eff1)    0.466    0.213    2.184    0.029
#>   Y_0 =~                                              
#>     X                 0.000                           
#>     Y        (ARY)   -0.079    0.036   -2.203    0.028
#>     Z       (CLYZ)    0.687    1.127    0.610    0.542
#>   Z_0 =~                                              
#>     X                 0.000                           
#>     Y                 0.000                           
#>     Z        (ARZ)   -0.245    0.292   -0.838    0.402
#> 
#> Covariances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>   X_0 ~~                                              
#>     Y_0     (CvXY)    0.299    0.060    4.977    0.000
#>     Z_0     (CvXZ)    0.299    0.060    4.977    0.000
#>   Y_0 ~~                                              
#>     Z_0     (CvYZ)    0.299    0.060    4.977    0.000
#>  .X ~~                                                
#>    .Y       (RCYX)    0.011    0.068    0.160    0.873
#> 
#> Variances:
#>                    Estimate  Std.Err  z-value  P(>|z|)
#>     X_0     (VarX)    1.000                           
#>     Y_0     (VarY)    1.000                           
#>     Z_0     (VarZ)    1.000                           
#>    .X                 0.747    0.081    9.175    0.000
#>    .Y                 0.659    0.087    7.535    0.000
#>    .Z                 0.225    1.430    0.157    0.875
#> 
#> Constraints:
#>                                                |Slack|
#>     ARX - (0.5/VarX)                             0.000
#>     ARY - ((0.1-0.6*CovXY)/VarY)                 0.000
#>     ARZ-((0.1-(Effect1*CovXZ+CLYZ*CvYZ))/VrZ)    0.000
#>     CovXY - ((0.6*VarX+ARY*CovXY)*ARX+RCovYX)    0.000
#>     CvXZ-((Effct1*VrX+CLYZ*CvXY+ARZ*CXZ)*ARX)    0.000
#>     CYZ-(0.6*(E1*VX+CLYZ*CXY+ARZ*CXZ)+(E1*CXY    0.000
```

#### NoWarnings

A vector with logical information on whether there were any errors or
warnings for each of the estimated models.

TRUE means no warnings FALSE means warnings.

``` r
modelFit$NoWarnings # Means no warnings for both models
#> [1] TRUE TRUE
```

#### CSModelSyntax

The user-specified model syntax (input for model argument)

``` r
modelFit$CSModelSyntax 
#> [1] "Y ~ .6 * X \n           Z ~ Effect1 * X + Y # label the estimated effect of X on Z\n          \n           X ~~ Y"
```

#### SIMSyntax

The syntax for the Stability Informed Model–model syntax for the lavaan
function. This contains the syntax to specify the structural part of the
Stability Informed Model as well as the parameter constraints for the
auto-regressive paths and the latent correlations

``` r
modelFit$SIMSyntax 
#> [[1]]
#>  [1] "X_0=~ARX*X+0.6*Y+Effect1*Z"                                                            
#>  [2] "Y_0=~0*X+ARY*Y+CLYZ*Z"                                                                 
#>  [3] "Z_0=~0*X+0*Y+ARZ*Z"                                                                    
#>  [4] "X_0~~ CovXY*Y_0"                                                                       
#>  [5] "X_0~~ CovXZ*Z_0"                                                                       
#>  [6] "Y_0~~ CovYZ*Z_0"                                                                       
#>  [7] "X_0~~VarX*X_0"                                                                         
#>  [8] "Y_0~~VarY*Y_0"                                                                         
#>  [9] "Z_0~~VarZ*Z_0"                                                                         
#> [10] "Y~~RCovYX*X"                                                                           
#> [11] "ARX==0.5/VarX"                                                                         
#> [12] "ARY==(0.1-0.6*CovXY)/VarY"                                                             
#> [13] "ARZ==(0.1-(Effect1*CovXZ+CLYZ*CovYZ))/VarZ"                                            
#> [14] "CovXY==(0.6*VarX+ARY*CovXY)*ARX+RCovYX"                                                
#> [15] "CovXZ==(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)*ARX"                                        
#> [16] "CovYZ==0.6*(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)+(Effect1*CovXY+CLYZ*VarY+ARZ*CovYZ)*ARY"
#> 
#> [[2]]
#>  [1] "X_0=~ARX*X+0.6*Y+Effect1*Z"                                                            
#>  [2] "Y_0=~0*X+ARY*Y+CLYZ*Z"                                                                 
#>  [3] "Z_0=~0*X+0*Y+ARZ*Z"                                                                    
#>  [4] "X_0~~ CovXY*Y_0"                                                                       
#>  [5] "X_0~~ CovXZ*Z_0"                                                                       
#>  [6] "Y_0~~ CovYZ*Z_0"                                                                       
#>  [7] "X_0~~VarX*X_0"                                                                         
#>  [8] "Y_0~~VarY*Y_0"                                                                         
#>  [9] "Z_0~~VarZ*Z_0"                                                                         
#> [10] "Y~~RCovYX*X"                                                                           
#> [11] "ARX==0.55/VarX"                                                                        
#> [12] "ARY==(0.15-0.6*CovXY)/VarY"                                                            
#> [13] "ARZ==(0.2-(Effect1*CovXZ+CLYZ*CovYZ))/VarZ"                                            
#> [14] "CovXY==(0.6*VarX+ARY*CovXY)*ARX+RCovYX"                                                
#> [15] "CovXZ==(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)*ARX"                                        
#> [16] "CovYZ==0.6*(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)+(Effect1*CovXY+CLYZ*VarY+ARZ*CovYZ)*ARY"
```

#### modelImpliedEquations

Model implied equations for the latent covariances and auto-regressive
paths

``` r
modelFit$modelImpliedEquations 
#> [[1]]
#> [1] "ARX==0.5/VarX"                                                                         
#> [2] "ARY==(0.1-0.6*CovXY)/VarY"                                                             
#> [3] "ARZ==(0.1-(Effect1*CovXZ+CLYZ*CovYZ))/VarZ"                                            
#> [4] "CovXY==(0.6*VarX+ARY*CovXY)*ARX+RCovYX"                                                
#> [5] "CovXZ==(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)*ARX"                                        
#> [6] "CovYZ==0.6*(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)+(Effect1*CovXY+CLYZ*VarY+ARZ*CovYZ)*ARY"
#> 
#> [[2]]
#> [1] "ARX==0.55/VarX"                                                                        
#> [2] "ARY==(0.15-0.6*CovXY)/VarY"                                                            
#> [3] "ARZ==(0.2-(Effect1*CovXZ+CLYZ*CovYZ))/VarZ"                                            
#> [4] "CovXY==(0.6*VarX+ARY*CovXY)*ARX+RCovYX"                                                
#> [5] "CovXZ==(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)*ARX"                                        
#> [6] "CovYZ==0.6*(Effect1*VarX+CLYZ*CovXY+ARZ*CovXZ)+(Effect1*CovXY+CLYZ*VarY+ARZ*CovYZ)*ARY"
```
