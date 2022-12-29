

# stim

```{r, include = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
```


The `stim` package fits the Stability Informed Model which incorporates variable stability--how a variable correlates with future versions of itself--into cross-sectional estimates.
Assuming the process is stationary, the model is specified correctly, and the stability values are correct, the Stability Informed Model can estimate parameters that are unbiased for cross-lagged (longitudinal) effects even when only cross-sectional data are available. 

For more information on the Stability Informed Model see https://psyarxiv.com/vg5as

This tutorial outlines how to estimate a Stability Informed Model using the `stim` package within an SEM framework.  

## Installation

You can install the development version of `stim` from GitHub with

```{r installation, eval = FALSE}
devtools::install_github("https://github.com/AnnaWysocki/stim")
```

## Example 

Let's create some data to use in our example.

```{r data}
library(stim)
S <-  matrix(c(1, .3, .3,
              .3,  1, .3,
              .3, .3,  1), nrow = 3, ncol = 3)
example_data <- as.data.frame(MASS::mvrnorm(n = 300, mu = rep(0, 3), Sigma =  S,
                              empirical = TRUE))
# Add column names to dataset
colnames(example_data) <- c("X", "Y", "Z")
```


## `stim` Function Overview

Estimate a single or a set of Stability Informed Models using the `stim()` function.

`stim()` has five arguments

* data : A data frame with the measured variables. Not needed if S is provided.
* S: A covariance matrix for the measured variables. Not needed if data is provided. 
* n: Number of observations in data set. Not needed if data is provided. 
* model: An object with the cross-sectional model specified in lavaan syntax. 
* stability: an object with the stability information for each variable in the model 

More details on the `model` and `stability` arguments can be found below.

### The `model` Argument

Input an object with the cross-sectional model specified in lavaan syntax. \n
The model syntax should be specified as a cross-sectional path model in lavaan
(See https://lavaan.ugent.be/tutorial/tutorial.pdf for information on lavaan syntax). 

This input determines what parameters/effects are estimated. 
Note, the Stability Informed model can estimate a maximum of 
$$
\frac{p (p-1)}{2}
$$
parameters (where p is the number of measured variables).
These parameters can be, for example, cross-lagged effects or residual covariances. 

To estimate the effect of X on Y, I could create the following object

```{r modelspec1}
model <- 'Y ~ X' # outcome ~ predictor
```

More complex models can be specified as well.

```{r modelspec2}
model2 <- 'Y ~ X
           Z ~ X + Y'
```

The default is to constrain all residual covariances to 0. But this constraint can be relaxed by specifying a residual covariance in the model syntax. 

```{r modelspec3}
model2 <- 'Y ~ X
           Z ~ X + Y
          
           X ~~ Y' # Allows X and Y to have covarying residuals
```

The above model object specifies 4 estimated parameters, but, with 3 measured variables,
the Stability Informed Model can only estimate 3 parameters. The remaining effects can either be fixed to 0 or fixed to a non-zero value. 


```{r modelspec4}
model2 <- 'Y ~ .6 * X  # fix effect of X on Y to .6
           Z ~ X + Y
          
           X ~~  Y' 
```

Labels can be specified for the estimated parameters.

```{r modelspec5}
model2 <- 'Y ~ .6 * X 
           Z ~ Effect1 * X + Y # label the estimated effect of X on Z
          
           X ~~ Y'
```

If no label is specified for a cross-lagged parameter, the default label is 'CL' and a subscript with the predictor name and the outcome name. 

Residual covariances are labeled 'RCov' and a subscript with the names of the two variable  whose residuals are covarying.

### Inputs for the `stability` Argument

Input a object with the stability information for each variable in the model. 

To fit `model2`, the stability input should have a stability value for X, Y, and Z. 

```{r stability1}
stability <- c(X = .5, Y = .1, Z = .1)
```

The stability values need to be named, and the names must match the variable names in the `data` or `S` input. 

Multiple stability values can be specified for each variable. This results in multiple Stability Informed Models being estimated (one for each stability condition). 


```{r stability2}
stability <- data.frame(X = c(.5, .55), Y = c(.1, .15), Z = c(.1, .2))
rownames(stability) <- c("Model 1", "Model 2")
stability
```
If this is the `stability` input, two models will be estimated. One model where the stability values for X, Y, and Z are .5, .1, and .1, respectively, and one where the stability values for X, Y, and Z are .55, .15, and .2, respectively.

### Estimate the Stability Informed Model 

```{r stim1}
modelFit <- stim(data = example_data, model = model2, stability = stability) 
```

Instead of the data input, I could also use the covariance matrix and sample size inputs. 

```{r stim2, eval = FALSE}
modelFit <- stim(S = cov(example_data), n = nrow(example_data), model = model2, stability = stability) 
```

Some information about the model(s) is automatically printed out when the stim() function is run. 
The summary() function can be used to print out more information 

```{r}
summary(modelFit)
```

### Eploring the stim Output Object

modelFit is a stim object that contains a list of objects with information for the Stability Informed Model

* n: Sample size
* p: Number of measured variables used in the Stability Informed Model
* q: Number of estimated parameters 
* df: Degrees of freedom 
* stability : A table of the stability conditions
* CLEffectTable : A table with information on which cross-lagged effects are estimated
* CLMatrices: A list of matrices (1 for each Stability Informed Model that was estimated) with the estimated cross-lagged effects and their associated standard errors and p-values. 
* RCovMatrices: A list of matrices (1 for each Stability Informed Model that was estimated) with the estimated residual covariances and their associated standard errors and p-values.
* ARVector:  A list of vectors (1 for each Stability Informed Model that was estimated) with the values for the auto-regressive effects. 
* lavaanObjects : A list of lavaan objects (1 for each Stability Informed Model that was estimated)
* NoWarnings : A vector with information on whether there were any errors or warnings for each of the estimated models
* CSModelSyntax : The user-specified model syntax (input for model argument)
* SIMSyntax : The syntax for the Stability Informed Model--model syntax for the lavaan function
* modelImpliedEquations : Model implied equations for the latent covariances and auto-regressive effects

#### stability

A table of the stability conditions. Each row contains the stability information for 
one Stability Informed Model. 
```{r output 1}
modelFit$stability 
```

#### CLEffectTable

A table with information on the cross-lagged paths. It has the predictor and outcome names, cross-lagged effect labels, and whether the cross-lagged path is estimated or constrained. 

```{r output 2 }
modelFit$CLEffectTable 
```


#### CLMatrices 

A list of matrices with the estimated cross-lagged effects and standard errors and p-values for each of the estimated cross-lagged effects. Each matrix corresponds to one of the estimated Stability Informed Models. 

```{r output 3}
modelFit$CLMatrices 
```
#### RCovMatrices 

A list of matrices with the estimated residual covariances and their standard errors and p-values. Each matrix corresponds to one of the estimated Stability Informed Models. 

```{r output 4}
modelFit$RCovMatrices 
```

#### ARVector
A list of vectors (1 for each Stability Informed Model that was estimated) with the values for the auto-regressive effects. 

A list of vectors with the values for each auto-regressive effect. Each vector corresponds to one of the estimated Stability Informed Models. 

```{r output 5}
modelFit$ARVector
```


#### lavaanObjects 
A list of lavaan objects (1 for each Stability Informed Model)

To output the lavaan object easily, you can use the `lavaanSummary() function`
```{r output 6}
lavaanSummary(modelFit)
```

You can also print a subset of the lavaan objects by using the `subset` argument. 

```{r output 6.2}
lavaanSummary(modelFit, subset = 1)
```

####  NoWarnings 
A vector with logical information on whether there were any errors or warnings for each of the estimated models. 

FALSE means no warnings
TRUE means warnings. 
```{r output 7}
modelFit$NoWarnings # Means no warnings for both models
```

#### CSModelSyntax
The user-specified model syntax (input for model argument)

```{r output 8}
modelFit$CSModelSyntax 
```

#### SIMSyntax 

The syntax for the Stability Informed Model--model syntax for the lavaan function. 
This contains the syntax to specify the structural part of the Stability Informed Model as well as the parameter constraints for the auto-regressive paths and the latent correlations

```{r output 9}
modelFit$SIMSyntax 
```

#### modelImpliedEquations 

Model implied equations for the latent covariances and auto-regressive paths

```{r output 10}
modelFit$modelImpliedEquations 
```
