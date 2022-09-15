## Terminal commands to use the function:
# formulaa = Petal.Length ~ Species
# # -> explain Petal.Length via Species and Petal.Width
# data(iris)
# lr_comps <- linreg(formulaa, data = iris)

## Function definition:
linreg <- function(formula, data){
  ## VARIABLE DEFINITION
  # Independent variable matrix:
  X <- model.matrix(formula, data)
  # Dependent variable:
  y <- data$Petal.Length
  
  ## LEAST SQUARES COMPUTATIONS
  # Regression coefficients:
  regCoeff <- solve(t(X)%*%X)%*%t(X)%*%y
  
  # Fitted values:
  y_fitted <- X %*%regCoeff
  
  # Residuals:
  residuals <- y-y_fitted
  
  # Degrees of freedom:
  degrees <- nrow(X)-ncol(X)
  
  # Residual variance:
  residual_variance <- (t(residuals)%*%residuals)/degrees
  
  # Variance of regression coefficients:
  coefficient_variance <- residual_variance[1]*solve(t(X)%*%X) 
  
  # t-values for each coefficient:
  t_values <- c()
  for (i in 1:ncol(coefficient_variance)) {
    t_values[i] <- regCoeff[i]/sqrt(coefficient_variance[i,i])
  }
  
  # p-values:
  p_values <- pt(t_values, df=degrees)
  
  ## Store all the computations
  lr_comps <- list(regCoeff, y_fitted, residuals, degrees, residual_variance, 
                  coefficient_variance, t_values, p_values)
  
  # Assign class to the object:
  class(lr_comps) <- "linreg"
  return(lr_comps)
}

## Create methods (Should be stored in separate files, this is just a template)

print.linreg <- function(lr_comps, ...){
  ##Just did this one quickly to check that it works fine (and to learn methods)
  Coefs <- unique(lr_comps[[1]])
  cat("Coefficients:\n(Intercept)\n", Coefs[1], 
      "\nSpeciesversicolor\n", Coefs[2],
      "\nSpeciesvirginica\n", Coefs[3])
}

plot.linreg <- function(){
  
}

resid.linreg <- function(){
  
}

pred.linreg <- function(){
  
}

coef.linreg <- function(){
  
}

summary.linreg <- function(){
  
}

## Remember to export methods: 
# S3method(method, linreg)

