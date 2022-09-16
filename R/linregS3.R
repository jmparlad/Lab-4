#' Calculate parameters of a multiple regression model.
#'
#' This function computes the linear regression parameters and assigns a class
#' to the returned object with the computation results.
#'
#' @param formula Formula or relation between the two model variables.
#' @param data Data to be used to compute the regression parameters.
#' @export
#' @return The list \code{lr_comps} containing the computation results of
#' the multiple regression model parameters.
#' @examples
#' data(iris)
#' lr_comps <- linreg(Petal.Length ~ Species, data = iris)


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

plot.linreg <- function(lr_comps, ...){
  j <- 1 # sample counter
  res_acc <- c() # Accumulated residuals (by appendage) for each species
  median_res <- c() # median of the residuals
  l <- c()
  l <- append(l, unique(lr_comps[[2]])) # Fitted values (1 per species)
  for (i in 1:length(l)){
    while (isTRUE(lr_comps[[2]][j] == l[i])){
      res_acc <- append(res_acc, lr_comps[[3]][j]) # Append residuals for each species
      j <- j + 1 # Next sample
      
    }
    median_res <- append(median_res, median(res_acc)) # Median of residuals for the species
    res_acc <- c() # Reset accumulateed residuals for the next species
  }
  # Create data frames to build correctly the plots 
  # (one for the points and another one for the median line):
  datap1 <- data.frame(
    fit_val = c(lr_comps[[2]]), 
    res = c(lr_comps[[3]])
  )
  datap2 <- data.frame(
    fit_val = c(l), 
    res = c(median_res)
  )
  
  # Should import ggplot2 (@import ggplot2 in the function description)
  p1 <- ggplot2::ggplot( data = datap1) +
    aes(x = fit_val, y = res) +
    geom_point() +
    geom_line(data= datap2, colour='red') +
    xlab("Fitted values\n lm(Petal.Length ~ Species)") + ylab("Residuals") +
    ggtitle("Residuals vs Fitted")
  
  ## Complete second plot
}

resid.linreg <- function(lr_comps, ...){
  
}

pred.linreg <- function(lr_comps, ...){
  
}

coef.linreg <- function(lr_comps, ...){
  
}

summary.linreg <- function(lr_comps, ...){
  
}

## Remember to export methods: 
# S3method(method, linreg)