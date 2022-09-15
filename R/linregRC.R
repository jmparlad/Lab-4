#' Calculate parameters of a multiple regression model.
#'
#' This function computes the linear regression parameters and assigns a class
#' to the returned object with the computation results.
#'
#' @param formula Formula or relation between the two model variables.
#' @param data Data to be used to compute the regression parameters.
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
  
  # Assign class, RC method:
  linreg <- setRefClass("linreg" ,
                          fields = list (lr_comps = list(regCoeff, y_fitted, 
                                                         residuals, degrees, 
                                                         residual_variance, 
                                                         coefficient_variance, 
                                                         t_values, p_values)),
                        # Defining the computation list straight away like this gives an (S4 OOP) error 
                          methods = list(
                            print = function(x){
                              Coefs <- unique(x[[1]])
                              cat("Coefficients:\n(Intercept)\n", Coefs[1],
                                  "\nSpeciesversicolor\n", Coefs[2],
                                  "\nSpeciesvirginica\n", Coefs[3])
                            },
                            plot = function(x){
                              # Complete
                            },
                            resid = function(x){
                              # Complete
                            },
                            pred = function(x){
                              # Complete
                            },
                            coef = function(x){
                              # Complete
                            },
                            summary = function(x){
                              # Complete
                            }))
  ## How to return correctly the class with its objects?
  
  # Store the computations:
  # c <- linreg$new(lr_comps = list(regCoeff, y_fitted, residuals, degrees, 
  #                                 residual_variance, coefficient_variance, 
  #                                 t_values, p_values))
  
  ## Running the previous commented section and returning c will return only 
  ## the object, whereas when returning the class linreg we cannot access the
  ## computations

  return(linreg)
  
  #### How to export RC methods in our package????
}
