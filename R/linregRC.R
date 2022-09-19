#' Assign a class representing our model
#'
#' This program assigns an RC class which contains the linear regression
#' parameters and some functions to do a simple linear regression analysis
#' @exportClass linreg
#' @export linreg
#' @import ggplot2
#'
#' @param formula Formula or relation between the two model variables.
#' @param data Data to be used to compute the regression parameters.
#' @examples
#' data(iris)
#' linreg_mod <- linreg$new(Petal.Length ~ Species, data = iris)


linreg <- setRefClass(
  "linreg" ,
  fields = list(
    regCoeff = "matrix",
    y_fitted = "numeric",
    residuals = "numeric",
    degrees = "numeric",
    residual_variance = "numeric",
    coefficient_variance = "matrix",
    t_values = "numeric",
    p_values = "numeric",
    formulaString = "character",
    dataName = "character"
  ),
  methods = list(
    initialize = function(formula, data) {
      dataName <<- deparse(substitute(data))
      formulaString <<- deparse(formula)
      # Independent variable matrix:
      X <- model.matrix(formula, data)
      # Dependent variable:
      y <- data[[all.vars(formula)[1]]]
      
      ## LEAST SQUARES COMPUTATIONS
      # Regression coefficients:
      regCoeff <<- solve(t(X) %*% X) %*% t(X) %*% y
      
      # Fitted values:
      y_fitted <<- as.vector(X %*% regCoeff)
      
      # Residuals:
      residuals <<- y - y_fitted
      
      # Degrees of freedom:
      degrees <<- nrow(X) - ncol(X)
      
      # Residual variance:
      residual_variance <<- as.vector((t(residuals) %*% residuals) / degrees)
      
      # Variance of regression coefficients:
      coefficient_variance <<- residual_variance[1] * solve(t(X) %*% X)
      
      # t-values for each coefficient:
      t_values <<- c(0)
      for (i in 1:ncol(coefficient_variance)) {
        t_values[i]  <<-  regCoeff[i] / sqrt(coefficient_variance[i, i])
      }
      
      # p-values:
      p_values <<- pt(t_values, df = degrees)
    },
    print = function() {
      writeLines(c(
        "Call:",
        paste(
          "linreg(formula = ",
          .self$formulaString,
          ", data = ",
          .self$dataName,
          ")",
          sep = ""
        ),
        "",
        "Coefficients:"
      ))
      base::print(.self$coef())
      },
    
    plot = function(){
      j <- 1 # sample counter
      res_acc <- c() # Accumulated residuals (by appendage) for each species
      median_res <- c() # median of the residuals
      l <- c()
      sq_standres <- c()
      median_sq_standres <- c()
      sd_res_total <- c()
      l <- append(l, unique(.self$y_fitted)) # Fitted values (1 per species)
      for (i in 1:length(l)){
        while (isTRUE(.self$y_fitted[j] == l[i])){
          res_acc <- append(res_acc, .self$residuals[j]) # Append residuals for each species
          
          # Compute square root of standardized residuals for the second plot:
          sq_standres <- append(sq_standres, sqrt(abs(.self$residuals[j]/sqrt(.self$residual_variance))))
          
          j <- j + 1 # Next sample
        }
        median_res <- append(median_res, median(res_acc)) # Median of residuals for the species
        res_acc <- c() # Reset accumulated residuals for the next species
        
        median_sq_standres <- append(median_sq_standres, median(sq_standres)) # Median of standardized residuals
        sd_res_total <- append(sd_res_total, sq_standres) # Append the set of data to have the entire set
        # to have all the elements stored after the loop
        sq_standres <- c() # Reset accumulated standardized residuals
        cat(sd_res_total, paste("\n\n"))
      }
                  
      # Create data frames to build correctly the plots
      # (one for the points and another one for the median line):
      ## Plot 1:
      datap1 <- data.frame(
        fit_val = c(.self$y_fitted),
        res = c(.self$residuals)
      )
      datap2 <- data.frame(
        fit_val = c(l),
        res = c(median_res)
      )
      p1 <- ggplot2::ggplot( data = datap1) +
        aes(x = fit_val, y = res) +
        geom_point() +
        geom_line(data= datap2, colour="#CC0000") +
        xlab(paste("Fitted values\n lm(", .self$formulaString, ")")) + ylab("Residuals") +
        ggtitle("Residuals vs Fitted")
      
      # Plot 2:
      datap3 <- data.frame(
        fit_val = c(.self$y_fitted),
        sd_res = c(sd_res_total)
      )
      datap4 <- data.frame(
        fit_val = c(l),
        sd_res = c(median_sq_standres)
      )
      p2 <- ggplot2::ggplot( data = datap3) +
        aes(x = fit_val, y = sd_res) +
        geom_point() +
        geom_line(data= datap4, colour="#CC0000") +
        xlab(paste("Fitted values\n lm(", .self$formulaString, ")")) + 
        ylab(expression(sqrt(abs("Standardized Residuals")))) +
        ggtitle("Scale - Location")
      
      rplots <- list(p1, p2)
      # TODO check the median of the standardized residuals computations, 
      return(rplots) 
    },
    resid = function() {
      return(.self$residuals)
    },
    pred = function() {
      return(.self$y_fitted)
    },
    coef = function() {
      coefs = as.vector(.self$regCoeff)
      names(coefs) = rownames(.self$regCoeff)
      return(coefs)
    },
    summary = function() {
      rstderror = paste("Residual standard error:", sqrt(.self$residual_variance),
                        "on", degrees,"degrees of freedom")
      p_values_temp = ifelse(.self$t_values > 0,1-.self$p_values, .self$p_values)
      
      stdErrors = c()
      for (i in 1:ncol(coefficient_variance)) {
        stdErrors = c(stdErrors,sqrt(coefficient_variance[i, i]))
        
      }
      writeLines(c(
          "Call:",
          paste(
            "linreg(formula = ",
            .self$formulaString,
            ", data = ",
            .self$dataName,
            ")"
          ),
          "", "Residuals:"))
      base::print(base::summary(.self$residuals)[-4])
      cat("\n\tEstimate \tStd. Error \tt value \tp value \n")
      coefs <- .self$coef()
      rn <- rownames(.self$regCoeff)
      sstars <- c()
      for (i in 1:ncol(coefficient_variance)) {
        if (p_values_temp[i] < 0.001){
          sstars <- append(sstars, "***")
        }
        else if ((p_values_temp[i] >= 0.001) && (p_values_temp[i] > 0.01)){
          sstars <- append(sstars, "**")
        }
        else if ((p_values_temp[i] >= 0.01) && (p_values_temp[i] > 0.05)){
          sstars <- append(sstars, "*")
        }
        else if ((p_values_temp[i] >= 0.05) && (p_values_temp[i] > 0.1)){
          sstars <- append(sstars, ".")
        }
        else {
          sstars <- append(sstars, " ")
        }
        cat(rn[i], paste(coefs[i],
                         stdErrors[i], 
                         .self$t_values[i],
                         p_values_temp[i], sstars[i], "\n"))
        
      }
      cat(paste("\n",rstderror, "\n", sep = ""))
    }
  )
)
