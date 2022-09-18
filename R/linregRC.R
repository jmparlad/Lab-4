#' This is a class representing our model
#'
#' This assigns a RC class
#' to the returned object with the computation results.
#' @exportClass linreg
#' @export linreg
#' @import ggplot2

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
          "linreg(formula =",
          .self$formulaString,
          ", data =",
          .self$dataName,
          ")"
        ),
        "",
        "Coefficients:"
      ))
      base::print(.self$coef())
      },
    plot = function() {
      j <- 1 # sample counter
      res_acc <- c() # Accumulated residuals (by appendage) for each species
      median_res <- c() # median of the residuals
      l <- c()
      l <- append(l, unique(.self$y_fitted)) # Fitted values (1 per species)
      for (i in 1:length(l)){
        while (isTRUE(.self$y_fitted[j] == l[i])){
          res_acc <- append(res_acc, .self$residuals[j]) # Append residuals for each species
          j <- j + 1 # Next sample
        }
        median_res <- append(median_res, median(res_acc)) # Median of residuals for the species
        res_acc <- c() # Reset accumulated residuals for the next species
      }
                  
      # Create data frames to build correctly the plots
      # (one for the points and another one for the median line):
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
        xlab("Fitted values\n lm(Petal.Length ~ Species)") + ylab("Residuals") +
        ggtitle("Residuals vs Fitted")
      
      # TODO Complete second plot
      return(p1)
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
      rstderror = paste("Residual Standard Error: ", sqrt(.self$residual_variance),
                        " on ", degrees," degrees of freedom")
      p_values_temp = 1 - .self$p_values
      coefMatrix = matrix(c(.self$coef(),0,0,0,0,0,.self$t_values,p_values_temp),nrow = length(.self$coef()),ncol = 4)
      colnames(coefMatrix) = c("Estimate", "Std. Error", "t value", "p value")
      rownames(coefMatrix) = rownames(.self$regCoeff)
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
      cat("\nCoefficients:\n")
      base::print(coefMatrix)
      cat(paste("\n",rstderror))
      
      #TODO replace 0 in coefMatrix with std error!
    }
  )
)
