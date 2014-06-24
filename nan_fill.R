# nan_fill(matrix_with_nans)
#
# replaces missing values (NaN) through kmeans clustering 
# and approximation by most likely cluster center 
# approximation is done by attribute (column-wise)
#
# parameters:
# matrix_with_nans: a data set passed as matrix, (possibly) containing NaN values
#   (minimum 2 data points (rows) must not contain NaN values)
#
# returns: 
# matrix where NaN values are replaced by meaningful values 
# 
# sets global variables:
# status: integer value indicating success of called function (0 = ok, >0 = warning, <0 = error)
# errormsg: contains a message string if an error or warning occurred

library(cclust)
library(e1071)

nan_fill <- function(matrix_with_na) {
  result <- tryCatch ( 
  {
    status <<- 1  # status is initially in Warning state
    
    matrix_na_omit <- na.omit(matrix_with_na) # reduced matrix
    # we have to retain min. 2 rows, otherwise cclust will fail
    if (nrow(matrix_na_omit) < 2) {
      status <<- -1
      errormsg <<- "Error IPN: matrix has to have at least 2 data points (rows) with non-NaN values!"
      return(NULL) # return(matrix_with_na)
    }    
    matrix_impute_basic <- impute(matrix_with_na)   # matrix filled with basic approximation
    
    # use 1/5 of data points as # of clusters (but min. 2)
    ncluster <- max(2,floor(nrow(matrix_na_omit) / 5))
    # compute a kmeans model on the reduced(omitted) matrix
    kmmodel <- cclust(matrix_na_omit,ncluster)
    # and apply to fill final matrix
    predicted <- predict(kmmodel,matrix_impute_basic)
    
    # create a matrix where values are are taken from cluster center values (via their indices in $cluster) 
    approximated <- predicted$centers[predicted$cluster,]
    
    # replace those the indices with NA vallues with the approximated values
    matrix_with_na_replaced <- matrix_with_na
    matrix_with_na_replaced[is.na(matrix_with_na)] <- approximated[is.na(matrix_with_na)]
    
    status <<- 0
    errormsg <<- ""
    return(matrix_with_na_replaced)
  },
  error = function(cond) {
    status <<- -1
    errormsg <<- paste("Error IPN: ", getwd(), cond)
    return(NULL)
  },
  warning = function(cond) {
    status <<- 1
    errormsg <<- paste("Warning IPN: ", cond)
    # try to continue, or
    # return("")
  },
  finally = {
    # Here goes everything that should be executed at the end, regardless of success or error.
  }
  )
  return(result)    
}