## The function, makeCacheMatrix, creats a matrix object that can cache 
## the inverse of the input matrix.
##
## X is the input matrix; 
##
## I is the inverse of X, its default is NULL (empty)

makeCacheMatrix <- function(X = matrix()) {

          I <- NULL
  
          # This is a function for you to get the input matrix          
          get <- function() X
          
          
          # This is a function that returned the inverse matrix, I
          # It will ask cacheSolve to compute the inverse / get the cached matrix;
          # and reassign back to I
          
          inverse <- function() {
                    I <<- cacheSolve(X, I)
                    I
          }
          
          
          # This is a function used for setting a new input matrix, Y.
          # If X and Y are different, it will assign Y to X, and I to empty.
          #
          # Because matirx I is reset to NULL, so the function cacheSolve will 
          # solve the new inverse and return a "solved" value instead of 
          # getting from the cached data.
          
          set <- function(Y){
                    if (identical(X, Y) != TRUE){
                              X <<- Y
                              I <<- NULL
                    }
          }
                    
          
          list(set = set, get = get, inverse = inverse)
}

## The function, cacheSolve, will solve the inverse or 
## get the cached matrix and return the value

cacheSolve <- function(X, I){
          if(!is.null(I)){
                    message ("Getting cached data ...")
                    return(I)
          }
          
          else I <- solve(X)
}