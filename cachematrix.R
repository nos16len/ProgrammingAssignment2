## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a function that gives a list of 4 functions, get(), getSol(), set(), setSol() 
## Defined in makeCacheMatrix is a variable Sol, which stores the inverse of the input matrix when cacheSolve is called

## cacheSolve is a function that will either solve for the inverse of matrix stored in makeCacheMatrix,
## or look for the cached inverse, Sol, stored in makeCacheMatrix if the matrix inverse has been calculated before

## To use it properly we need to assign makeCacheMatrix to another variable first
## e.g. test <- makeCacheMatrix
## cacheSolve(test)



## Write a short comment describing this function

## There are 4 functions in makeCacheMatrix with below uses:
## set(matrix) : input the matrix being solved for inverse, and reset the variable Sol
## setSol(inverse) : used by cacheSolve() to pass the inverse matrix solved to Sol
## get() : displays the matrix being solved for its inverse
## getSol(): returns Sol, which is either NULL (before cacheSolve is run) or return the inverse of matrix displayed by get()
## Within the makeCacheMatrix, Sol is where the inverse solution is stored

makeCacheMatrix <- function(x = matrix()) {

  Sol <- NULL
  set <- function(y) {
    x <<- y
    Sol <<- NULL
  }
  get <- function() x
  setSol <- function(inverse) Sol <<- inverse
  getSol <- function() Sol
  list(get = get, getSol = getSol, set = set, setSol = setSol)
  
}


## Write a short comment describing this function

## this function will search in makeCacheMatrix to see if Sol is Null or not
## if Sol is null, meaning the inverse has not been solved before, cacheSolve will solve for the inverse
## and pass it back to the Sol in MakeCacheMatrix

## If Sol is already solved, it will return Sol instead of solving for the inverse again

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  ##If x$getSol() is not NULL, meaning the inverse has been computed
  ##then return cache values
  
    inverse <- x$getSol()
  
    if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
    }
  
  ##otherwise compute the inverse with solve()
  
    mat <- x$get()
    inverse <- solve(mat)
    x$setSol(inverse)
    inverse
  
}
