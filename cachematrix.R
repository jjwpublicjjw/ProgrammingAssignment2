#################################################################
## This file contains functions to support caching the 
## inverse of a matrix to avoid unnecessary (re)computation.
## 
## The makeCacheMatrix function creates a special "matrix" 
## object that can cache its inverse. 
##
## The cacheSolve function computes the inverse of the special
## "matrix" returned by makeCacheMatrix above. If the inverse
## has already been calculated and cached in the special
## "matrix" object, and the matrix has not changed, the 
## function returns the cached value.
#################################################################

##---------------------------------------------------------------
## Create and return a special "matrix", which is actually a list 
## containing functions to:
##  1. set the value of the matrix
##  2. get the value of the matrix
##  3. set the value of the matrix's inverse
##  4. get the value of the matrix's inverse
##---------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {

  # cache for matrix inverse 
  i <- NULL
  
  set <- function(y){
    x <<- y

    # reset cache when matrix value changed
    i <<- NULL
  }
  
  get <- function(){
    x
  }
  
  setinverse <- function(inverse){
    i <<- inverse    
  }
  
  getinverse <- function(){
    i
  }
  
  # return object that provides access
  # to the functions defined above
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

##---------------------------------------------------------------
## Return the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated 
## and cached in the special "matrix" object, and the matrix has 
## not changed, the function just returns the cached value.
## Otherwise, the function calculates the inverse, caches it in
## the special "matrix" object, and returns the newly calculated
## value.
##
## Note: This function *assumes* (does not check) that x is an
## invertible matrix. If x fails to meet this condition, the 
## call to solve() will fail.
##---------------------------------------------------------------
cacheSolve <- function(x, ...) {
  
  # if inverse is already cached, return it
  i <- x$getinverse()
  
  if(!is.null(i)){
    message("returning cached inverse")
    return(i)
  }
  
  # if inverse is not already cached, calculate 
  # it, cache it, and return it
  message("inverse is not cached - calculating it now")
  
  data <- x$get()
  
  i <- solve(data)
    
  x$setinverse(i)
  
  i
}


#################################################################
## Testing functions not explicitly required for Coursera 
## assignment. Please ignore the following code during the 
## peer evaluation.
#################################################################
test1 <- function(){
  rawMatrix <- rbind(
    c(1, 2, 3),
    c(4, 4, 4),
    c(3, 2, 3)
  )

  rawMatrixInverse = solve(rawMatrix)
  
  cacheMatrix <- makeCacheMatrix(rawMatrix)

  # expect cacheMatrix$get() to be identical to rawMatrix  
  if(!identical(rawMatrix, cacheMatrix$get())){
    message("UNEXPECTED: cacheMatrix$get() not identical to rawMatrix")
  }else{
    message("EXPECTED: cacheMatrix$get() identical to rawMatrix")
  }
  
  # expect cacheMatrix$getinverse() to be null
  if(!is.null(cacheMatrix$getinverse())){
    message("UNEXPECTED: cacheMatrix$getinverse() not null")
  }else{
    message("EXPECTED: cacheMatrix$getinverse() null")
  }
  
  # should generate "inverse is not cached" message
  inverse <- cacheSolve(cacheMatrix)
  
  # expect calculated inverse to be identical to rawMatrixInverse
  if(!identical(inverse, rawMatrixInverse)){
    message("UNEXPECTED: inverse not identical to rawMatrixInverse")
  }else{
    message("EXPECTED: inverse identical to rawMatrixInverse")
  }
  
  # check that cacheSolve caused inverse to be stored in "matrix" object
  if(!identical(cacheMatrix$getinverse(), rawMatrixInverse)){
    message("UNEXPECTED: cacheMatrix$getinverse() not identical to rawMatrixInverse")
  }else{
    message("EXPECTED: cacheMatrix$getinverse() identical to rawMatrixInverse")
  }
  
  # should generate "returning cached inverse" message
  inverse <- cacheSolve(cacheMatrix)
  
  # check that correct inverse remains stored in "matrix" object
  if(!identical(cacheMatrix$getinverse(), rawMatrixInverse)){
    message("UNEXPECTED: cacheMatrix$getinverse() not identical to rawMatrixInverse")
  }else{
    message("EXPECTED: cacheMatrix$getinverse() identical to rawMatrixInverse")
  }
  
  secondRawMatrix <- rbind(
    c(6, 2, 6),
    c(4, 4, 4),
    c(7, 2, 7)
  )
  
  cacheMatrix$set(secondRawMatrix)
  
  # expect cacheMatrix$get() to be identical to secondRawMatrix
  if(!identical(secondRawMatrix, cacheMatrix$get())){
    message("UNEXPECTED: cacheMatrix$get() not identical to secondRawMatrix")
  }else{
    message("EXPECTED: cacheMatrix$get() identical to secondRawMatrix")
  }
  
  # check that the formerly cached inverse has been reset to null
  if(!is.null(cacheMatrix$getinverse())){
    message("UNEXPECTED: cacheMatrix$getinverse() not null")
  }else{
    message("EXPECTED: cacheMatrix$getinverse() null")
  }
}
