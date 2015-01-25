makeCacheMatrix <- function(x = matrix()) { 
  # cachedInverse is similar to variable m in the MEAN example provided
  # the function SOLVE is similar to the function MEAN in the example provided
  cachedInverse <- NULL 
  set <- function(y) { 
    x <<- y 
    cachedInverse <<- NULL 
  } 
  get <- function() x 
  setInverse <- function(solve) cachedInverse <<- solve 
  getInverse <- function() cachedInverse 
  list(set = set, get = get, 
            setInverse = setInverse, 
            getInverse = getInverse) 
  } 




## Return the inverse of an cacheMatrix object 
## HERE CACHESOLVE ACTUALLY CHECKS IF THE MATRIX HAS BEEN CHANGED SINCE
## THE LAST COMPUTATION OF THE INVERSE.  SO CACHESOLVE REQUIRES TWO ARGUMENTS
## ARGUMENT 1 is the "SPECIAL MATRIX" AND ARGUMENT 2 is the CURRENT STORED MATRIX
## (WHOSE INVERSE IS BEING COMPUTED)
## TWO ARGUMENTS SHOULD BE PASSED THROUGH CACHE SOLVE AS IN THE FOLLOWING CALL
## Z <- cacheSolve(B, A)
## B is the "special" matrix constructed with makeCacheMatrix
## A is the current form of the matrix
## The "identical" function checks if A has been altered from x$get()

cacheSolve <- function(x,y = matrix(),...) { 
     ## Return a matrix that is the inverse of 'x'  
     cachedInverse <- x$getInverse() 
     if((!is.null(cachedInverse)) & (identical(x$get(), y))) { 
         message("getting cached data") 
         return(cachedInverse) 
       } 
     message("resetting makeCacheMatrix for NEW altered matrix")
     x$set(y)
     data <- x$get() 
     # print(data)
     cachedInverse <- solve(data,...)
     x$setInverse(cachedInverse) 
     cachedInverse
   } 
