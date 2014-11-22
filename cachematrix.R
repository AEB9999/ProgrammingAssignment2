##
## simply put create a function that returns the inverse of a matrix
## get the matrix from a cached variable 
## if the cached matrix is for the same requested matrix then return such
## otherwise solve the inverse of the matrix and return that matrix 
##
makeCacheMatrix <- function(x = matrix()) {     ## create main Functions to be called and set x as matrix
       m <- NULL                                ## initiate local m to NULL
       set <- function(y) {                     ## create set FUN with arg y
           x <<- y                              ## store passed var y to global var x
           m <<- NULL                           ## initiate global m to NULL
       }
       get <- function() x                      ## create get FUN returning global x from set Fun parm y
       setsolve <- function(solve) m <<- solve  ## create FUN with arg solve to cache solve value to global m
       getsolve <- function() m                 ## create FUN to return global m for matrix x
       list(set = set, get = get,               ## coerce FUN object list names $set $get $setsolve $getsolve
                  setsolve = setsolve,
                  getsolve = getsolve)
  }
##
## Function get MATRIX inverse from a cached variable or compute such with solve
##
cacheSolve <- function(x, ...) {              ## create Fun cacheSolve with passed args
    m <- x$getsolve()                         ## setlocal m with 'FUN getsolve global m' for matrix x  
       if(!is.null(m)) {                      ## verify global m is populated with !NULL data
           message("getting cached data")     ## if so, msg notify using cached result
           return(m)                          ## and exit Returning a matrix that is the inverse of 'x'
       }
       data <- x$get()                        ## set data to list x 
       m <- solve(data, ...)                  ## comp solve for data
       x$setsolve(m)                          ## cache local m (solve result) to global m for matrix x
       m                                      ## Return a matrix that is the inverse of 'x=matrix()'
}       
