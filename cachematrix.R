##The fucntions in cachematrix.R create a special matrix, which is really a list 
## containing functions to 1) generate a matrix, 2) get the matrix, 3) generate 
## the inverse of the matrix and 4) get the value of the matrix
################################################################################
################################################################################
################################################################################


################################################################################
## The makeCacheMatrix function creates a special "matrix" object that can cache 
## its inverse

############  EXAMPLE USAGE for makeCacheMatrix#################################
## Initialize a matrix mymatrix: 
## mymatrix <- makeCacheMatrix()
## Pass matrix values to mymatrix: 
## mymatrix$set(matrix(rnorm(4, 1,1), ncol=2, nrow=2))


makeCacheMatrix <- function(x = matrix()) {
  
  m<- NULL
  set<-function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  
  list(set=set, get=get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


################################################################################
################################################################################
################################################################################
################################################################################
## The cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above

############  EXAMPLE USAGE for cacheSolve #####################################
## Get the inverse of mymatrix: 
## cacheSolve(mymatrix)


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
