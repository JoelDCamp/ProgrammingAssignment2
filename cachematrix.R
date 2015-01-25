## The folowing two functions, when used combination,
## allow a person to first generate a matrix and cache
## its value, and then to retrieve the cached matrix
## and generate its inverse.

## The first function is makeCacheMatrix. This function
## generates a matrix and caches its value.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setmatrix<-function(solve)m<<-solve
  getmatrix<-function()m
  list(set=set,get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}


## The second function, cacheSolve, generates the inverse
## of a matrix from makeCacheMatrix. If the value of the
## matrix was already cached by makeCacheMatrix, it retrieves
## the cached value and generates the inverse matrix from that.

cacheSolve <- function(x, ...) {
        ## The function checks for a cached matrix and returns its inverse.
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix,...)
  x$setmatrix(m)
}
