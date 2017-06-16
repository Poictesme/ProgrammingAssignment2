# cachematrix.R
# Coursera R Programming - Programming Assignment 2: Lexical scoping
# contains two functions:
#
# makeCacheMatrix
# cacheSolve 

#makeCacheMatrix
# Given a matrix, defines four functions to operate on that matrix
# set uses the <<-- assignment operator to cache the input matrix
# get returns the cached value of the matrix
# setinvert uses the <<-- assignment to cache the invert of the input matrix
# getinvert returns the cached invert matrix if already setinvert, otherwise returns null
makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  get<-function()x
  setinvert<-function(solve) m <<-solve
  getinvert<-function()m
  list(set=set,get=get,setinvert=setinvert,getinvert=getinvert)
  

}


# cacheSolve
# Given a matrix, checks to see if the invert has already been setinvert with makeCacheMatrix.
# If so, the cached invert is returned.  
# Otherwise, the invert of the cached matrix is found and cached using the setinvert function of makeCacheMatrix, then returned
cacheSolve <- function(x, ...) {

  m<-x$getinvert()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data)
  x$setinvert(m)
  m

}
