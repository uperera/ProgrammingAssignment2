#This script consists of two main functions. The first, makeCacheMatirx is a special matrix object 
#that returns a list of functions which can be used by the second function cacheSolve.
#cacheSolve function returns the inverse of the input argument matrix and 
#saves it in a variable which can be accessed by both functions due to the lexical 
#scoping feature of R. i.e. functions within the function is able to access variables 
#declared in the environment in which they are defined.



## makeCacheMatrix
#This function returns a list of 5 functions as follows
#1.set() Takes a matrix argument and saves it in a memory variable x.
#it also sets the variable m to NULL. 
#
#2.inset() This fuction is similar to set(), only difference is it does not set m to NULL.
#inset is used when we need to save a new matrix without deleting the current
#value in m.
#
#3.get() This returns the matrix currently saved by set.
#
#4.setinverse() This function computes and saves in m, the inverse  matrix of its input argument
#
#5.getinverse() returns the matrix saved in gloabl variable m.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  o <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
     }
  inset <- function(y) {
      x <<- y
     }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse,
       inset= inset)

}

#cacheSolve
#This function takes the function makeCacheMatrix and an invertible matrix as input arguments.
#It returns the inverse matrix of the input argument and saves it in a global variable m.
#At the same time, it calls inset() function defined above to save the input arugment matrix in
#x of makeCacheMatrix. By Doing this, we are able to examine if the 
#input argument matrix is a new one the next time cacheSolve is called by comparing with x.
#If it is the same and the value of m is not NULL, then the cached value is returned as the
#inverse matrix.
#If it's a new matrix, then the new inverse matrix is calculated and returned while saved in m.

cacheSolve <- function(x, ...) {
          
  m <- x$getinverse()
  o <- x$get()
      if(identical(..., o) & (!is.null(m))) {
           message("getting cached data")
           return(m)
      }
  data <- x$get()
  m <- solve(...)
  x$setinverse(m)
  x$inset(...)
  m
}
