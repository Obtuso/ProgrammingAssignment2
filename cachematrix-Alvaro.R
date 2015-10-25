## This script contains tw functions, namely makeCacheMatrix and cacheSolve. 
## The first function makeCacheMatrix creates a matrix in cache memory, and the second function 
## calculates and stores the inverse of the matrix in cache memory. 
## The inverse of a matrix is used very often to resolve systems of linear equations. 
## the calculation of the inverse matrix of a large matrix can be computer intensive, and for 
## this reason it is often advantegeous to store the inverse in cache memory.

## The function makeCacheMatrix performs the following tasks 

## set the value of a matrix
## get the value of the martix
## set the value of the inverse of the matrix
## get the value of the inverse matrix
## and these tasks are catalogued in a 
## list at the end of the function to be retrieved by the next function 
makeCacheMatrix <- function(x = matrix()) 
  {
  Inv_x <- NULL
## Inv_x is a variable to store the inverse of the matrix. In this step we make sure that the 
## variable is initilized with NULL. Just in case the variable Inv_x has been used by another
## program, so we make sure we initiate it with our own value. NULL
  set <- function(y) 
    {
    x <<- y
    Inv_x <<- NULL
    }
## function set does write the values of the matrix x and its inverse Inv_x into the global 
## environment. 
  get <- function() x
## obtains the matrix
  setinverse <- function(inverse) Inv_x <<- inverse
## setinverse writes the inverse of the matrix into the global environement "<<-"
## as opposed to assigning the value within the scope of the function only.
  getinverse <- function() Inv_x
## recovers the value of the inverse matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
## catalogues all the subfunctions into a list with names for the subfuncions)
    }

## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) 
  {
## Return a matrix that is the inverse of 'x'
  
  Inv_x <- x$getinverse()
  if(!is.null(Inv_x)) 
    {
    message("getting cached data")
    return(Inv_x)
    }
  data <- x$get()
## the next line does calculate the inverse using the R function "solve"
## I am using the assumtion that the matrix can always be inverted
## otherwise I could introduce a conditional here and calculate the determinant of the matrix
## if the determinant of the matrix is zero the matrix cannot be inverted
  Inv_x <- solve(data, ...)
  x$setinverse(Inv_x)
  Inv_x ##returns the inverse
    }
