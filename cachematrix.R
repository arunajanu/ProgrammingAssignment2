## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function gets matrix as an input, set the value of matrix, get the value of matrix, set the inverse matrix and get the inverse matrix.Matrix object can cache its own object

makeCacheMatrix <- function(x = matrix()) {
	invMatrix <- NULL
#set the value of the matrix
     setMatrix <- function(y) {
     	x <<- y
     	invMatrix <<- NULL
     }
   
   getMatrix <- function() x
   setInverse <- function(inverse) invMatrix <<- inverse
   getInverse <- function() invMatrix
   list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## this function takes output of previous matrix as an input and checks inverse matrix has any value in it or not. In case inverse matrix from makeCacheMatrix is empty, it gets the original matrix data from and set the invertible matrix by solve function.
cacheSolve <- function(x, ...) {
#get value of invertible matrix from makeCacheMatrix function
         invMatrix <- x$getInverse()
        if(!is.null(invMatrix)) {
        	message("Getting cached Invertible Matrix")
        	return(invMatrix)
        }
  
 #if value of the invertible matrix is NULL then
       MatrixData <- x$getMatrix()
       invMatrix <- solve(MatrixData,...)
       x$setInverse(invMatrix)
       return(invMatrix)
        ## Return a matrix that is the inverse of 'x'
}

## Test 1
TestMatrix <- matrix(1:4,2,2)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

cacheSolve(CacheMatrix)
cacheSolve(CacheMatrix)

#Test3
TestMatrix <- matrix(1:9,3,3)
TestMatrix

CacheMatrix <- makeCacheMatrix(TestMatrix)
CacheMatrix$getMatrix()
CacheMatrix$getInverse()

