################################ Assignment Week 3 ############################

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that generates a special matrix object that can
## cashe its inverse.


makeCacheMatrix <- function(x = matrix()) {
        i = NULL # Set the inverse as NULL 
        set <- function(y){ # The set function is used to assign values to the
                x <<- y # matrix 
                i <<- NULL
        }
        get <- function()x # The get function is used to return matrix x
        
setinver <- function(inverse)i <<- inverse # Assign values to the inverse

getinver <- function(){ # Get values of the inverse 
                inv <- ginv(x)
                inv%*%x
        }

        list(set = set, # list the functions by the correct order
             get = get,
             setinver = setinver,
             getinver = getinver)
}


## Write a short comment describing this function

## The cacheSolve function generates the inverse of the "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
                i <- x$getinv()
                if(!is.null(i)){  ## See if the result of the inverse is null 
                        message("getting cached data")
                        return(i) 
                }
                data <- x$get()
                i <- solve(data, ...) ## Generate the inverse matrix 
                x$setinverse(i)
                i
        }
  

        