## The following functions do a job of caching the inverse of a matrix so that inverse is not required to be 
## calculated everytime the matrix is called for.

## makeCacheMatrix takes as input a matrix and creates special matrix which is a list of three functions to
## get the values of the matrix 
## set the inverse of the matrix 
## get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
	
	m <- NULL   
 
        get <- function() x    #Function to retrieve the original finction

        setinverse <- function(inverse) m <<- inverse    #Function to cache the inverse

        getinverse <- function() m    #Function to get the inverse

        list(get = get, setinverse = setinverse, getinverse = getinverse)

}


## cacheSolve calculates the inverse of the special matrix created with makeCacheMatrix. If the inverse is already there
## it retrieves the inverse, else it calculates the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

	m <- x$getinverse()   #Gets the inverse matrix cached in the special matrix

	  #Checks whether the inverse matrix is NULL matrix or not
        if(!is.null(m)) {
                message("getting cached data")
                return(m)     #If the inverse matrix is not NULL, then the value is retrieved from cache
        }

        data <- x$get()    #Value of the original matrix is retrieved for the inverse to be calculated

        m <- solve(data)
        x$setinverse(m)    #New inverse matrix is set to the speical matrix

        m

}
