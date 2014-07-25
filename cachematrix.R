## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix() makes a special vector that contains the function to 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the inverse matrix
# 4.  get the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        #define a set method; when called will reset the inverse to null
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        #get method which will return the input matrix
        get <- function() x
        #setinverse method for setting the mean manually
        setinverse <- function(inverse) i <<- inverse
        #getinverse method that will return the inverse
        getinverse <- function() i
        #return a list of functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cachesolve() checks if the inverse matrix has already been calculated 
# and value has been set in the environment. If so then return the cached value 
#otherwise calculate new inverse matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        #check if the inverse has already been calculated 
        #if so then return the cached value
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        #otherwise calculate the inverse matrix
        else{
                data <- x$get()
                #make sure there is no 'NA' value in the matrix
                if (sum(is.na(data)) ==0)
                {
                        i <- solve(data, ...)       
                }
                else{
                        
                        message("NA value contained in matrix hence iverse can not be found")
                }
        }
        #set the inverse equal to the newly calculated value
        x$setinverse(i)
        #return the inverse matrix
        i
}
