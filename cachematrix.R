## makeCacheMatrix will create a special Matrix with a list
##that stores the Inverse Matrix of this one.
##cacheSolve will check if the inverse matrix have been already
##computed and takes this value instead of doing again the operation

##Creates a special Matrix that can save its inverse value
##if the value had been already computed.

makeCacheMatrix <- function(x = matrix()) {

        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function(){
                x
        }
                
        setInverse <- function(Inverse) {
                I <<- Inverse
        }
        getInverse <- function(){
                I
        } 
                
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve checks whereas the inverse value have been already
## computed

cacheSolve <- function(x, ...) {
        
        I <- x$getInverse()
        
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        
        matrix <- x$get()
        I <- solve(matrix)
        x$setInverse(I)
        I
}
