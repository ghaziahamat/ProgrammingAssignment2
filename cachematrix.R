## Cached matrix inversion functions
## Reads and writes matrix inverses 
#Example of how to use in a program or command line
# Z <- matrix(1:4,2,2)          #Define an invertibe square matrix Z
# Zcache <- makeCacheMatrix(Z)  #Create the cache with Z
# cacheSolve(Zcache)            #Use this instead of solve(Z) to use cache           

## Creates a vector of 4 functions to write a matrix X and its inverse M to a cache
## X is the matrix to be inverted. Output this function to a list
makeCacheMatrix <- function(X = matrix()) {
    # Initialise M as an empty matrix
    M <- NULL
    #Defines a function 'set' that writes the input Y as the matrix in the cache
    set <- function(Y){
      X <<- Y
      M <<- NULL
    }
    
    ## Defines a function 'get' that retrieves the matrix from the cache
    get <- function() X
    
    ## Defines a function 'setinv' that uses the standard 'solve' function to invert 
    ## the matrix, and writes the inverse as M in the cache
    setinv <- function(solve) M <<- solve
    
    ## Retrieves the current value of the inverse M from the cache
    getinv <- function() M
    
    ## writes the list of the four functions defined above, so that they are available in the parent environment
    list(set = set, get = get, setinv = setinv,getinv = getinv)

}


## Uses the list of functions defined in the makeCacheMatrix function to 
## extract the inverse from the cache if it exists, otherwise compute it
## Note 

cacheSolve <- function(cachelist, ...) {
    ## Retrieves the existing stored inverse as Xinv
  Xinv <- cachelist$getinv()
    ## If the stored value exists, print it from the cache
  if(!is.null(Xinv)){
    message("getting cached data")
    return(Xinv)
  }
    ## If it does not exist, retrieve the original matrix to invert
  data <- cachelist$get()
    ## Compute the inverse as Xinv
  Xinv <- solve(data,...)
    ## Write the computed Xinv to the cache
  cachelist$setinv(Xinv)
  Xinv
}

