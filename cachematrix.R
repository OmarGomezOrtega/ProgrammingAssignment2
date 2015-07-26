## The functions implemented below create an inverse matrix of another matrix 
## and storage it in a global variable in order to not recalculate it again 
## if the original matrix doesn't change.


makeCacheMatrix <- function(x = matrix()) {
    ## makeCacheMatrix takes a matrix as argument and return a list of functions 
    ## which store the value of the matrix and its inverse and can get them 
    ## back. 
    
    ## Initiallizing of inverse container "inv" as NULL.
    inv <- NULL
    
    ## Defining function set.
    set <- function(y) {
        ## It takes the value of the argument "y" and storages it in "x" and 
        ## gives to variable "inv" the value of NULL and return them.
        
        x <<- y 
        inv <<- NULL
    }
    
    ## Defining function get.
    get <- function() {
        ## Return the value of the matrix "x".
        
        x
    }
    
    ## Defining function setinv.
    setinv <- function(invers){
        ## It gives the value of the argunment "invers" to the variable "inv" 
        ## and return it.
        
        inv <<- invers
    }
    
    ## Defining function getinv.
    getinv <- function(){
        ## Return the value of the matrix "inv".
        
        inv
    }   
    
    ## Create the list of functions which stores the value of tha matrix an its
    ## inverse. 
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x) {
    ## cacheSolve take the list of functions returned by makeCacheMatrix as 
    ## argument and return the inverse of the matrix storaged in that list. 
    
    ## Getting the value of the inverse matrix from the argument.
    inv <- x$getinv()
    
    ## Checking if the inverse matrix was storaged before. If it has been 
    ## calculated before, it will take the value of the inverse matrix from the 
    ## cache, returns it and exit from the function. 
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    
    ## Extract the value of the matrix from the cache.
    data <- x$get()
    
    ## Calculate the inverse of the matrix stored in the cache.
    inv <- solve(data)
    
    ## Set the value calculated in the previous line as the inverse on cache.
    x$setinv(inv)
    
    ## Return the invers value calculated before.
    inv
}
