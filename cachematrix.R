##Together, these functions take in a matrix, cache it, solve it, cache the
##inverse, return the cached inverse and repeat when given a new matrix. 

##makeCacheMatrix function takes in and caches a matrix. Calls solve function
##to find inverse of matrix. Caches inverse. Returns list of functions that 
##cache and retrieve values.
makeCacheMatrix<-function(x=matrix()) {
    inv<-NULL               ##initializes variable to cache inverse
    set<-function(y) {      ##set function caches the matrix, sets 
                            ##cached inverse to NULL.
        x<<- y
        inv<<-NULL
    }
    get<-function()x        ##initialize func to retrieve cached matrix.
    invset<-function(solve)inv<<- solve ##initialize func to solve matrix, cache inverse to inv.
    invget<-function()inv   ##initialize func to get cached inverse. 
    list(set = set, get = get,  ##returns functions that cache/retrieve values.
         invset = invset,
         invget = invget)
}
##cacheSolve function taxes in list of functions from makeCacheMatrix and 
##uses those functions/variables to retrieve cached matrix and inverse if 
##previously solved, otherwise solves cached matrix and caches inverse. 
##Returns inverse of matrix supplied to/cached by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    m <- x$invget()     ##retrieves cached inverse for x
    if(!is.null(m)) { ##checks if inverse cached by invset is not NULL
        message("getting cached data")
        return(m)     ##returns cached inverse if it has a value
    }
    mymatrix <- x$get() ##if no value, calls get to retrieve cached matrix
    m <- solve(mymatrix)##solves cached matrix
    x$invset(m)         ##calls invset to cache new inverse value 
    m                   ##returns inverse
    
}

