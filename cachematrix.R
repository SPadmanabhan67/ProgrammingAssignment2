
## makeCacheMatrix takes a conventional matrix and returns a "special matrix" that remembers its inverse once computed
## The object returned is a list that carriers get, set, getInverse and setInverse methods

makeCacheMatrix <- function(x = matrix()) {

      myInverse <- NULL
      get <- function () x
      set <- function (m){
        x <<- m
        myInverse <<- NULL
      }
      
      
      ## setInverse(InverseMatrix)
      setInverse <- function (InvMatrix){
          myInverse <<- InvMatrix
      }
  
      ## getInverse()
      getInverse <- function(){
          return (myInverse) 
      }
      print ("Creating a special matrix")
      ##special matrix object that knows about its inverse if cached
      list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve takes the "special matrix" created via makeCacheMatrix and returns the inverse of the "real matrix" that was passed into makeCacheMatrix
## Input is a "special matrix" while what is returned is a conventional matrix that is inverse of original passed in
## It checks the cache for existence and computes & caches it if not there.
## The methods have print statements that can tell when it is returning from cache or setting if fresh for tracing.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## check if inverse is already there for x
        
        myInverse = x$getInverse()
        
        if (!is.null(myInverse)){
            print ("getting cached inverse")
            return (myInverse)
        }
        ## else i need to compute inverse now and cache it
        myMat = x$get()
        myInverse = solve(myMat, ...)
        print ("Caching it now")
        x$setInverse(myInverse)
        myInverse
}
