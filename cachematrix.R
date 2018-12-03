## makeCacheMatrix returns a special object with cachable methods
## cacheSolve takes the special type and returns the inverse, from cache if available

##usage examples:
## t<-matrix(sample(16),4,4)
## makeCacheMatrix(t)->u
## cacheSolve(u)
## cacheSolve(u) ##Second run gets cache
## u$set(matrix(sample(9),3,3)) ## new values
## cacheSolve(u) ##First one shows new values
## cacheSolve(u) ##Second run gets cache



## Pass in a matrix, return a new special matrix
## Replace the values to reuse with the set method

makeCacheMatrix <- function(x = matrix()) {
  minv<-NULL
  set<-function(y){
    x<<-y
    minv<<-NULL
  }
  get<-function()x
  setinv<-function(mtx) minv<<-mtx
  getinv<-function() minv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## Return the inverse, from cache if possible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  testinv<-x$getinv()
  if (is.null(testinv)){
    mtx<-x$get()
    mtxinv<-solve(mtx)
    x$setinv(mtxinv)
    mtxinv
  }
  else
  {
    message("getting cache")
    testinv
    }
}
