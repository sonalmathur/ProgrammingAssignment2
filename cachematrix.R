## The below functions are designed to initialise a matrix and find its inverse
## if the inverse is already calculated, function will not compute it again but
#  return the stored copy from cache

## This function will be used to set and get the matrix as well as its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  invM<-NULL
  set<-function(y){
    x <<- y
    invM <<- NULL
  }
  get<-function() x
  setInv<-function(inv)invM<<-inv
  getInv<-function() invM
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}


## This function will compute the inverse of matrix

cacheSolve <- function(x, ...) {
       
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInv(inv)
  inv
  
}
