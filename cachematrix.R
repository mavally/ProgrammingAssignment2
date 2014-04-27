## Put comments here that give an overall description of what your
## functions do

##This function allow us not to have to repeatedly calculate the inverse
## of a matrix if it has already been calculated

## Write a short comment describing this function
##The makeCacheMatrix function creates a special "matrix",which is really a list of functions
##With the set we can initialize the matrix.
##The get function returns the actual matrix.
##The setinverse sets the inverse of the matrix. It uses, the R function solve.
##The getinverse returns the matrix's inverse if it's calculated else it return NULL.
makeCacheMatrix <-function (x=matrix()){
	m<-NULL
	set <- function(y){
			x<<-y
			m<<-NULL
		}
	get<-function() x
	setinverse<-function(solve) m<<-solve
	getinverse<-function()m
	list(set=set,get=get,
		setinverse=setinverse,
		getinverse=getinverse)
}


## Write a short comment describing this function
##The cacheSolve function calculates the inverse of the special "matrix" create by the
##makeCacheMatrix function. It first query's the matrix's cache. If there a cache it just 
##returns it. Otherwise it computes the inverse of the data and set the inverse in the
##cache via the setinverse function  	
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()           #query the x matrix's cache         
 if(!is.null(m)) {           #if there is a cache
    message("getting cached data") 
    return(m)                #just return the cache, no computation needed
  }
  data <- x$get()             #if there's no cache
  m <- solve(data, ...)        #compute  here
  x$setinverse(m)           
  m                           #return the result

}
