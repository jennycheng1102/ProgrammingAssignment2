# makecCacheMatrix function creats a special 'matrix' ,which contains a function to creates a list 
#1.set the matrix 2.get the matrix 3.set the inverse of the matrix 4.get the inverse of the matrix
makeCacheMatrix<-function(x=matrix()){
    i<-NULL
    set<-function(y){
        x<<-y
        i<<-NULL
    }
    get<-function() x
    setinverse<-function(solve) i<<-solve
    getinverse<-function() i
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
#cachesolve matrix calculates the inverse of the special 'matrix' using the above function.It first checks if the 
#inverse has been calculated.If so, it directly get the mean from the cache and skip the computation.If not,it calculates
#the inverse using getinverse function.
cachesolve<-function(x,...) {
    i<-x$getinverse()
    if(!is.null(i)){
        message('getting cached data')
        return(i)
        
    }
    data<-x$get()
    i<-solve(data,...)
    x$setinverse(i)
    i
}
