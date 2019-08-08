
## This function will take an input matrix, define member and get/set function calls. 
## It will return the new object
makeCacheMatrix <- function(x = matrix()) {
        inverse<-NULL                                       ##initalize inverse
        set<-function(y){                                   ##define set
                x <<-y
                inverse<<-NULL
        }
        get <-function() x                                   ##define get
        set_inverse<-function(inverse) inverse<<-inverse     ##define function to set inverse 
        get_inverse<-function() inverse                      ##define function to get inverse
        list(set = set,                                      ##return new object
                get = get,
                set_inverse = set_inverse,
                get_inverse = get_inverse) 

}


## This function will check if a cached inverse is available, return that if available,
## or calculate the inverse if not available
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse<-x$get_inverse                             ##Initialize inverse
        if(!is.null(inverse)){                             ##if inverse is not null, return it
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()                                      ##otherwise, get X, and solve for inverse
        inverse<-solve(data)
        x$set_inverse(inverse)
        inverse
}
