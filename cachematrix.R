## Put comments here that give an overall description of what your
## functions do

## The first function i.e. the makeCacheMatrix(), inputs a matrix as an argument
## in the function. The function then excecutes the get_matrix_new() function
## in order to display the matrix given as input.
## The first function below is used to do the following tasks:
## 1. Set the matrix, 2. Get the matrix, 3. Set the inverse, 4. Get the inverse

makeCacheMatrix <- function(mat) {
        mat_new <- mat            ##Assign the matrix to mat_new
        mat_inv <- NULL           ##Initialize the inverse of matrix to NULL
        mat_old <- NULL           ##Initialize the old matrix to NULL
        
##mat_inv will store the inverse that is calculated by the second function and
##mat_old will store the matrix, whose inverse has been calculated.
        
        set_matrix <- function(newmat){
                mat_new <<- newmat     ##Sets a new matrix, whose inverse is not yet calculated.
        }
        set_inverse <- function(m,inv){
                mat_old <<- m          ##Stores the matrix whose inverse is calculated.
                mat_inv <<- inv        ##Stores the inverse of the matrix
        }
        get_matrix_new <- function()
                mat_new                ##Returns the matrix whose inverse needs to be calculated.
        get_matrix_old <- function()
                mat_old                ##Returns the old matrix, whose inverse has been calculated.
        get_inverse <- function()
                mat_inv                ##Returns the inverse of the matrix
        list(set_matrix=set_matrix,set_inverse=set_inverse,
             get_matrix_new=get_matrix_new,get_matrix_old=get_matrix_old,
             get_inverse=get_inverse)
}


## Write a short comment describing this function

## The second function i.e. the cacheSolve() function is used to calculate the
## inverse of the matrix and store it in the variable 'inv' which is returned
## by the first function. It checks whether the inverse has been previously
## calculated or no. If calculated, the caced inverse data is returned and if
## no, then inverse of the matrix is calculated and the value is returned.

cacheSolve <- function(x){
        m1<-x$get_matrix_old() ##Get the matrix whose inverse is stored
        m2<-x$get_matrix_new() ##Get the matrix whose inverse need to be calculated
        inv<-x$get_inverse()   ##Get the inverse
        k=0
        if(!is.null(m1)){      ##First time there is no old matrix(no inverse calculated)
                
                for(i in seq_len(nrow(m1))){  ##Checking for new and old matrix are they equal
                        for(j in seq_len(ncol(m1))){
                                if (!(m1[i,j]==m2[i,j]))
                                        k=1
                        }
                }
        }
        ##If both the matrix are equal, then returned the cached data
        if ((k==0)&& (!is.null(inv))){
                message("Getting Cached Data")
                return(inv)
        }
        
        ##If both matrix are not the same, then calculate the inverse for the matrix.
        message("Calculating Inverse")
        inv=solve(m2)
        x$set_inv(m2,inv)
        inv
}