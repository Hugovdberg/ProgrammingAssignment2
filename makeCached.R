## makeCached.R
## Author: Hugo van den Berg
##
## makeCached is similar to the cachematrix function in that it caches the
## output of a (possibly computationally heavy) function. However, this version
## allows to change the function and applies the set function as soon as the
## input is changed.

makeCached <- function(fun, x = numeric(), ...) {
    v <- fun(x, ...)
    setInput <- function(y, ...) {
        x <<- y
        v <<- fun(x, ...)
    }
    getInput <- function() x
    setFunction <- function(f, ...) {
        fun <<- f
        v <<- fun(x, ...)
    }
    getFunction <- function() fun
    getReturn <- function() v
    list(setInput = setInput, getInput = getInput,
         setFunction = setFunction, getFunction = getFunction,
         getReturn = getReturn)
}
