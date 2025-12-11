## New Class

## Class for numeric vectors that only contain 1s and 0s (DO NOT CHANGE)
setClass(
    Class = "binary_vector",
    contains = "numeric"
)

## Validity method for 'binary_vector' -- there should only be 0s and 1s
setValidity(
    Class = "binary_vector",
    method = function(object) {
        if (!all(object %in% c(0, 1))) {
            return("All elements must be 0 or 1")
        }
        return(TRUE)
    }
)

## Coerce method from character --> binary_vector
setAs(
    from = "character",
    to = "binary_vector",
    def = function(from) {
        numeric_values <- as.numeric(from)
        new("binary_vector", numeric_values)
    }
)

## Adding two binary_vectors using binary arithmetic
## 1 + 1 = 0
## 1 + 0 = 1
## 0 + 1 = 1
## 0 + 0 = 0

## Generic function for adding two vectors
setGeneric(
    "add_vector",
    function(x, y) {
        standardGeneric("add_vector")
    }
)

## Specific method function for adding two binary vectors
setMethod(
    "add_vector",
    signature(x = "binary_vector", y = "binary_vector"),
    function(x, y) {
        result <- (x + y) %% 2
        new("binary_vector", result)
    }
)


################################################################################
## Some test cases

## Create a new vector
x <- new("binary_vector", c(0, 1, 1, 0, 0))

## This should fail with an error
x <- new("binary_vector", c(0, 1, 1, 0, 2))

## This should fail with an error
x <- new("binary_vector", c(0, 1, -1, 0, 0))

## Coerce from a character vector
x <- as(c("0", "1", "1", "0", "0", "1"), "binary_vector")
y <- new("binary_vector", c(1, 1, 0, 0, 1, 0))

## Add two binary vectors
z <- add_vector(x, y)
z  ## should be c(1, 0, 1, 0, 1, 1)
