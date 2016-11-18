### Create a list of matrices from the 'sets' variable
SplitTable <- function(data, sets) {
    tables <- list()
    for (i in 1:length(sets)) {
        tables[[i]] <- data[, sets[[i]]]
    }

    if (!is.null(names(sets)))
        names(tables) <- names(sets)
    tables
}

### MFA on normalised tables
NormalizeAndSVD <- function(tables) {
    svs <- numeric(0)
    
    for (i in 1:length(tables)) {
        ## Normalise each table by the singular values
        sv <- svd(tables[[i]])$d[1]
        tables[[i]] <- tables[[i]] / sv
        svs <- c(svs, sv)               # Accumulate a list of singluar values
    }

    ## PCA on the whole table
    X <- do.call(cbind, tables)
    X.svd <- svd(X)

    list(svd = X.svd,
         svs = svs)
}

#' Multiple factor analysis on a data frame or matrix.
#'
#' @param data the data set, a data frame or matrix
#' @param sets a list of numeric or character vectors indicating the sets of
#'     variables
#' @param ncomps integer indicating how many number of components
#'     (i.e. factors) are to be extracted
#' @param center either a logical value or a numeric vector of length
#'     equal to the number of active variables in the analysis
#' @param scale either a logical value or a numeric vector of length
#'     equal to the number of active variables in the analysis
#' @return an object of class "mfa", containing eigenvalues, factor
#'     scores, partial factor scores, and factor loadings.
#' 
#' @export
mfa <- function(data, sets, ncomp = NULL, center = TRUE, scale = TRUE) {
    data <- scale(data, center, scale) / sqrt(nrow(data) - 1)
    tables <- SplitTable(data, sets)
    raw <- NormalizeAndSVD(tables)

    if (is.null(ncomp)) ncomp = length(raw$svd$d)

    ## Compute the partial factor scores
    nvar <- vapply(tables, ncol, FUN.VALUE = 0) # num of vars in each table
    positions <- c(0, cumsum(nvar))
    pfscores <- list()
    k <- length(tables)
    for (i in 1:k) {
        pfscores[[i]] <- k * 1 / raw$svs[i] *
            tables[[i]] %*%
            raw$svd$v[(positions[i] + 1):(positions[i + 1]), 1:ncomp]
    }

    ## Compute the factor loadings
    svec <- unlist(Map(function(sv, times) rep(sv, times), raw$svs, nvar))
    loadings <- diag(svec) %*% raw$svd$v[, 1:ncomp]
    
    return(list(eig = raw$svd$d[1:ncomp] ^ 2 / nrow(data),
                factor.scores = (raw$svd$u %*% diag(raw$svd$d))[, 1:ncomp],
                loadings = loadings,
                partial.factor.scores = pfscores
                ))
    
}
