### Import the data needed for testing
wines <- read.csv('wines.csv')
wines <- wines[, 2:(ncol(wines) - 4)]   # Get rid of the last 4 columns
g <- c(6, 6, 6, 5, 6, 5, 4, 6, 5, 4)
sets <- list(1:6, 7:12, 13:18, 19:23, 24:29, 30:34, 35:38, 39:44, 45:49, 50:53)

### Create a list of matrices from the 'sets' variable
split.variables <- function(data, sets) {
    tables <- list()
    for (i in 1:length(sets)) {
        tables[[i]] <- data[, sets[[i]]]
    }

    if (!is.null(names(sets)))
        names(tables) <- names(sets)
    tables
}

### MFA on normalised tables
mfa.raw <- function(tables) {
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
    eigenvalues <- X.svd$d ^ 2
    comp.factor.scores <- X.svd$u %*% diag(X.svd$d)
    loadings <- X.svd$v

    list(eigenvalues = eigenvalues,
         factor.scores = comp.factor.scores,
         factor.loadings = loadings,
         svs = svs
         )
}

mfa <- function(data, sets, ncomp = NULL, center = TRUE, scale = TRUE) {
    data <- scale(data, center, scale) / sqrt(nrow(data) - 1)
    tables <- split.variables(data, sets)
    raw <- mfa.raw(tables)

    if (is.null(ncomp)) ncomp = ncol(raw$factor.loadings)
    nvar <- vapply(tables, ncol, FUN.VALUE = 0) # num of vars in each table
    positions <- c(0, cumsum(nvar))
    pfscores <- list()
    k <- length(tables)
    for (i in 1:k) {
        pfscores[[i]] <- k * 1 / raw$svs[i] *
            tables[[i]] %*%
            raw$factor.loadings[(positions[i] + 1):(positions[i + 1]), 1:ncomp]
    }
    
    return(list(eigenvalues = raw$eigenvalues[1:ncomp],
                factor.scores = raw$factor.scores[, 1:ncomp],
                factor.loadings = raw$factor.loadings[, 1:ncomp],
                partial.factor.scores = pfscores
                ))
    
}

