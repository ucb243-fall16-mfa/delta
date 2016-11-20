tr <- function(m) sum(diag(m))

#' @export
RV <- function(table1, table2) {
    table1 <- data.matrix(table1)
    table2 <- data.matrix(table2)
    tr(tcrossprod(table1) %*% tcrossprod(table2)) /
        sqrt(tr(tcrossprod(table1) %*% tcrossprod(table1)) *
             tr(tcrossprod(table2) %*% tcrossprod(table2)))
}

#' @export
RV_table <- function(data, sets) {
    tables <- SplitTable(data, sets)
    n <- length(sets)
    result <- matrix(, nrow = n, ncol = n)
    for (i in 1:n) {
        for (j in 1:n) {
            result[i, j] <- RV(tables[[i]], tables[[j]])
        }
    }
    result
}
