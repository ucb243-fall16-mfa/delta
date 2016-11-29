#' @import ggplot2
PlotHelper <- function(data, group, xlab, ylab, title) {
    data <- data.frame(data)
    if (!is.null(group)) data <- cbind(data, group)

    plot <- ggplot(data = data) +
        labs(x = 'Dimension 1',
             y = 'Dimension 2',
             title = title) +
        coord_fixed(1) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)
        
    if (!is.null(group))
        plot <- plot + geom_point(aes(x = X1, y = X2, colour = group))
    else
        plot <- plot + geom_point(aes(x = X1, y = X2))
    plot
}

PlotRawFactorScores <- function(fs, group, title) {
    PlotHelper(fs, group,
               'Dimension 1', 'Dimension 2', title)
}

CheckArg <- function(mfa, dim) {
    if (class(mfa) != 'mfa') stop('Object not of class mfa.')
    if (length(dim) != 2) stop('Plot only works with 2 dimensions.')
}    

#' @title Plot Factor Score
#' @param mfa an object of class "mfa"
#' @param group a factor vector, indicating which group each row
#'     belongs to
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @export
PlotFactorScores <- function(mfa, group = NULL, dim = c(1, 2)) {
    CheckArg(mfa, dim)
    fs <- mfa$factor.scores[ , dim]
    PlotRawFactorScores(fs, group, 'Factor Scores')
}

#' @title Plot Partial Factor Scores
#' @param mfa an object of class "mfa"
#' @param k the partial factor score of which table
#' @param group a factor vector, indicating which group each row
#'     belongs to
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @export
PlotPartialFactorScores <- function(mfa, k,
                                   group = NULL, dim = c(1, 2)) {
    CheckArg(mfa, dim)
    fs <- mfa$partial.factor.scores[[k]][ , dim]
    title <- paste('Partial factor scores of table', k)
    PlotRawFactorScores(fs, group, title)
}

#' @title Plot Variable Loadings
#' @param mfa an object of class "mfa"
#' @param var.group a factor vector, indicating which group each row
#'     (variable) belongs to
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @export
PlotLoadings <- function(mfa, var.group = NULL, dim = c(1, 2)) {
    CheckArg(mfa, dim)
    ld <- mfa$loadings
    PlotHelper(ld, var.group,
               'Dimension 1', 'Dimension 2',
               'Variable Loadings')
}
