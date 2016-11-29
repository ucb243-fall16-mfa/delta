#' @title Plot Factor Score
#' @param mfa an object of class "mfa"
#' @param group a factor vector, indicating which group each row
#'     belongs to
#' @param dim a integer vector indicating the dimensions to plot, the
#'     first element being x and second being y
#' @import ggplot2
#' @export
PlotFactorScore <- function(mfa, group = NULL, dim = c(1, 2)) {
    if (class(mfa) != 'mfa') stop('Object not of class mfa.')
    if (length(dim) != 2) stop('Plot only works with 2 dimensions.')

    fs <- data.frame(mfa$factor.scores[ , dim])
    if (!is.null(group)) fs <- cbind(fs, group)

    plot <- ggplot(data = fs) +
        labs(x = 'Dimension 1',
             y = 'Dimension 2',
             title = 'Factor scores') +
        coord_fixed(1) +
        geom_hline(yintercept = 0) +
        geom_vline(xintercept = 0)
        
    if (!is.null(group))
        plot <- plot + geom_point(aes(x = X1, y = X2, colour = group))
    else
        plot <- plot + geom_point(aes(x = X1, y = X2))
    plot
}
